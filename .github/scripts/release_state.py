#!/usr/bin/env python3
"""Recover a release only when the existing artifacts match the tagged source."""

import argparse
import hashlib
import io
import json
import os
import re
import tarfile
import time
import tomllib
from pathlib import Path
from urllib.error import HTTPError
from urllib.parse import quote
from urllib.request import Request, urlopen

USER_AGENT = "prettyless-release-workflow (https://github.com/typstyle-rs/prettyless)"
MAX_DOWNLOAD = 32 * 1024 * 1024
MAX_UNPACKED = 128 * 1024 * 1024


def request(url, *, token=None, payload=None, accept="application/json"):
    headers = {"User-Agent": USER_AGENT, "Accept": accept}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    data = None
    if payload is not None:
        headers["Content-Type"] = "application/json"
        data = json.dumps(payload).encode()
    with urlopen(Request(url, data=data, headers=headers), timeout=30) as response:
        body = response.read(MAX_DOWNLOAD + 1)
    if len(body) > MAX_DOWNLOAD:
        raise ValueError("release artifact exceeds the download limit")
    return body


def get_json(url, *, token=None):
    try:
        return json.loads(request(url, token=token))
    except HTTPError as error:
        if error.code == 404:
            error.close()
            return None
        raise


def archive_sources(data, package, commit):
    files = {}
    total = 0
    prefix = package + "/"
    with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as archive:
        for member in archive:
            if member.isdir():
                continue
            if not member.isfile() or not member.name.startswith(prefix):
                raise ValueError("crate archive contains an unexpected entry")
            name = member.name[len(prefix) :]
            if not name or any(part in ("", ".", "..") for part in name.split("/")):
                raise ValueError("crate archive contains an invalid path")
            if name in files:
                raise ValueError("crate archive contains duplicate paths")
            total += member.size
            if total > MAX_UNPACKED:
                raise ValueError("crate archive exceeds the unpacked size limit")
            files[name] = archive.extractfile(member).read()
    vcs = json.loads(files.pop(".cargo_vcs_info.json"))
    if (
        vcs["git"]["sha1"] != commit
        or vcs["git"].get("dirty", False)
        or vcs["path_in_vcs"] != ""
    ):
        raise ValueError("crate was not packaged from the clean tagged commit")
    # Cargo regenerates the manifest's comments and ordering. Its effective
    # configuration must still match; only the dependency lockfile may change.
    manifest = tomllib.loads(files["Cargo.toml"].decode("utf-8"))
    files["Cargo.toml"] = json.dumps(manifest, sort_keys=True).encode()
    files.pop("Cargo.lock", None)
    if "Cargo.toml.orig" not in files:
        raise ValueError("crate archive is missing the original manifest")
    return files


def crate_is_published(name, version, archive, commit):
    url = f"https://crates.io/api/v1/crates/{quote(name, safe='')}/{quote(version, safe='')}"
    metadata = get_json(url)
    if metadata is None:
        return False
    published = metadata["version"]
    if published["crate"] != name or published["num"] != version:
        raise ValueError("registry returned a different crate version")
    if published["yanked"]:
        raise ValueError("published crate version is yanked; inspect it before retrying")
    checksum = published["checksum"]
    if not isinstance(checksum, str) or not re.fullmatch(r"[0-9a-f]{64}", checksum):
        raise ValueError("registry returned an invalid crate checksum")
    remote = request(url + "/download", accept="application/octet-stream")
    if hashlib.sha256(remote).hexdigest() != checksum:
        raise ValueError("published crate does not match its registry checksum")
    package = f"{name}-{version}"
    if archive_sources(remote, package, commit) != archive_sources(
        archive.read_bytes(), package, commit
    ):
        raise ValueError("published crate contents differ from the tagged package")
    return True


def verify_remote_tag(api, repository, tag, commit, token):
    ref = get_json(f"{api}/repos/{repository}/git/ref/tags/{quote(tag, safe='')}", token=token)
    if ref is None:
        raise ValueError("release tag does not exist on GitHub")
    target = ref["object"]
    visited = set()
    while target["type"] == "tag":
        sha = target["sha"]
        if sha in visited:
            raise ValueError("release tag contains a cycle")
        visited.add(sha)
        annotated = get_json(f"{api}/repos/{repository}/git/tags/{sha}", token=token)
        if annotated is None:
            raise ValueError("annotated release tag does not exist")
        target = annotated["object"]
    if target["type"] != "commit" or target["sha"] != commit:
        raise ValueError("GitHub release tag differs from the tested commit")


def verify_github_release(release, tag, notes, prerelease):
    if (
        release["tag_name"] != tag
        or release["name"] != tag
        or release["draft"]
        or release["prerelease"] != prerelease
        or (release["body"] or "").strip() != notes.strip()
    ):
        raise ValueError("existing GitHub Release differs; inspect it before retrying")


def ensure_github_release(api, repository, tag, commit, notes, prerelease, token):
    verify_remote_tag(api, repository, tag, commit, token)
    url = f"{api}/repos/{repository}/releases"
    existing = get_json(f"{url}/tags/{quote(tag, safe='')}", token=token)
    if existing is not None:
        verify_github_release(existing, tag, notes, prerelease)
        return False
    payload = {
        "tag_name": tag,
        "name": tag,
        "body": notes,
        "draft": False,
        "prerelease": prerelease,
    }
    if prerelease:
        payload["make_latest"] = "false"
    created = json.loads(request(url, token=token, payload=payload))
    verify_github_release(created, tag, notes, prerelease)
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    crate = commands.add_parser("crate", help="check the published package before a retry")
    crate.add_argument("--archive", type=Path)
    crate.add_argument("--require-published", action="store_true")
    github = commands.add_parser("github", help="create or verify the GitHub Release")
    github.add_argument("--tag", required=True)
    github.add_argument("--notes", type=Path, required=True)
    for command in (crate, github):
        command.add_argument("--commit", required=True)
    args = parser.parse_args()
    try:
        if not re.fullmatch(r"(?:[0-9a-f]{40}|[0-9a-f]{64})", args.commit):
            raise ValueError("expected a full release commit ID")
        root = Path(__file__).resolve().parents[2]
        package = tomllib.loads((root / "Cargo.toml").read_text(encoding="utf-8"))["package"]
        name, version = package["name"], package["version"]
        if args.command == "crate":
            archive = args.archive or root / "target" / "package" / f"{name}-{version}.crate"
            attempts = 6 if args.require_published else 1
            for attempt in range(attempts):
                published = crate_is_published(name, version, archive, args.commit)
                if published or attempt == attempts - 1:
                    break
                time.sleep(5)
            if args.require_published and not published:
                raise ValueError("publish failed and the version is absent from crates.io")
            if output := os.environ.get("GITHUB_OUTPUT"):
                with open(output, "a", encoding="utf-8") as file:
                    file.write(f"published={str(published).lower()}\n")
            print("Verified existing crate." if published else "Crate needs publishing.")
        else:
            if args.tag != f"v{version}":
                raise ValueError("release tag does not match the package version")
            created = ensure_github_release(
                os.environ.get("GITHUB_API_URL", "https://api.github.com"),
                os.environ["GH_REPO"],
                args.tag,
                args.commit,
                args.notes.read_text(encoding="utf-8"),
                "-" in version,
                os.environ["GH_TOKEN"],
            )
            print("Created GitHub Release." if created else "Verified existing GitHub Release.")
    except (ValueError, KeyError, TypeError, OSError, tarfile.TarError) as error:
        parser.error(str(error))


if __name__ == "__main__":
    main()
