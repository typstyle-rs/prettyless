import hashlib
import io
import json
import tarfile
import unittest
from pathlib import Path
from unittest.mock import Mock, patch
from urllib.error import HTTPError

import release_state as state

COMMIT = "a" * 40
PACKAGE = "prettyless-0.4.0"
TAG = "v0.4.0"
NOTES = "- New API\n"


def crate_archive(*, commit=COMMIT, dirty=False, lock=b"lock", source=b"source", extra=None):
    files = {
        ".cargo_vcs_info.json": json.dumps(
            {"git": {"sha1": commit, "dirty": dirty}, "path_in_vcs": ""}
        ).encode(),
        "Cargo.toml": b'[package]\nname = "prettyless"\nversion = "0.4.0"\n',
        "Cargo.toml.orig": b"original manifest",
        "Cargo.lock": lock,
        "src/lib.rs": source,
    }
    files.update(extra or {})
    output = io.BytesIO()
    with tarfile.open(fileobj=output, mode="w:gz") as archive:
        for name, content in files.items():
            member = tarfile.TarInfo(f"{PACKAGE}/{name}")
            member.size = len(content)
            archive.addfile(member, io.BytesIO(content))
    return output.getvalue()


def github_release(**changes):
    release = {"tag_name": TAG, "name": TAG, "body": NOTES, "draft": False, "prerelease": False}
    return {**release, **changes}


class RegistryTests(unittest.TestCase):
    def check(self, remote, local=None, checksum=None):
        archive = Mock(spec=Path)
        archive.read_bytes.return_value = local or remote
        metadata = {"version": {"crate": "prettyless", "num": "0.4.0", "yanked": False, "checksum": checksum or hashlib.sha256(remote).hexdigest()}}

        def download(request, *, timeout):
            body = remote if request.get_header("Accept") == "application/octet-stream" else b'{"url": "https://registry.invalid/archive.crate"}'
            return io.BytesIO(body)

        with patch.object(state, "get_json", return_value=metadata), patch.object(state, "urlopen", side_effect=download):
            return state.crate_is_published("prettyless", "0.4.0", archive, COMMIT)

    def test_absent_version_requires_publication(self):
        with patch.object(state, "get_json", return_value=None), patch.object(state, "request") as download:
            self.assertFalse(state.crate_is_published("prettyless", "0.4.0", Path("unused"), COMMIT))
            download.assert_not_called()

    def test_matching_package_can_be_retried_with_a_new_lockfile(self):
        self.assertTrue(self.check(crate_archive(lock=b"old"), crate_archive(lock=b"new")))

    def test_checksum_mismatch_is_rejected(self):
        with self.assertRaisesRegex(ValueError, "checksum"):
            self.check(crate_archive(), checksum="b" * 64)

    def test_source_changes_and_missing_files_are_rejected(self):
        for remote in [crate_archive(source=b"different"), crate_archive(extra={"extra.rs": b"extra"}), crate_archive(extra={"Cargo.toml.orig": b"changed"})]:
            with self.subTest(remote=hashlib.sha256(remote).hexdigest()), self.assertRaisesRegex(ValueError, "contents differ"):
                self.check(remote, crate_archive())

    def test_wrong_commit_and_dirty_packages_are_rejected(self):
        for remote in [crate_archive(commit="b" * 40), crate_archive(dirty=True)]:
            with self.subTest(remote=hashlib.sha256(remote).hexdigest()), self.assertRaisesRegex(ValueError, "clean tagged commit"):
                self.check(remote)

    def test_generated_manifest_configuration_is_compared(self):
        reordered = b'# new Cargo version\n[package]\nversion = "0.4.0"\nname = "prettyless"\n'
        self.assertTrue(self.check(crate_archive(extra={"Cargo.toml": reordered}), crate_archive()))
        with self.assertRaisesRegex(ValueError, "contents differ"):
            self.check(crate_archive(extra={"Cargo.toml": reordered + b'edition = "2024"\n'}), crate_archive())

    def test_yanked_version_is_not_a_successful_release(self):
        with patch.object(state, "get_json", return_value={"version": {"crate": "prettyless", "num": "0.4.0", "yanked": True}}), self.assertRaisesRegex(ValueError, "yanked"):
            state.crate_is_published("prettyless", "0.4.0", Path("unused"), COMMIT)

    def test_invalid_archive_paths_are_rejected_without_extraction(self):
        with self.assertRaisesRegex(ValueError, "invalid path"):
            state.archive_sources(crate_archive(extra={"src/../../escape": b"bad"}), PACKAGE, COMMIT)

    def test_only_not_found_means_absent(self):
        for code in [404, 403]:
            error = HTTPError("https://registry.invalid", code, "test", {}, None)
            self.addCleanup(error.close)
            with self.subTest(code=code), patch.object(state, "request", side_effect=error):
                if code == 404:
                    self.assertIsNone(state.get_json("https://registry.invalid"))
                else:
                    with self.assertRaises(HTTPError):
                        state.get_json("https://registry.invalid")


class GitHubTests(unittest.TestCase):
    def ensure(self, responses, *, prerelease=False, tag=TAG, notes=NOTES):
        with patch.object(state, "get_json", side_effect=responses):
            return state.ensure_github_release("https://api.invalid", "owner/repo", tag, COMMIT, notes, prerelease, "token")

    def ref(self, *, commit=COMMIT, kind="commit"):
        return {"object": {"type": kind, "sha": commit}}

    def test_existing_matching_release_is_a_no_op(self):
        with patch.object(state, "request") as create:
            self.assertFalse(self.ensure([self.ref(), github_release()]))
            create.assert_not_called()

    def test_annotated_tags_are_resolved_before_creation(self):
        with patch.object(state, "request", return_value=json.dumps(github_release()).encode()) as create:
            self.assertTrue(self.ensure([self.ref(commit="tag-object", kind="tag"), self.ref(), None]))
            self.assertEqual(create.call_args.kwargs["payload"]["tag_name"], TAG)

    def test_missing_or_moved_tags_do_not_create_a_release(self):
        for ref in [None, self.ref(commit="b" * 40)]:
            with self.subTest(ref=ref), patch.object(state, "request") as create, self.assertRaises(ValueError):
                self.ensure([ref])
            create.assert_not_called()

    def test_existing_conflicting_releases_are_never_overwritten(self):
        for change in [{"tag_name": "v0.3.0"}, {"name": "edited"}, {"body": "edited notes"}, {"draft": True}, {"prerelease": True}]:
            with self.subTest(change=change), patch.object(state, "request") as create, self.assertRaisesRegex(ValueError, "inspect it"):
                self.ensure([self.ref(), github_release(**change)])
            create.assert_not_called()

    def test_prerelease_does_not_replace_latest(self):
        tag = "v0.4.0-rc.1"
        release = github_release(tag_name=tag, name=tag, prerelease=True)
        with patch.object(state, "request", return_value=json.dumps(release).encode()) as create:
            self.assertTrue(self.ensure([self.ref(), None], prerelease=True, tag=tag))
            self.assertEqual(create.call_args.kwargs["payload"]["make_latest"], "false")


if __name__ == "__main__":
    unittest.main()
