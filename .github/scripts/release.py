#!/usr/bin/env python3
"""Validate release metadata and extract notes; never publish or create tags."""

import argparse
import os
import re
from datetime import date
from pathlib import Path

import tomllib

SEMVER = re.compile(
    r"(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)"
    r"(?:-([0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?"
)


def release_notes(tag: str, manifest: str, changelog: str) -> tuple[str, bool]:
    """Require a canonical tag, matching version, and one dated, nonempty entry."""
    version = tomllib.loads(manifest)["package"]["version"]
    if not isinstance(version, str):
        raise TypeError("package.version must be a string")
    match = SEMVER.fullmatch(version)
    if not match or tag != f"v{version}":
        raise ValueError("tag must be v<package.version>, without build metadata")
    prerelease = match.group(4)
    if prerelease and any(
        part.isdigit() and len(part) > 1 and part.startswith("0")
        for part in prerelease.split(".")
    ):
        raise ValueError("numeric prerelease identifiers cannot have leading zeros")

    changelog = changelog.replace("\r\n", "\n")
    sections = list(re.finditer(r"^## (.+)$", changelog, re.MULTILINE))
    candidates = [
        (i, section)
        for i, section in enumerate(sections)
        if section.group(1).split(" ", 1)[0] == tag
    ]
    if len(candidates) != 1:
        raise ValueError(f"expected exactly one changelog section for {tag}")
    index, section = candidates[0]
    heading = re.fullmatch(r"v\S+ \((\d{4}-\d{2}-\d{2})\)", section.group(1))
    if not heading:
        raise ValueError("release heading must be '## v<version> (YYYY-MM-DD)'")
    date.fromisoformat(heading.group(1))
    end = sections[index + 1].start() if index + 1 < len(sections) else len(changelog)
    notes = changelog[section.end() : end].strip()
    if not any(re.match(r"^-\s+\S", line) for line in notes.splitlines()):
        raise ValueError("release notes must contain at least one changelog bullet")
    return notes + "\n", bool(prerelease)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("tag", help="existing or planned release tag, e.g. v0.4.0")
    parser.add_argument(
        "--notes", type=Path, required=True, help="output Markdown path"
    )
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    try:
        notes, prerelease = release_notes(
            args.tag,
            (root / "Cargo.toml").read_text(encoding="utf-8"),
            (root / "CHANGELOG.md").read_text(encoding="utf-8"),
        )
        args.notes.parent.mkdir(parents=True, exist_ok=True)
        args.notes.write_text(notes, encoding="utf-8")
    except (ValueError, TypeError, KeyError, OSError) as error:
        parser.error(str(error))
    if output := os.environ.get("GITHUB_OUTPUT"):
        with open(output, "a", encoding="utf-8") as file:
            file.write(f"prerelease={str(prerelease).lower()}\n")
    print(f"Validated {args.tag}; notes written to {args.notes}")


if __name__ == "__main__":
    main()
