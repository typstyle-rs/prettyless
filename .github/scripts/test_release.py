import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

from release import release_notes


class ReleaseTests(unittest.TestCase):
    def notes(self, version="0.4.0", tag="v0.4.0", changelog=None):
        return release_notes(
            tag,
            f'[package]\nversion = "{version}"\n',
            changelog
            or "## Unreleased\n\n## v0.4.0 (2026-10-03)\n\n### Added\n\n- New API\n\n## v0.3.0 (2025-07-17)\n\n- Old API\n",
        )

    def test_extract_only_selected_release(self):
        self.assertEqual(self.notes(), ("### Added\n\n- New API\n", False))

    def test_windows_line_endings(self):
        self.assertEqual(
            self.notes(changelog="## v0.4.0 (2026-10-03)\r\n- New API\r\n"),
            ("- New API\n", False),
        )

    def test_non_string_version(self):
        with self.assertRaisesRegex(TypeError, "must be a string"):
            release_notes("v0.4.0", "[package]\nversion = 4\n", "")

    def test_prerelease(self):
        self.assertEqual(
            self.notes(
                "0.4.0-rc.1", "v0.4.0-rc.1", "## v0.4.0-rc.1 (2026-10-03)\n- Preview\n"
            ),
            ("- Preview\n", True),
        )

    def test_invalid_or_mismatched_versions(self):
        for version, tag in [
            ("0.4.0", "v0.3.0"),
            ("0.4.0", "0.4.0"),
            ("0.04.0", "v0.04.0"),
            ("0.4.0-rc.01", "v0.4.0-rc.01"),
            ("0.4.0+build", "v0.4.0+build"),
            ("0.4.0", "v0.4.0\nprerelease=true"),
        ]:
            with self.subTest(version=version, tag=tag), self.assertRaises(ValueError):
                self.notes(version, tag)

    def test_missing_duplicate_empty_or_invalid_dated_notes(self):
        for changelog in [
            "## Unreleased\n- Future\n",
            "## v0.4.0 (2026-10-03)\n- A\n## v0.4.0 (2026-10-03)\n- B\n",
            "## v0.4.0 (2026-10-03)\n### Added\n",
            "## v0.4.0 (2026-10-03)\n- \n### Placeholder\n",
            "## v0.4.0\n- A\n",
            "## v0.4.0 (2026-02-30)\n- A\n",
        ]:
            with self.subTest(changelog=changelog), self.assertRaises(ValueError):
                self.notes(changelog=changelog)

    def test_cli_creates_output_directory_only_after_validation(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            script = root / ".github" / "scripts" / "release.py"
            script.parent.mkdir(parents=True)
            shutil.copyfile(Path(__file__).with_name("release.py"), script)
            (root / "Cargo.toml").write_text(
                '[package]\nversion = "0.4.0"\n', encoding="utf-8"
            )
            (root / "CHANGELOG.md").write_text(
                "## v0.4.0 (2026-10-03)\n- New API\n", encoding="utf-8"
            )
            notes = root / "target" / "release-notes.md"
            output = root / "outputs"
            env = {**os.environ, "GITHUB_OUTPUT": str(output)}
            command = [sys.executable, str(script), "v0.3.0", "--notes", str(notes)]
            invalid = subprocess.run(
                command, check=False, capture_output=True, text=True, env=env
            )
            self.assertEqual(invalid.returncode, 2)
            self.assertNotIn("Traceback", invalid.stderr)
            self.assertFalse(notes.parent.exists())
            self.assertFalse(output.exists())
            command[2] = "v0.4.0"
            valid = subprocess.run(
                command, check=False, capture_output=True, text=True, env=env
            )
            self.assertEqual(valid.returncode, 0, valid.stderr)
            self.assertEqual(notes.read_text(encoding="utf-8"), "- New API\n")
            self.assertEqual(output.read_text(encoding="utf-8"), "prerelease=false\n")


if __name__ == "__main__":
    unittest.main()
