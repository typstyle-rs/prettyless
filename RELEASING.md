# Releasing prettyless

Pushing a `v<version>` tag to `typstyle-rs/prettyless` runs validation and tests,
publishes to crates.io, and creates a GitHub Release from the changelog.

## One-time setup

Install `.github/workflows/release.yml` on `main` and complete this setup before
tagging:

- **GitHub environment:** In **Settings → Environments**, create `crates-io`.
  Choose **Selected branches and tags**, allow `v*` with ref type **Tag**, and
  allow no branches. Leave required reviewers and wait timers unset for automatic
  publishing.
- **Trusted Publisher:** A crate owner opens [prettyless settings](https://crates.io/crates/prettyless/settings)
  and selects **Trusted Publishing → Add → GitHub**. Set owner `typstyle-rs`,
  repository `prettyless`, workflow `release.yml`, and environment `crates-io`.
  This uses short-lived credentials; no API-token secret is needed.
- **Protect `main`:** In **Settings → Rules → Rulesets**, add an active branch
  ruleset requiring pull requests, one approval from another maintainer, dismissal
  of stale approvals, resolved review conversations, and passing `check` and
  `test` checks on an up-to-date branch. Block force pushes and deletion; allow
  no bypass actors.
- **Protect `v*` tags:** Use two active tag rulesets: restrict **creation** with
  **Maintain** and **Admin** bypass roles; restrict **updates and deletion** in a
  separate ruleset with **no bypass actors**.

## Prepare and release

1. Bump the version in `Cargo.toml` and add `## v<version> (YYYY-MM-DD)` with at
   least one changelog bullet to `CHANGELOG.md`. Keep `## Unreleased` and leave
   `Cargo.lock` untracked.
2. Check locally with Python 3.11+; replace `v0.4.0` with the planned tag:

   ```sh
   python .github/scripts/release.py v0.4.0 --notes target/release-notes.md
   cargo publish --dry-run --all-features --registry crates-io
   ```

3. Merge to `main` and wait for CI. Create an annotated `v<version>` tag on the
   merged release commit, push it to `origin`, and monitor the Release workflow
   until both jobs succeed.

The tag must match the manifest version. Prereleases such as `v0.5.0-rc.1` are
supported; build metadata (`+...`) is rejected.

## Failure recovery

- For transient or configuration failures, use **Re-run failed jobs**. Rerunning
  all jobs is also safe: existing artifacts are verified before being skipped.
- Existing crates must be unyanked, pass registry checksum verification, and
  match the clean tagged commit, manifests, and source files. Lockfile changes and generated-manifest
  comments/order are allowed. Existing GitHub Releases must be published and
  match the tag's commit, title, prerelease status, and changelog notes.
  Conflicts stop the workflow and require inspection.
- Never move or delete a release tag. Source or workflow fixes need a new
  version/tag because reruns use the original tagged source and workflow.
