# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- The GitHub Action now verifies that the release is immutable before installing it (https://github.com/svenvanheugten/gh-post-range-diff/pull/25)

### Fixed

- Fix the GitHub Action not working when it is pinned by SHA digest (https://github.com/svenvanheugten/gh-post-range-diff/pull/25)
- Fix incorrect parsing of ranges of ten or more commits (https://github.com/svenvanheugten/gh-post-range-diff/pull/38)
- Fix consecutive spaces being collapsed to a single space in commit messages (https://github.com/svenvanheugten/gh-post-range-diff/pull/39)
- Fix commits of the base branch being reported as commits of the pull request when the base branch was force-pushed and then advanced twice by an ordinary push (https://github.com/svenvanheugten/gh-post-range-diff/pull/56)
- Fix commits of the base branch being reported as commits of the pull request when the base branch was force-pushed back onto a commit it already held (https://github.com/svenvanheugten/gh-post-range-diff/pull/58)
- Fix a commit of the base branch being reported as removed from the pull request when the base branch was rewound off that commit and later advanced back onto it (https://github.com/svenvanheugten/gh-post-range-diff/pull/60)
- Fix the Nix package failing to evaluate when it is used as a `gh` extension via home-manager's `programs.gh.extensions` (https://github.com/svenvanheugten/gh-post-range-diff/pull/63)
