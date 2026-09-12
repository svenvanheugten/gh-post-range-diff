# gh-post-range-diff

One of the unfortunate problems with force-pushing to GitHub is that it's really hard for your reviewers to see _what_ has changed in a Pull Request.

Sure, there is a "Compare" button on the _force push_ event...

![Example of a force push event on GitHub with a Compare button](docs/force-push-compare.png)

... but if you've rebased the Pull Request on its base branch, then all changes in the base branch will _also_ show up there, which makes it really hard to understand what has changed in the Pull Request's _own_ commits. And even _if_ the base branch is completely unchanged, you will be unable to see _which_ commits have changed, and how. All you'll see is one big diff.

This little program solves that by posting a pretty-printed version of [`git range-diff`](https://git-scm.com/docs/git-range-diff) in your Pull Requests on every push:

> ### Range-diff for push `3ce33dc` → `806d19e`
>
> ⚪ **Unchanged** `df10bd1` Add the widget
>
> <details>
> <summary>🟠 <strong>Updated</strong> <code>869404b</code> Add a CLI for the widget</summary>
>
> ```diff
> @@ src/cli.py (new)
>  +import argparse
>  +
>  +from widget import Widget
>  +
>  +def main():
>  +    parser = argparse.ArgumentParser()
> -+    parser.add_argument("--timeout", type=int, default=30)
> ++    parser.add_argument("--timeout", type=int, default=5)
>  +    args = parser.parse_args()
>  +    Widget("cli", args.timeout).run()
>  +
> ```
>
> </details>
>
> 🔴 **Removed** `3ce33dc` Add a benchmark script
>
> 🟢 **Added** `806d19e` Test the widget

Long lines are wrapped to fit the width of a PR comment, carrying on below under the same diff markers after a ↪. In the real comment, GitHub turns each sha into a link to that commit.

## Sharper reports for [jj](https://jj-vcs.github.io/jj/) repositories

`git range-diff` has to _guess_ which commit of the old version of your branch is which commit of the new one, by comparing how alike their diffs are, and it can guess wrong — reporting one rewritten commit as a commit dropped and an unrelated commit added.

Commits made with [jj](https://jj-vcs.github.io/jj/) carry a change ID that survives every rewrite, so there is nothing to guess. Where every commit in both versions of the branch carries one, and `jj` is on the `PATH`, the report is worked out with `jj interdiff` instead: commits are matched by change ID, and each rewrite is shown as a diff of the commits themselves rather than of their formatted patches. Everywhere else, `git range-diff` is used exactly as before.

The checkout itself doesn't have to be a jj repository: where it isn't one, the tool makes it one in place, which costs a throwaway CI checkout nothing. A repository you work in with jj yourself is left exactly as it is — nothing is fetched into it and nothing is written to it, because it already holds both versions of the branch and every commit the force-push left behind. The GitHub Action installs `jj` for you, and the Nix package brings it along, so this happens by itself.

## Supported base branch movements

To show only the changes to your own commits, the tool needs to know where the old and the new version of your branch forked off the base branch.

It can always work that out in these two cases, which should cover _most_ common use cases:

- _The base branch only ever grows_: Nobody force-pushes to it, which is usually true for PR's that target a protected branch like `main`. Your branch can fork off any commit on it, so you don't have to rebase onto the latest version before you push.
- _Your branch is always on the latest version of the base branch_: In this case, the base branch is free to be force-pushed. This is the usual case for stacked PRs, where your PR targets another PR's branch. The easiest way to stay on the latest version is to rebase the whole stack with [`--update-refs`](https://andrewlock.net/working-with-stacked-branches-in-git-is-easier-with-update-refs/) and push all the branches at once, and to never push up just one of them.

Both cases are covered by tens of thousands of [property-based tests](https://en.wikipedia.org/wiki/Software_testing#Property_testing). Anything outside them isn't supported. In particular, once the base branch has been force-pushed at any point while your PR is open, every push to your branch has to be on the latest version of the base branch, or the report will include commits from the base branch.

## GitHub Actions

The recommended way to use this is as a workflow that comments on every push to a Pull Request. Add `.github/workflows/range-diff.yml`:

```yaml
name: range-diff

on:
  pull_request:
    types: [synchronize]

permissions:
  contents: read
  pull-requests: write

jobs:
  range-diff:
    runs-on: ubuntu-latest
    steps:
      - uses: svenvanheugten/gh-post-range-diff@v0.6.0
```

## Manual use

To report on the most recent force-push to a Pull Request yourself, use it as a [`gh` CLI extension](https://docs.github.com/en/github-cli/github-cli/using-github-cli-extensions):

```
gh extension install svenvanheugten/gh-post-range-diff
gh post-range-diff <pr number>
```

Or with Nix:

```
nix run github:svenvanheugten/gh-post-range-diff -- <pr number>
```

Or with [home-manager](https://nix-community.github.io/home-manager/):

```nix
# flake.nix inputs:
#   gh-post-range-diff.url = "github:svenvanheugten/gh-post-range-diff";

programs.gh.extensions = [ inputs.gh-post-range-diff.packages.${pkgs.system}.default ];
```

Then run `gh post-range-diff <pr number>`.
