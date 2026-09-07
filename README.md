# gh-post-range-diff

One of the unfortunate problems with force-pushing to GitHub is that it's really hard for your reviewers to see _what_ has changed in a Pull Request.

Sure, there is a "Compare" button on the _force push_ event...

![Example of a force push event on GitHub with a Compare button](docs/force-push-compare.png)

... but if you've rebased the Pull Request on its base branch, then all changes in the base branch will _also_ show up there, which makes it really hard to understand what has changed in the Pull Request's _own_ commits. And even _if_ the base branch is completely unchanged, you will be unable to see _which_ commits have changed, and how. All you'll see is one big diff.

This little program solves that by posting a pretty-printed version of the result of [`git range-diff`](https://git-scm.com/docs/git-range-diff) in your Pull Requests every time that they are pushed to. It takes care of the nitty-gritty involved in figuring out the correct commit ranges to compare, even when the base branch was updated simultaneously.

## Supported base branch movements

The tool can unambiguously figure out the old and new base ref, as long as the base branch movements fall into one of these categories, which should cover _most_ common workflows:

- A base branch that nobody _ever_ force-pushes to, and which only grows through ordinary pushes, e.g. `main`. Your PR branch is allowed to lag behind it, and doesn't have to be based on the latest version.
- A base branch that receives _any_ force pushes. Your PR branch _always_ needs to be based on the latest version. One way to guarantee that is to use [`--update-refs`](https://andrewlock.net/working-with-stacked-branches-in-git-is-easier-with-update-refs/), and to always push the whole stack simultaneously.

Both of these categories are extensively covered by tens of thousands of [property-based tests](https://en.wikipedia.org/wiki/Software_testing#Property_testing). Other base branch movements aren't supported, and might lead to hard-to-read reports.

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
      - uses: svenvanheugten/gh-post-range-diff@v0.5.1
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
