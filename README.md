# gh-post-range-diff

One of the unfortunate problems with force-pushing to GitHub is that it's really hard for your reviewers to see _what_ has changed in a Pull Request.

Sure, there is a "Compare" button on the _force push_ event...

![Example of a force push event on GitHub with a Compare button](docs/force-push-compare.png)

... but if you've rebased the Pull Request on its base branch, then all changes in the base branch will _also_ show up there, which makes it really hard to understand what has changed in the Pull Request's _own_ commits. And even _if_ the base branch is completely unchanged, you will be unable to see _which_ commits have changed, and how. All you'll see is one big diff.

This little program solves that by posting a pretty-printed version of the result of [`git range-diff`](https://git-scm.com/docs/git-range-diff) in your Pull Requests every time that they are pushed to.

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
      - uses: svenvanheugten/gh-post-range-diff@v0.4.2
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
