# gh-post-range-diff

One of the unfortunate problems with force-pushing to GitHub is that it's really hard for your reviewers to see _what_ has changed in a Pull Request.

Sure, there is a "Compare" button on the _force push_ event...

![Example of a force push event on GitHub with a Compare button](docs/force-push-compare.png)

... but if you've rebased the Pull Request on its base branch, then all changes in the base branch will _also_ show up there, which makes it really hard to understand what has changed in the Pull Request's _own_ commits. And even _if_ the base branch is completely unchanged, you will be unable to see _which_ commits have changed, and how. All you'll see is one big diff.

This little program solves that by posting the result of [`git range-diff`](https://git-scm.com/docs/git-range-diff) in your Pull Requests every time that someone force-pushes to them.

## Installation and Usage

As a [`gh` CLI extension](https://docs.github.com/en/github-cli/github-cli/using-github-cli-extensions):

```
gh extension install svenvanheugten/gh-post-range-diff
gh post-range-diff <pr number>
```

With Nix:

```
nix run github:svenvanheugten/gh-post-range-diff -- <pr number>
```
