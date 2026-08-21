-- | The pull request the tool reports on, behind a handle, so that it can
-- be mocked in the tests. The one implementation that talks to the real
-- thing is 'gh', with the `gh` invocations and timeline parsing it needs.
module GhPostRangeDiff.GitHub
  ( Ev (..),
    Handle (..),
    gh,
  )
where

import Data.List.Extra (trim)
import GhPostRangeDiff.Git (CommitSha, commitSha, sh)
import System.Process (callProcess)

-- One timeline event: type, before-oid, after-oid. GitHub reports both oids in
-- full, so they read straight back as shas.
data Ev = Ev
  { evType :: String,
    evBefore, evAfter :: CommitSha
  }

-- | Everything the tool does with one pull request.
data Handle = Handle
  { -- | the force-push timeline events, oldest first
    timeline :: IO [Ev],
    -- | the branch the pull request is targeting
    baseRef :: IO String,
    -- | every comment body on it, concatenated
    comments :: IO String,
    postComment :: String -> IO ()
  }

-- | The real pull request numbered @pr@, as the `gh` CLI reaches it.
gh :: String -> Handle
gh pr =
  Handle
    { timeline = ghTimeline pr,
      baseRef = ghBaseRef pr,
      comments = ghComments pr,
      postComment = ghPostComment pr
    }

parse :: String -> Ev
parse l = case words l of
  (t : b : c : _)
    | Just before <- commitSha b,
      Just after <- commitSha c ->
        Ev t before after
  _ -> error ("unexpected timeline line: " ++ l)

query :: String
query =
  unlines
    [ "query($o: String!, $r: String!, $n: Int!) {",
      "  repository(owner: $o, name: $r) {",
      "    pullRequest(number: $n) {",
      "      timelineItems(",
      "        last: 250",
      "        itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT, BASE_REF_FORCE_PUSHED_EVENT]",
      "      ) {",
      "        nodes {",
      "          __typename",
      "          ... on HeadRefForcePushedEvent { beforeCommit { oid } afterCommit { oid } }",
      "          ... on BaseRefForcePushedEvent { beforeCommit { oid } afterCommit { oid } }",
      "        }",
      "      }",
      "    }",
      "  }",
      "}"
    ]

-- The repository the `gh` CLI is pointed at, as (owner, name).
ownerRepo :: IO (String, String)
ownerRepo = do
  nameWithOwner <- trim <$> sh "gh" ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
  case break (== '/') nameWithOwner of
    (owner, _ : repo) -> pure (owner, repo)
    _ -> error ("unexpected repository name: " ++ nameWithOwner)

-- The PR's force-push timeline events, oldest first.
ghTimeline :: String -> IO [Ev]
ghTimeline pr = do
  (owner, repo) <- ownerRepo
  raw <-
    sh
      "gh"
      [ "api",
        "graphql",
        "-f",
        "query=" ++ query,
        "-F",
        "o=" ++ owner,
        "-F",
        "r=" ++ repo,
        "-F",
        "n=" ++ pr,
        "--jq",
        ".data.repository.pullRequest.timelineItems.nodes[]"
          ++ " | \"\\(.__typename) \\(.beforeCommit.oid) \\(.afterCommit.oid)\""
      ]
  pure (map parse (lines raw))

-- The branch the PR is targeting.
ghBaseRef :: String -> IO String
ghBaseRef pr = trim <$> sh "gh" ["pr", "view", pr, "--json", "baseRefName", "-q", ".baseRefName"]

-- Every comment body on the PR, concatenated.
ghComments :: String -> IO String
ghComments pr = sh "gh" ["pr", "view", pr, "--json", "comments", "-q", ".comments[].body"]

ghPostComment :: String -> String -> IO ()
ghPostComment pr body = callProcess "gh" ["pr", "comment", pr, "--body", body]
