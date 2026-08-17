-- | The `gh` invocations the tool needs, and the timeline parsing on top of them.
module GhPostRangeDiff.GitHub (
    Ev (..),
    timeline,
    baseRef,
    comments,
    postComment,
) where

import Data.List.Extra (trim)
import GhPostRangeDiff.Git (sh)
import System.Process (callProcess)

-- One timeline event: type, before-oid, after-oid.
data Ev = Ev {evType, evBefore, evAfter :: String}

parse :: String -> Ev
parse l = case words l of
    (t : b : c : _) -> Ev t b c
    _ -> error ("unexpected timeline line: " ++ l)

query :: String
query =
    unlines
        [ "query($o: String!, $r: String!, $n: Int!) {"
        , "  repository(owner: $o, name: $r) {"
        , "    pullRequest(number: $n) {"
        , "      timelineItems("
        , "        last: 250"
        , "        itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT, BASE_REF_FORCE_PUSHED_EVENT]"
        , "      ) {"
        , "        nodes {"
        , "          __typename"
        , "          ... on HeadRefForcePushedEvent { beforeCommit { oid } afterCommit { oid } }"
        , "          ... on BaseRefForcePushedEvent { beforeCommit { oid } afterCommit { oid } }"
        , "        }"
        , "      }"
        , "    }"
        , "  }"
        , "}"
        ]

-- The repository the `gh` CLI is pointed at, as (owner, name).
ownerRepo :: IO (String, String)
ownerRepo = do
    nameWithOwner <- trim <$> sh "gh" ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
    case break (== '/') nameWithOwner of
        (owner, _ : repo) -> pure (owner, repo)
        _ -> error ("unexpected repository name: " ++ nameWithOwner)

-- The PR's force-push timeline events, oldest first.
timeline :: String -> IO [Ev]
timeline pr = do
    (owner, repo) <- ownerRepo
    raw <-
        sh
            "gh"
            [ "api"
            , "graphql"
            , "-f"
            , "query=" ++ query
            , "-F"
            , "o=" ++ owner
            , "-F"
            , "r=" ++ repo
            , "-F"
            , "n=" ++ pr
            , "--jq"
            , ".data.repository.pullRequest.timelineItems.nodes[]"
                ++ " | \"\\(.__typename) \\(.beforeCommit.oid) \\(.afterCommit.oid)\""
            ]
    pure (map parse (lines raw))

-- The branch the PR is targeting.
baseRef :: String -> IO String
baseRef pr = trim <$> sh "gh" ["pr", "view", pr, "--json", "baseRefName", "-q", ".baseRefName"]

-- Every comment body on the PR, concatenated.
comments :: String -> IO String
comments pr = sh "gh" ["pr", "view", pr, "--json", "comments", "-q", ".comments[].body"]

postComment :: String -> String -> IO ()
postComment pr body = callProcess "gh" ["pr", "comment", pr, "--body", body]
