{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List (isInfixOf, nub, unsnoc)
import Data.List.Extra (trim)
import Git (baseFor, sh, unshallowRange)
import RangeDiffRenderer (format)
import System.Environment (getArgs)
import System.Process (callProcess)

-- One timeline event: type, before-oid, after-oid.
data Ev = Ev {evType, evBefore, evAfter :: String}

parse :: String -> Ev
parse l = case words l of
    (t : b : c : _) -> Ev t b c
    _ -> error ("unexpected timeline line: " ++ l)

query :: String
query =
    """
    query($o: String!, $r: String!, $n: Int!) {
      repository(owner: $o, name: $r) {
        pullRequest(number: $n) {
          timelineItems(
            last: 250
            itemTypes: [HEAD_REF_FORCE_PUSHED_EVENT, BASE_REF_FORCE_PUSHED_EVENT]
          ) {
            nodes {
              __typename
              ... on HeadRefForcePushedEvent { beforeCommit { oid } afterCommit { oid } }
              ... on BaseRefForcePushedEvent { beforeCommit { oid } afterCommit { oid } }
            }
          }
        }
      }
    }
    """

-- The repository the `gh` CLI is pointed at, as (owner, name).
ownerRepo :: IO (String, String)
ownerRepo = do
    nameWithOwner <- trim <$> sh "gh" ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
    let (owner, _ : repo) = break (== '/') nameWithOwner
    pure (owner, repo)

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

-- Report on the push oldHead..newHead: post its range-diff as a PR comment.
run :: String -> String -> String -> IO ()
run pr oldHead newHead = do
    base <- trim <$> sh "gh" ["pr", "view", pr, "--json", "baseRefName", "-q", ".baseRefName"]
    -- Every recorded base tip, in chronological order, for base reconstruction.
    baseOids <-
        nub . concatMap (\e -> [evBefore e, evAfter e]) . filter ((== "BaseRefForcePushedEvent") . evType)
            <$> timeline pr

    -- Hidden marker identifying this exact push (before..after). Lets us run the
    -- program multiple times without duplicating comments.
    let marker = "<!-- gh-post-range-diff " ++ oldHead ++ ".." ++ newHead ++ " -->"

    posted <- sh "gh" ["pr", "view", pr, "--json", "comments", "-q", ".comments[].body"]
    if marker `isInfixOf` posted
        then
            putStrLn ("Already reported on " ++ take 7 oldHead ++ ".." ++ take 7 newHead ++ ". Nothing to do.")
        else do
            -- Fetch current base tip, both heads, and every historical base oid.
            --
            -- refs/rd/base is a scratch ref holding the fetched base tip. We need it
            -- because without a destination the tip only lands in FETCH_HEAD, which
            -- this same fetch also fills with the heads and every base oid, so we
            -- couldn't pick the base tip back out to rev-parse on the next line.
            callProcess "git" $ ["fetch", "--quiet", "origin", base ++ ":refs/rd/base", oldHead, newHead] ++ baseOids
            unshallowRange base oldHead newHead baseOids
            newBaseTip <- trim <$> sh "git" ["rev-parse", "refs/rd/base"]
            let cands = baseOids ++ [newBaseTip] -- current tip is newest, so it goes last
            b1 <- baseFor newBaseTip oldHead cands
            b2 <- baseFor newBaseTip newHead cands
            diff <- sh "git" ["range-diff", b1 ++ ".." ++ oldHead, b2 ++ ".." ++ newHead]

            let header = "### Range-diff for push " ++ take 7 oldHead ++ " → " ++ take 7 newHead
            callProcess
                "gh"
                [ "pr"
                , "comment"
                , pr
                , "--body"
                , marker ++ "\n" ++ header ++ "\n\n" ++ format diff
                ]

-- Manual use: no SHAs on the command line, so derive them from the most recent
-- force-push in the timeline and hand off to `run`.
manual :: String -> IO ()
manual pr = do
    heads <- filter ((== "HeadRefForcePushedEvent") . evType) <$> timeline pr
    case unsnoc heads of
        Nothing -> putStrLn "No force-push events on this PR. Nothing to diff."
        Just (_, h) -> run pr (evBefore h) (evAfter h)

main :: IO ()
main = do
    -- Either just a PR number (manual use: report the most recent force-push),
    -- or a PR number plus the push's before/after SHAs from the pull_request
    -- payload (CI use: report exactly that push, whatever kind it is).
    args <- getArgs
    case args of
        [pr] -> manual pr
        [pr, before, after]
            | not (null before) && not (null after) -> run pr before after
            | otherwise -> manual pr
        _ -> error "usage: gh-post-range-diff <pr> [<before-sha> <after-sha>]"
