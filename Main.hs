{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Extra (findM)
import Data.List (isInfixOf, nub, unsnoc)
import Data.List.Extra (trim)
import RangeDiffRenderer (format)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.Process (callProcess, readProcess, readProcessWithExitCode)

sh :: String -> [String] -> IO String
sh cmd args = readProcess cmd args ""

-- One timeline event: type, ISO-8601 createdAt, before-oid, after-oid.
data Ev = Ev {evType, evAt, evBefore, evAfter :: String}

parse :: String -> Ev
parse l = case words l of
    (t : a : b : c : _) -> Ev t a b c
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
              ... on HeadRefForcePushedEvent { createdAt beforeCommit { oid } afterCommit { oid } }
              ... on BaseRefForcePushedEvent { createdAt beforeCommit { oid } afterCommit { oid } }
            }
          }
        }
      }
    }
    """

isAncestor :: String -> String -> IO Bool
isAncestor a b = do
    (code, _, _) <- readProcessWithExitCode "git" ["merge-base", "--is-ancestor", a, b] ""
    pure (code == ExitSuccess)

-- The base for `head`: the most recently recorded base tip that is still an
-- ancestor of it. `cands` needs to be in chronological order (current tip last).
--
-- If none is an ancestor (e.g. the base advanced through ordinary pushes, which
-- emits no force-push events) fall back to the merge-base with the current base
-- tip: the point where `head` forked from today's base line.
baseFor :: String -> String -> [String] -> IO String
baseFor currentBase head cands = do
    found <- findM (`isAncestor` head) (reverse cands)
    case found of
        Just b -> pure b
        Nothing -> trim <$> sh "git" ["merge-base", currentBase, head]

main :: IO ()
main = do
    [pr] <- getArgs
    ownerRepo <- trim <$> sh "gh" ["repo", "view", "--json", "nameWithOwner", "-q", ".nameWithOwner"]
    let (owner, _ : repo) = break (== '/') ownerRepo
    base <- trim <$> sh "gh" ["pr", "view", pr, "--json", "baseRefName", "-q", ".baseRefName"]
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
                ++ " | \"\\(.__typename) \\(.createdAt) \\(.beforeCommit.oid) \\(.afterCommit.oid)\""
            ]

    let evs = map parse (lines raw)
        heads = filter ((== "HeadRefForcePushedEvent") . evType) evs
        -- Every recorded base tip, in chronological order.
        baseOids =
            nub
                [ oid
                | e <- evs
                , evType e == "BaseRefForcePushedEvent"
                , oid <- [evBefore e, evAfter e]
                ]

    case unsnoc heads of
        Nothing -> putStrLn "No force-push events on this PR. Nothing to diff."
        Just (_, h) -> do
            let oldHead = evBefore h -- the force-push we're reporting on
                newHead = evAfter h
                -- Hidden marker identifying this exact force-push. Lets us run
                -- the program multiple times without duplicating comments.
                marker = "<!-- gh-post-range-diff " ++ evAt h ++ " " ++ oldHead ++ ".." ++ newHead ++ " -->"

            posted <- sh "gh" ["pr", "view", pr, "--json", "comments", "-q", ".comments[].body"]
            if marker `isInfixOf` posted
                then
                    putStrLn ("Already reported on the force-push at " ++ evAt h ++ ". Nothing to do.")
                else do
                    -- Fetch current base tip, both heads, and every historical base oid.
                    --
                    -- refs/rd/base is a scratch ref holding the fetched base tip. We need it
                    -- because without a destination the tip only lands in FETCH_HEAD, which
                    -- this same fetch also fills with the heads and every base oid, so we
                    -- couldn't pick the base tip back out to rev-parse on the next line.
                    callProcess "git" $ ["fetch", "--quiet", "origin", base ++ ":refs/rd/base", oldHead, newHead] ++ baseOids
                    newBaseTip <- trim <$> sh "git" ["rev-parse", "refs/rd/base"]
                    let cands = baseOids ++ [newBaseTip] -- current tip is newest, so it goes last
                    b1 <- baseFor newBaseTip oldHead cands
                    b2 <- baseFor newBaseTip newHead cands
                    diff <- sh "git" ["range-diff", b1 ++ ".." ++ oldHead, b2 ++ ".." ++ newHead]

                    let header = "### Range-diff for force push " ++ take 7 oldHead ++ " → " ++ take 7 newHead
                    callProcess
                        "gh"
                        [ "pr"
                        , "comment"
                        , pr
                        , "--body"
                        , marker ++ "\n" ++ header ++ "\n\n" ++ format diff
                        ]
