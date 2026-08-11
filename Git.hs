{- | The gh-free git plumbing: base reconstruction and the shallow-clone
deepening, split out from "Main" so the test can drive them against real
throwaway repos without going through the `gh` CLI.
-}
module Git (sh, baseFor, unshallowRange) where

import Control.Monad (unless, when)
import Control.Monad.Extra (anyM, findM)
import Data.List.Extra (trim)
import System.Exit (ExitCode (..))
import System.Process (callProcess, readProcess, readProcessWithExitCode)

sh :: String -> [String] -> IO String
sh cmd args = readProcess cmd args ""

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

isShallowRepo :: IO Bool
isShallowRepo = (== "true") . trim <$> sh "git" ["rev-parse", "--is-shallow-repository"]

-- Deepen `head` until a recorded base tip is reachable from it, so baseFor can
-- resolve to that tip. Grows the step so a deep base is reached in a few fetches,
-- and stops when a deepen brings nothing new (head is fully present), where
-- baseFor's merge-base fallback then applies. Only fetches this head's own line,
-- never the whole history.
--
-- It takes the whole `baseOids` set, not a single guessed tip, and that is what
-- bounds the depth: since `head` grows from its tip downward, the `anyM` stop
-- halts at the *nearest* recorded base -- exactly the one baseFor will pick -- so
-- we never deepen past it toward older tips. This is also why the deepening can't
-- live inside baseFor's per-candidate findM: confirming a base *is* an ancestor is
-- cheap (deepen until it appears), but confirming one is *not* would require
-- deepening `head` to the root, and findM would hit that on any non-ancestor tip it
-- tried before the real base.
deepenToBase :: [String] -> String -> IO ()
deepenToBase baseOids head = go 1
  where
    go step = do
        connected <- anyM (`isAncestor` head) baseOids
        shallow <- isShallowRepo
        when (not connected && shallow) $ do
            before <- reachable
            callProcess "git" ["fetch", "--quiet", "--deepen=" ++ show step, "origin", head]
            after <- reachable
            when (after /= before) (go (step * 2))
    reachable = sh "git" ["rev-list", "--count", head]

-- A shallow clone (git clone --depth, actions/checkout with fetch-depth: 1) is
-- missing the commits between each head and its base, so range-diff would walk a
-- truncated range and baseFor's ancestry checks would fail or crash. Deepen just
-- enough to fix that, never dragging in the whole history.
--
-- Fast path: pull each head's commits down to its fork with the base branch
-- (--shallow-exclude), then one level deeper so that fork commit itself is linked
-- into the graph. That is the base baseFor resolves to for an ordinary head-only
-- force-push, a base that merely advanced, or a base-ref force-push whose old tip
-- the head still sits on -- i.e. every ordinary shape.
--
-- The one case the fast path misses is a base-ref force-push that left a head
-- based *below* that fork; its recorded base tip is not a ref we can
-- --shallow-exclude, so deepen each head by SHA until that tip is reachable.
-- `base` is the base branch name; refs/rd/base must already point at its tip.
unshallowRange :: String -> String -> String -> [String] -> IO ()
unshallowRange base oldHead newHead baseOids = do
    shallow <- isShallowRepo
    when shallow $ do
        callProcess "git" ["fetch", "--quiet", "--shallow-exclude=" ++ base, "origin", oldHead, newHead]
        callProcess "git" ["fetch", "--quiet", "--deepen=1", "origin", oldHead, newHead]
        unless (null baseOids) $ do
            deepenToBase baseOids oldHead
            deepenToBase baseOids newHead
