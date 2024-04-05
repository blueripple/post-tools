{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoStrictData #-}
{-# OPTIONS_GHC -fno-cse #-}
module BlueRipple.Configuration
  (module BlueRipple.Configuration
  , module Path
  ) where

import           Data.String.Here               ( i )
import qualified Data.Text as T
import qualified Path
import qualified Path.IO as Path
import           Path (Path, Abs, Rel, Dir, File, PathException, (</>), parseRelDir)
import qualified Data.Time.Calendar as Time
import qualified System.Console.CmdArgs as CmdArgs
import System.Console.CmdArgs ((&=)) -- , (+=), Annotate((:=)))
import qualified Say
import qualified Knit.Effect.Logger as K
import qualified Graphics.Vega.VegaLite        as GV
import Graphics.Vega.VegaLite.Configuration (configuredVegaLiteSchema, ViewConfig)

brReadMore :: T.Text
brReadMore = [i|
*Want to read more from Blue Ripple?
Visit our [**website**](${brHome}),
sign up for [**email updates**](${brEmailSignup}),
and follow us on [**Twitter**](${brTwitter})
and [**FaceBook**](${brFaceBook}).
Folks interested in our data and modeling efforts should also check out our
[**Github**](${brGithub}) page.*
|]


brHome :: T.Text
brHome = "https://www.blueripplepolitics.org"

brEmailSignup :: T.Text
brEmailSignup = "http://eepurl.com/gzmeQ5"

brTwitter :: T.Text
brTwitter = "https://twitter.com/BlueRipplePol"

brFaceBook :: T.Text
brFaceBook = "https://www.facebook.com/blueripplepolitics"

brGithub :: T.Text
brGithub = "https://github.com/blueripple"

brGithubLanding :: T.Text
brGithubLanding = brGithub <> "/Guide"

brResearch :: T.Text
brResearch = "research"

brResearchRootUrl :: T.Text
brResearchRootUrl = "https://blueripple.github.io/" <> brResearch <> "/"

brResearchRootPath :: T.Text
brResearchRootPath = "/" <> brResearch <> "/"

brExplainer :: T.Text
brExplainer = "explainer"

brExplainerRootUrl :: T.Text
brExplainerRootUrl = "https://blueripple.github.io/" <> brExplainer <> "/"

brExplainerRootPath :: T.Text
brExplainerRootPath = "/" <> brExplainer <> "/"

brGithubUrl :: T.Text -> T.Text
brGithubUrl x = "https://blueripple.github.io" <> x <> ".html"

brLocalRoot :: T.Text
brLocalRoot = "posts/"

-- command line
data LogLevel = LogInfo | LogDiagnostic | LogDebugMinimal | LogDebugVerbose | LogDebugAll  deriving stock (Show, CmdArgs.Data, Typeable, Ord, Eq)

knitLogSeverity :: LogLevel -> K.LogSeverity -> Bool
knitLogSeverity LogInfo = K.nonDiagnostic
knitLogSeverity LogDiagnostic = K.logDiagnostic
knitLogSeverity LogDebugMinimal = K.logDebug 1
knitLogSeverity LogDebugVerbose = K.logDebug 3
knitLogSeverity LogDebugAll = K.logDebug 10

-- subDir is partial. Which triggers a warning. But the name of the field is used to form the commandLine so
-- we don't want to prefix it with an underscore to suppress the warning
data CommandLine =
  CLLocalDraft { logLevel :: LogLevel, stanChains :: Int, subDir :: Maybe Text, postNames :: [Text] }
  | CLOnlineDraft { logLevel :: LogLevel, stanChains :: Int, postNames :: [Text]}
  | CLPublished {logLevel :: LogLevel, stanChains :: Int, postNames :: [Text] }
  deriving stock (Show, CmdArgs.Data, Typeable, Eq)

logLevelCL :: LogLevel
logLevelCL = LogInfo &= CmdArgs.typ "LOGLEVEL" &= CmdArgs.help "logging Verbosity. One of LogInfo, LogDiagnostic, LogDebugMinimal, LogDebugVerbose, LogDebugAll"

stanChainsCL :: Int
stanChainsCL = 4 &= CmdArgs.typ "NUMCHAINS" &= CmdArgs.help "Number of Stan chains to run."

subDirCL :: Maybe Text
subDirCL = Nothing &= CmdArgs.typFile &= CmdArgs.help "Subdirectory for draft"

postNamesCL :: [Text]
postNamesCL = [] &= CmdArgs.typ "[POSTS]" &= CmdArgs.help "post names" -- &= CmdArgs.args

localDraft :: CommandLine
localDraft = CLLocalDraft
             {
               logLevel = logLevelCL
             , stanChains = stanChainsCL
             , subDir = subDirCL
             , postNames = postNamesCL
             } &= CmdArgs.help "Build local drafts" &= CmdArgs.auto

onlineDraft :: CommandLine
onlineDraft = CLOnlineDraft
              { logLevel = logLevelCL
              , stanChains = stanChainsCL
              , postNames = postNamesCL
              } &= CmdArgs.help "Build online drafts (in blueripple.github.io directory)"

published :: CommandLine
published = CLPublished
            {
              logLevel = logLevelCL
            , stanChains = stanChainsCL
            , postNames = postNamesCL
            } &= CmdArgs.help "Build for publication (in blueripple.github.io directory)"


commandLine :: CmdArgs.Mode (CmdArgs.CmdArgs CommandLine)
commandLine = CmdArgs.cmdArgsMode $ CmdArgs.modes [localDraft, onlineDraft, published] &= CmdArgs.help "Build Posts"

postStage :: CommandLine -> PostStage
postStage (CLLocalDraft _ _ _ _) = LocalDraft
postStage (CLOnlineDraft _ _ _) = OnlineDraft
postStage (CLPublished _ _ _) = OnlinePublished

clStanChains :: CommandLine -> Int
clStanChains = stanChains

clStanParallel :: CommandLine -> StanParallel
clStanParallel cl = StanParallel (clStanChains cl) MaxCores

data StanParallel = StanParallel { parallelChains :: Int, cores :: StanCores } deriving stock (Show, Eq)

data StanCores = MaxCores | FixedCores Int deriving stock (Show, Eq)


-- I want functions to support
-- 1. Putting post documents in the right place in a tree for both draft and post
-- 2. Support Unused bits, existing only in dev.
-- 3. Help forming the correct URLs for links in either case
--data Output = Draft | Post deriving (Show)

data NoteName = Used Text | Unused Text deriving stock Show

data PubTime = Unpublished | Published Time.Day deriving stock Show

data PostStage = LocalDraft
               | OnlineDraft
               | OnlinePublished deriving stock (Show, Read, CmdArgs.Data, Typeable, Eq)

data PubTimes =  PubTimes { initialPT :: PubTime
                          , updatePT  :: Maybe PubTime
                          } deriving stock Show

data PostInfo = PostInfo PostStage PubTimes

data PostPaths a = PostPaths { sharedInputsDir :: Path a Dir -- inputs shared among various posts
                             , inputsDir :: Path a Dir -- place to put inputs (markDown, etc.)
                             , localDraftDir :: Path a Dir
                             , onlineDraftDir :: Path a Dir
                             , onlinePubDir :: Path a Dir -- local html location, to be pushed to github pages
                             , localWebDir :: Path a Dir -- files that will be served at http://localhost:1313/
                             , draftUrlRoot :: Path Abs Dir -- URL root for post links, without "https:/"
                             , pubUrlRoot :: Path Abs Dir -- URL root for post links, without "https:/"
                             } deriving stock Show

absPostPaths :: Path Abs Dir -> PostPaths Rel -> PostPaths Abs
absPostPaths s (PostPaths si i' ld pd pp lwd dr pr) =
  PostPaths (s </> si) (s </> i') (s </> ld) (s </> pd) (s </> pp) (s </> lwd) dr pr

defaultLocalRoot :: Path Abs Dir
defaultLocalRoot = [Path.absdir|/Users/adam/BlueRipple|]

noteRelDir :: Path Rel Dir
noteRelDir = [Path.reldir|Notes|]

dataRelDir :: Path Rel Dir
dataRelDir = [Path.reldir|data|]

unusedRelDir :: Path Rel Dir
unusedRelDir = [Path.reldir|Unused|]

postPaths :: MonadIO m
          => Path Abs Dir -> Path Rel Dir -> Path Rel Dir -> Path Rel Dir -> Path Rel Dir -> m (PostPaths Abs)
postPaths localRoot siP iP ldP postRel = do
  let pp = absPostPaths
           localRoot
           $ PostPaths
           ([Path.reldir|research|] </> siP)
           ([Path.reldir|research|] </> iP)
           ([Path.reldir|research|] </> ldP)
           ([Path.reldir|blueripple.github.io/Draft|] </> postRel)
           ([Path.reldir|blueripple.github.io|] </> postRel)
           ([Path.reldir|localWebRoot|])
           ([Path.absdir|/blueripple.github.io/Draft|] </> postRel)
           ([Path.absdir|/blueripple.github.io|] </> postRel)
  Say.say "If necessary, creating post input directories"
  let sharedINotesP = sharedInputsDir pp </> noteRelDir
      iNotesP = inputsDir pp </> noteRelDir
      iUnusedP =   inputsDir pp </> unusedRelDir
  Say.say $ toText $ Path.toFilePath sharedINotesP
  Path.ensureDir sharedINotesP
  Say.say $ toText $ Path.toFilePath iNotesP
  Path.ensureDir iNotesP
  Say.say $ toText $ Path.toFilePath iUnusedP
  Path.ensureDir iUnusedP
  return pp

dataDir ::   PostPaths a -> PostInfo -> Path a Dir
dataDir pp (PostInfo ps _) = case ps of
  LocalDraft -> localWebDir pp </> [Path.reldir|data|]
  OnlineDraft -> onlineDraftDir pp </>  [Path.reldir|data|]
  OnlinePublished -> onlinePubDir pp </> [Path.reldir|data|]

postInputPath :: PostPaths a -> Text -> Either Text (Path a File)
postInputPath pp postFileEnd = do
  pTail <- first show $ Path.parseRelFile $ toString $ "post" <> postFileEnd
  return $ inputsDir pp </> pTail

sharedInputPath :: PostPaths a -> Text -> Either Text (Path a File)
sharedInputPath pp fileName = do
  pTail <- first show $ Path.parseRelFile $ toString fileName
  return $ sharedInputsDir pp </> pTail

noteInputPath ::  PostPaths a -> NoteName -> Text -> Either Text (Path a File)
noteInputPath pp noteName noteFileEnd = do
  pTail <- first show
           $ case noteName of
               Used t -> fmap (\s -> [Path.reldir|Notes|] </> s) $ Path.parseRelFile $ toString (t <> noteFileEnd)
               Unused t ->   fmap (\s -> [Path.reldir|Unused|] </> s) $ Path.parseRelFile $ toString (t <> noteFileEnd)
  return $ inputsDir pp </> pTail

postPath :: PostPaths a -> PostInfo -> Path a File
postPath pp (PostInfo ps _) = case ps of
  LocalDraft -> localDraftDir pp </> [Path.relfile|post|]
  OnlineDraft -> onlineDraftDir pp </>  [Path.relfile|post|]
  OnlinePublished -> onlinePubDir pp </> [Path.relfile|post|]

-- Unused do not get put on github pages
notePath :: PostPaths a -> PostInfo -> NoteName -> Either Text (Path a File)
notePath pp (PostInfo ps _) nn = do
  let parseRel = first show . Path.parseRelFile . toString
  case nn of
    Unused t -> fmap (\s -> localDraftDir pp </> unusedRelDir </> s) $ parseRel t
    Used t -> case ps of
      LocalDraft -> fmap (\s -> localDraftDir pp </> noteRelDir </> s) $ parseRel t
      OnlineDraft -> fmap (\s -> onlineDraftDir pp </> noteRelDir </> s) $ parseRel t
      OnlinePublished ->fmap (\s -> onlinePubDir pp </> noteRelDir </> s) $ parseRel t

-- | Given PostPaths, post info and a note name, produce the link URL
noteUrl :: PostPaths Abs -> PostInfo -> NoteName -> Either Text Text
noteUrl pp (PostInfo ps _) noteName = do
  noteNameRelFile <- case noteName of
    Used t -> first show $ Path.parseRelFile (toString $ t <> ".html")
    Unused t -> Left $ "Cannot link to unused note (" <> t <> ")"
  let noteRelFile :: Path Rel File = noteRelDir </> noteNameRelFile
      noteUrl' = case ps of
        LocalDraft -> Path.toFilePath noteRelFile
        OnlineDraft -> "https:/" <> Path.toFilePath (draftUrlRoot pp </> noteRelFile)
        OnlinePublished -> "https:/" <> Path.toFilePath (pubUrlRoot pp </> noteRelFile)
  return $ toText noteUrl'

jsonPath :: PostPaths a -> PostInfo -> Text -> Either Text (Path a File)
jsonPath pp postInfo jn = do
  let parseRel = first show . Path.parseRelFile . toString
      dataPath = dataDir pp postInfo
  (dataPath </>) <$> parseRel jn

-- | Given PostPaths and PostInfo and a relative data-file path, construct URL (e.g., for hvega)
dataURL :: PostPaths Abs -> PostInfo -> Text -> Either Text Text
dataURL pp (PostInfo ps _) jsonName = do
  jsonNameRelFile <- first show $ Path.parseRelFile (toString $ jsonName)
  let jsonRelFile :: Path Rel File = dataRelDir </> jsonNameRelFile
      jsonUrl' = case ps of
        LocalDraft -> "http://localhost:1313/" <> Path.toFilePath jsonRelFile
        OnlineDraft -> "https:/" <> Path.toFilePath (draftUrlRoot pp </> jsonRelFile)
        OnlinePublished -> "https:/" <> Path.toFilePath (pubUrlRoot pp </> jsonRelFile)
  pure $ toText jsonUrl'

brVLSchema :: Text
brVLSchema = GV.vlSchema 5 Nothing Nothing Nothing

brConfiguredVegaLite :: ViewConfig -> [(GV.VLProperty, GV.VLSpec)] -> GV.VegaLite
brConfiguredVegaLite = configuredVegaLiteSchema brVLSchema
