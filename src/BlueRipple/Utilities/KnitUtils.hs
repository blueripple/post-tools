{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module BlueRipple.Utilities.KnitUtils
  (
    module BlueRipple.Utilities.KnitUtils
  )
where

import qualified BlueRipple.Configuration as BRC

import qualified Control.Monad.Except as X
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Format as Time
import qualified Data.Time.LocalTime as Time
import qualified Knit.Report as K
import qualified Knit.Report.Input.MarkDown.PandocMarkDown as K
import qualified Path
import Polysemy.Error (Error)
#if MIN_VERSION_streamly(0,8,0)
--import qualified Streamly.Data.Array.Foreign         as Streamly.Array
#else
import qualified Streamly
import qualified Streamly.Memory.Array         as Streamly.Array
#endif
import qualified System.Directory as SD
--import qualified System.Random.MWC as MWC
import qualified Text.Pandoc.Options as PA
--import qualified Streamly.Internal.Memory.ArrayStream as Streamly.ByteString

type KnitEffects r = K.KnitEffects r
type KnitOne r = K.KnitOne r
type KnitMany r = K.KnitMany r

knitX ::
  forall r a.
  K.Member (Error K.PandocError) r =>
  X.ExceptT T.Text (K.Sem r) a ->
  K.Sem r a
knitX ma = runExceptT ma >>= (K.knitEither @r)

copyAsset :: KnitOne r => T.Text -> T.Text -> K.Sem r ()
copyAsset sourcePath destDir = do
  sourceExists <- K.liftKnit $ SD.doesFileExist (toString sourcePath)
  if sourceExists
    then (do
             K.logLE K.Info $
               "If necessary, creating \""
               <> destDir
               <> "\" and copying \""
               <> sourcePath
               <> "\" there"
             K.liftKnit $ do
               let (_, fName) = T.breakOnEnd "/" sourcePath
               SD.createDirectoryIfMissing True (toString destDir)
               SD.copyFile (toString sourcePath) (toString $ destDir <> "/" <> fName))
    else K.knitError $ "\"" <> sourcePath <> "\" doesn't exist (copyAsset)."

brWriterOptionsF :: PA.WriterOptions -> PA.WriterOptions
brWriterOptionsF o =
  let exts = PA.writerExtensions o
   in o
        { PA.writerExtensions =
            PA.enableExtension PA.Ext_header_attributes $
--            PA.enableExtension PA.Ext_grid_tables $
--            PA.enableExtension PA.Ext_pipe_tables $
--            PA.enableExtension PA.Ext_fancy_lists $
--            PA.enableExtension PA.Ext_tex_math_dollars $
              PA.enableExtension PA.Ext_smart exts,
          PA.writerSectionDivs = True
        }

brAddMarkDown :: KnitOne r => T.Text -> K.Sem r ()
brAddMarkDown = K.addMarkDownWithOptions brMarkDownReaderOptions
  where
    brMarkDownReaderOptions =
      let exts = PA.readerExtensions K.markDownReaderOptions
       in PA.def
            { PA.readerStandalone = True,
              PA.readerExtensions =
                PA.enableExtension PA.Ext_smart
                  . PA.enableExtension PA.Ext_raw_html
                  . PA.enableExtension PA.Ext_escaped_line_breaks
                  $ exts
            }

brAddAnchor :: KnitOne r => T.Text -> K.Sem r Text
brAddAnchor t = do
  brAddMarkDown $ "<a name=\"" <> t <> "\"></a>"
  return $ "#" <> t

brLineBreak :: KnitOne r => K.Sem r ()
brLineBreak = brAddMarkDown "\\\n"

brAddDates ::
  Bool -> Time.Day -> Time.Day -> M.Map String String -> M.Map String String
brAddDates updated pubDate updateDate tMap =
  let formatTime t = Time.formatTime Time.defaultTimeLocale "%B %e, %Y" t
      pubT = one ("published", formatTime pubDate)
      updT = if updated
             then
               (
                 if updateDate > pubDate
                 then one ("updated", formatTime updateDate)
                 else M.empty
               )
             else M.empty
   in tMap <> pubT <> updT

brGetTextFromFile :: KnitEffects r => Path.Path Path.Abs Path.File -> K.Sem r (Maybe Text)
brGetTextFromFile p = do
  isPresent <- K.liftKnit $ SD.doesFileExist $ Path.toFilePath p
  if isPresent
    then Just <$> K.liftKnit (T.readFile $ Path.toFilePath p)
    else return Nothing

brAddToPostFromFile :: KnitOne r => (Text -> K.Sem r ()) -> Bool -> Path.Path Path.Abs Path.File -> K.Sem r ()
brAddToPostFromFile toPost errorIfMissing p = do
  tM <- brGetTextFromFile p
  case tM of
    Just t -> toPost t
    Nothing -> do
      case errorIfMissing of
        True -> K.knitError $ "Post input file " <> show p <> " not found!"
        False -> do
          K.logLE K.Warning $ "Post input file " <> show p <> " not found. Inserting placeholder text"
          brAddMarkDown $ "**Post input file " <> show p <> " not found. Please fix/remove**"

brAddToPostFromFileWith :: KnitOne r => (Text -> K.Sem r ()) -> Bool -> Path.Path Path.Abs Path.File -> Maybe Text -> K.Sem r ()
brAddToPostFromFileWith toPost errorIfMissing p mRefs = brAddToPostFromFile toPost' errorIfMissing p
  where
    toPost' = maybe toPost (\t -> toPost . (<> "\n" <> t)) mRefs

brAddPostMarkDownFromFileWith :: KnitOne r => BRC.PostPaths Path.Abs -> Text -> Maybe Text -> K.Sem r ()
brAddPostMarkDownFromFileWith pp postFileEnd mRefs = do
  postInputPath <- K.knitEither $ BRC.postInputPath pp (postFileEnd <> ".md")
  brAddToPostFromFileWith K.addMarkDown False postInputPath mRefs

brAddSharedMarkDownFromFileWith :: KnitOne r => BRC.PostPaths Path.Abs -> Text -> Maybe Text -> K.Sem r ()
brAddSharedMarkDownFromFileWith pp postFileEnd mRefs = do
  postInputPath <- K.knitEither $ BRC.sharedInputPath pp (postFileEnd <> ".md")
  brAddToPostFromFileWith K.addMarkDown False postInputPath mRefs

brAddPostMarkDownFromFile :: KnitOne r => BRC.PostPaths Path.Abs -> Text -> K.Sem r ()
brAddPostMarkDownFromFile pp postFileEnd = brAddPostMarkDownFromFileWith pp postFileEnd Nothing

brAddSharedMarkDownFromFile :: KnitOne r => BRC.PostPaths Path.Abs -> Text -> K.Sem r ()
brAddSharedMarkDownFromFile pp postFileEnd = brAddSharedMarkDownFromFileWith pp postFileEnd Nothing


brAddNoteMarkDownFromFileWith :: KnitOne r
  => BRC.PostPaths Path.Abs -> BRC.NoteName -> Text -> Maybe Text -> K.Sem r ()
brAddNoteMarkDownFromFileWith  pp nn noteFileEnd mRefs = do
  notePath <- K.knitEither $ BRC.noteInputPath pp nn (noteFileEnd <> ".md")
  brAddToPostFromFileWith K.addMarkDown False notePath mRefs

brAddNoteMarkDownFromFile :: KnitOne r => BRC.PostPaths Path.Abs -> BRC.NoteName -> Text -> K.Sem r ()
brAddNoteMarkDownFromFile  pp nn noteFileEnd = brAddNoteMarkDownFromFileWith pp nn noteFileEnd Nothing

brAddNoteRSTFromFileWith :: KnitOne r
  => BRC.PostPaths Path.Abs -> BRC.NoteName -> Text -> Maybe Text -> K.Sem r ()
brAddNoteRSTFromFileWith pp nn noteFileEnd mRefs = do
  notePath <- K.knitEither $ BRC.noteInputPath pp nn (noteFileEnd <> ".rst")
  brAddToPostFromFileWith K.addRST False notePath mRefs

brAddNoteRSTFromFile :: KnitOne r => BRC.PostPaths Path.Abs -> BRC.NoteName -> Text -> K.Sem r ()
brAddNoteRSTFromFile  pp nn noteFileEnd = brAddNoteRSTFromFileWith pp nn noteFileEnd Nothing

brDatesFromPostInfo :: KnitEffects r => BRC.PostInfo -> K.Sem r (M.Map String String)
brDatesFromPostInfo (BRC.PostInfo postStage  (BRC.PubTimes ppt mUpt)) = do
  tz <- liftIO $ Time.getCurrentTimeZone
  let formatPTime t = Time.formatTime Time.defaultTimeLocale "%B %e, %Y" t
      formatUPTime t = formatPTime t <> Time.formatTime Time.defaultTimeLocale " (%H:%M:%S)" t
--      getCurrentDay :: KnitEffects r => K.Sem r Time.Day
--      getCurrentDay = K.getCurrentTime
      dayFromPT = \case
        BRC.Unpublished -> case postStage of
          BRC.OnlinePublished -> formatPTime . Time.utcToLocalTime tz <$> K.getCurrentTime
          _ -> formatUPTime . Time.utcToLocalTime tz <$> K.getCurrentTime
        BRC.Published d -> return $ formatPTime d
  pt <- dayFromPT ppt
  let mp = one ("published", pt)
  case mUpt of
    Nothing -> return mp
    (Just upt) -> do
      ut <- dayFromPT upt
      return $ mp <> one ("updated", ut)


brNewPost :: KnitMany r
          => BRC.PostPaths Path.Abs
          -> BRC.PostInfo
          -> Text
          -> K.Sem (K.ToPandoc ': r) ()
          -> K.Sem r ()
brNewPost pp pi' pageTitle content = do
  dates <- brDatesFromPostInfo pi'
  let postPath = BRC.postPath pp pi'
      pageConfig = dates <> one ("pagetitle", toString pageTitle)
  K.newPandoc (K.PandocInfo (toText $ Path.toFilePath postPath) pageConfig) $ do
    content
    brAddMarkDown BRC.brReadMore

brNewNote :: KnitMany r
          => BRC.PostPaths Path.Abs
          -> BRC.PostInfo
          -> BRC.NoteName
          -> Text
          -> K.Sem (K.ToPandoc ': r) ()
          -> K.Sem r (Maybe Text)
brNewNote pp pi' nn pageTitle content = do
  dates <- brDatesFromPostInfo pi'
  notePath <- K.knitEither $ BRC.notePath pp pi' nn
  let pageConfig = dates <> one ("pagetitle", toString pageTitle)
  K.newPandoc (K.PandocInfo (toText $ Path.toFilePath notePath) pageConfig) content
  case nn of
    BRC.Unused _ -> return Nothing
    BRC.Used _ -> Just <$> (K.knitEither $ BRC.noteUrl pp pi' nn)

jsonLocations :: BRC.PostPaths Path.Abs -> BRC.PostInfo -> (Path.Path Path.Abs Path.Dir, Text -> Either Text Text)
jsonLocations pp pi' = (BRC.dataDir pp pi', BRC.dataURL pp pi')

-- returns URL
brAddJSON :: KnitEffects r
          => BRC.PostPaths Path.Abs
          -> BRC.PostInfo
          -> Text
          -> A.Value
          -> K.Sem r Text
brAddJSON pp postInfo jsonName jsonVal = do
  let destDir = BRC.dataDir pp postInfo
      jsonFileName = jsonName <> ".json"
  K.liftKnit $ SD.createDirectoryIfMissing True (Path.toFilePath destDir)
  jsonPath' <- K.knitEither $ BRC.jsonPath pp postInfo jsonFileName
  K.liftKnit $ A.encodeFile (Path.toFilePath jsonPath') jsonVal
  K.knitEither $ BRC.dataURL pp postInfo jsonFileName

-- Copy data from an absolute path into data directory so it's available for, e.g.,
-- hvega
data WhenDestExists = ReplaceExisting | ExistsIsError | LeaveExisting deriving stock (Eq)
brCopyDataForPost ::  KnitEffects r
                => BRC.PostPaths Path.Abs
                -> BRC.PostInfo
                -> WhenDestExists
                -> Path.Path Path.Abs Path.File
                -> Maybe Text
                -> K.Sem r Text
brCopyDataForPost pp postInfo whenExists srcFile destFileNameM = do
  let destDir = BRC.dataDir pp postInfo
  srcExists <- K.liftKnit $ SD.doesFileExist (Path.toFilePath srcFile)
  destFileName <- K.liftKnit @IO -- FIXME: this is a hack to handle the possible thrown error
                  $ maybe (pure $ Path.filename srcFile) (Path.parseRelFile)
                  $ fmap toString destFileNameM
  let tgtFile = destDir Path.</> Path.filename destFileName
      doCopy = K.liftKnit $ SD.copyFile (Path.toFilePath srcFile) (Path.toFilePath tgtFile)
      createDirIfNec = K.liftKnit $ SD.createDirectoryIfMissing True (Path.toFilePath destDir)
      url = K.knitEither $ BRC.dataURL pp postInfo (toText $ Path.toFilePath $ Path.filename tgtFile)
  when (not srcExists) $ K.knitError $ "brCopyDataForPost: source file (" <> show srcFile <> ") does not exist."
  destExists <- K.liftKnit $ SD.doesFileExist (Path.toFilePath tgtFile)
  when (destExists && whenExists == ExistsIsError)
    $ K.knitError
    $ "brCopyDataForPost: file (" <> show (Path.filename srcFile) <> ") exists in destination and whenExisting==ExistsIsError."
  if destExists && whenExists == LeaveExisting
    then K.logLE K.Info $ "brCopyDataForPost: target (" <> show tgtFile <> ") exists. Not copying since whenExists==LeaveExisting"
    else (do
             if destExists
               then K.logLE K.Info $ "brCopyDataForPost: overwriting " <> show tgtFile <> " with " <> show srcFile <> ", since whenExists==ReplaceExisting"
               else K.logLE K.Info $ "brCopyDataForPost: copying " <> show srcFile <> " to " <> show tgtFile
             createDirIfNec
             doCopy
         )
  url
--  let noteParent = Path.parent notePath
--  K.logLE K.Info $ "If necessary, creating note path \"" <> toText (Path.toFilePath noteParent)
