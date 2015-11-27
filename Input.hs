-----------------------------------------------------------------------------
-- |
-- Module      :  Groove.Input
-- Copyright   :  (c) Carlos Gómez 2013
--
-- Basic parsing of MusicXML files.
--
-----------------------------------------------------------------------------

module Input where

--import Debug.Trace

import Prelude hiding (catch)

-- --import Basic
-- import Viewpoints (Event)

--import Euterpea as Euterpea hiding (Event)
--import Codec.Midi

--import System.IO
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Data.Char
import Data.List
--import Data.Function
import Data.Maybe
import Data.Either

import Text.XML.HaXml
import Text.XML.HaXml.Posn
--import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Util
--import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse

type Evento = (Int, Int)

type Event = (Int, Int, Bool, Int, Bool, Int, Int)

--type PreprocFunction = Midi -> Midi

--collections :: [(FilePath, Maybe PreprocFunction)]
--collections = [("/home/carlos/Groove/midi/", Just (addProgChanges . (cropTrack 1)))]

loadMusicXmls :: String -> IO ([[Evento]], [String])
loadMusicXmls xml_dir = do
    d <- catch (getDirectoryContents $ xml_dir) catchDirError
    let paths = map (xml_dir ++) $ filter (\x -> not (x == "." || x == "..")) d
    let paths' = sort paths
    read_files <- readFiles paths'
    let (errors1, files) = partitionEithers read_files
    --putStrLn $ "Files:\n" ++ (unlines $ map show ([0..] `zip` file_names))
    putStrLn $ unlines $ errors1
    putStrLn $ (show (length files)) ++ " files read"
    let read_xmls = map parseFile files
    let (errors2, xmls) = partitionEithers read_xmls
    putStrLn $ unlines $ errors2
    putStrLn $ (show (length xmls)) ++ " files parsed"
    let ses = map parseDocument xmls
    let (errors, seqfns) = partitionEithers ses
    putStrLn $ unlines $ errors
    putStrLn $ (show (length seqfns)) ++ " files interpreted"
    let (sequences, filenames) = unzip seqfns
    let sequences' = map (map (\ (x,_,_,_,_,_,y) -> (x,y))) sequences
    return (sequences', filenames)

--loadMusic :: IO [Music Pitch]
--loadMusic = do
--    let coll = head collections
--    let inputDirectory = fst $ coll
--    d <- catch (getDirectoryContents $ inputDirectory) catchDirError
--    let files = map (inputDirectory ++)
--                    $ filter (\x -> not (x == "." || x == "..")) d
--    midis <- readMidi files
--    let midis' = fst $ unzip midis
--    let files' = snd $ unzip midis
--    _ <- putStrLn $ "Files:\n" ++ (unlines $ map show ([1..] `zip` files))
--    putStrLn $ (show (length midis')) ++ " files read"
--    let preFunction = case snd coll of
--                          Just f -> f
--                          Nothing -> id
--    let music = map (fromMidi . preFunction) midis'
--    -- Remove the Context, UserPatchMap and note attributes:
--    let examples = map ((mMap fst) . (\ (x, _, _) -> x)) music
--    --let examples' = map (mFold (:+:) (\x _ -> x) Prim (\ _ y -> y)) examples
--    -- Remove modifiers (i.e. controls):
--    let examples' = map (mFold Prim (:+:) (:=:) (\ _ y -> y)) examples
--    --return = map lineToList1 examples'
--    return examples'

catchDirError :: IOError -> IO [FilePath]
catchDirError e | isDoesNotExistError e = error $ msg ++ " :file not found"
                | isPermissionError e = error $ msg ++ " :permission denied"
                | otherwise = error $ msg
                where
                    msg = "Error reading directory"

--readMidi :: [FilePath] -> IO [(Midi, String)]
--readMidi [] = return []
--readMidi (x:xs) = do
--    v <- importFile x
--    case v of
--        Left _ -> rest
--        Right m -> do
--                    r <- rest
--                    return ((m, x):r)
--        where
--            rest = readMidi xs

--cropTrack :: Int -> Midi -> Midi
--cropTrack n (Midi ft td trs) | n >= length trs =
--                                   error "Groove.Main.cropTrack: invalid track number"
--                             | otherwise = Midi ft td [trs!!n]

--addProgChanges :: Midi -> Midi
--addProgChanges (Midi ft td trs) = Midi ft td (map addProgChange trs)

--addProgChange :: Track Ticks -> Track Ticks
--addProgChange t | (searchPC t == False) = case searchChanNumber t of
--                                          Just ch -> ((0, ProgramChange ch 1):t)
--                                          Nothing -> t
--                | otherwise = t

--searchPC :: Track Ticks -> Bool
---- (Adapted from Euterpea/FromMidi.lhs)
--searchPC ((_, ProgramChange ch num):es)   = True
--searchPC ((t, NoteOn _ _ _):es) | t > 0   = False
--searchPC (e:es)                           = searchPC es
--searchPC []                               = False

--searchChanNumber :: Track Ticks -> Maybe Channel
--searchChanNumber ((_, ProgramChange ch _):es) = Just ch
--searchChanNumber ((_, NoteOn ch _ _):es) = Just ch
--searchChanNumber (e:es) = searchChanNumber es
--searchChanNumber [] = Nothing

---- (Adapted from Euterpea/Music/Note/MoreMusic.hs)
--lineToList1                    :: Music a -> [Music a]
----lineToList1 _ | trace ("lineToList1 ") False = undefined
--lineToList1 p@(Prim x)         = [p]
--lineToList1 (n :+: ns)         = n : lineToList1 ns
--lineToList1 _                  =
--    error "Groove.Main.lineToList1: a line should only contain Prim and :=:"

stepToInt :: String -> Int
stepToInt "C" = 0
stepToInt "D" = 2
stepToInt "E" = 4
stepToInt "F" = 5
stepToInt "G" = 7
stepToInt "A" = 9
stepToInt "B" = 11
stepToInt _ = -1

getFermata :: Content Posn -> Bool
getFermata c = let
  check [] = False
  check _ = True
  ffermata = keep /> tag "notations" /> tag "fermata"
  fermata = ffermata c
  in check fermata

getDur :: Int -> Content Posn -> Int
getDur divs n = case i of
  Just d -> let d' = ((fromIntegral d) * 4.0 / (fromIntegral divs))
    in if (snd $ properFraction d') == 0
          then round d' else -1
  Nothing -> -1
  where
  i = getInt duration
  fduration = keep /> tag "duration" /> txt
  duration = fduration n

getPitch :: Content Posn -> Int
getPitch n = case (s, o) of
  (Nothing, _) -> -1
  (_, Nothing) -> -1
  (Just s', Just o') ->
    let stp = stepToInt (map toUpper s') in
      if stp >= 0 then ((o'+1) * 12) + stp + a'
      else -1
  where
  a' = case a of
    Nothing -> 0
    Just x -> x
  fstep = keep /> tag "pitch" /> tag "step" /> txt
  falter = keep /> tag "pitch" /> tag "alter" /> txt
  foctave = keep /> tag "pitch" /> tag "octave" /> txt
  a = getInt alter
  alter = falter n 
  s = getString step 
  o = getInt octave
  step = fstep n 
  octave = foctave n
  
readFiles :: [FilePath] -> IO [Either String (String, String)]
readFiles xs = readFiles1 xs []

readFiles1 :: [FilePath] -> [Either String (String, String)] 
  -> IO [Either String (String, String)]
readFiles1 [] ds = return ds
readFiles1 (f:fs) ds = do
  m <- readFromFile f `catch` readError
  let a = case m of
            Left s -> Left s
            Right d -> Right (d,f)
  readFiles1 fs (ds++[a])

parseFile :: (String,String) -> Either String (Document Posn, String)
parseFile (d,s) = case m of 
    Left e -> Left e
    Right x -> Right (x, s)
  where m = xmlParse' s d

readError :: IOError -> IO (Either String String)
readError _ = return (Left "Groove.Input: Error reading an input file")

readFromFile :: String -> IO (Either String String)
readFromFile f = do 
--  h <- openFile f ReadMode
  s <- readFile f
--  hClose h
  return (Right s)

parseMeasure :: Int -> Int -> Bool -> Int -> Either String Int -> Content Posn 
  -> (Either String Int, [Event])
parseMeasure _ _ _ _ (Left s) _ = (Left s, [])
parseMeasure divs fifths mode ts (Right t) m = case e of
    Right i -> if i < t then (Left measureError, []) else (Right end, notes)
    Left s -> (Left s, [])
  where
  end = if (snd $ properFraction l) == 0
           then t + (round l) else tsError $ "Time signature: " ++ show ts
  l = (fromIntegral ts) -- * (fromIntegral divs) / 4.0
  fnotes = keep /> tag "note"
  ns = fnotes m
  (e, notes) = mapAccumR (parseNote divs fifths mode ts) (Right end) ns

measureError :: String
measureError = "Measure length exceded"
  
tsError :: String -> Int
tsError s = error $ "Groove.Input: time signature too small in input file. "
  ++ s

parseNote :: Int -> Int -> Bool -> Int -> Either String Int -> Content Posn
  -> (Either String Int, Event)
parseNote _ _ _ _ (Left s) _ = (Left s, nullEvent)
parseNote divs fifths mode ts (Right t') n
  | duration > 0 && pitch > 0 = (Right t, note)
  | duration > 0 = (Right t, nullEvent)
  | otherwise = (Left "Error parsing note", nullEvent)
  where
  fer = getFermata n
  duration = getDur divs n
  pitch = getPitch n
  note = (pitch, fifths, mode, ts, fer, t, duration)
  t = t' - duration

nullEvent :: Event
nullEvent = (-1, 0, False, 0, False, 0, 0)

isNullEvent :: Event -> Bool
isNullEvent (-1, _, _, _, _, _, _) = True
isNullEvent _ = False
  
parseXml :: [(String, String)] -> IO [(Document Posn, String)]
parseXml [] = return []
parseXml ((s,f):xs) = do
  let m = xmlParse' f s
  case m of
    Left e -> do 
      putStrLn e
      rest
    Right d -> do
      r <- rest
      return ((d,f):r)
  where
  rest = parseXml xs

getDivisions :: Content Posn -> Maybe Int
getDivisions c = case getInt divisions of 
  Just i -> if i > 0 then Just i else Nothing
  Nothing -> Nothing
  where
  fdivisions = (keep /> tag "divisions" /> txt) `o` fattributes
  divisions = fdivisions c

getBeats :: Content Posn -> Maybe Int
getBeats c = case getInt beats of 
  Just i -> if i > 0 && i <= 16 then Just i else Nothing
  Nothing -> Nothing
  where
  ftime = (keep /> tag "time") `o` fattributes
  fbeats = (keep /> tag "beats" /> txt) `o` ftime
  beats = fbeats c

getBeatType :: Content Posn -> Maybe Int
getBeatType c = case getInt beat_type of 
  Just i -> if i > 0 && i <= 16 then Just i else Nothing
  Nothing -> Nothing
  where
  ftime = (keep /> tag "time") `o` fattributes
  fbeat_type = (keep /> tag "beat-type" /> txt) `o` ftime
  beat_type = fbeat_type c

getFifths :: Content Posn -> Maybe Int
getFifths c = case getInt fifths of 
  Just i -> if i >= -7 && i <= 7 then Just i else Nothing
  Nothing -> Nothing
  where
  fkey = (keep /> tag "key") `o` fattributes
  ffifths = (keep /> tag "fifths" /> txt) `o` fkey
  fifths = ffifths c

getInt :: [Content Posn] -> Maybe Int
getInt [CString _ s _] = readInt s
getInt _ = Nothing

fattributes :: Content i -> [Content i]
fattributes = keep /> tag "attributes"
  
readInt :: String -> Maybe Int
readInt s = case r of
  [(n,_)] -> Just n
  _ -> Nothing
  where 
  r = reads s
    
getMode :: Content Posn -> Maybe Bool
getMode c = case s of
  Just m ->
    if m == "major" then Just True
    else if m == "minor" then Just False
      else Nothing
  Nothing -> Nothing
  where
  fkey = (keep /> tag "key") `o` fattributes
  fmode = (keep /> tag "mode" /> txt) `o` fkey 
  mode = fmode c
  s = getString mode

getString :: [Content Posn] -> Maybe String
getString [CString _ s _] = Just s
getString _ = Nothing

parseDocument :: (Document Posn, String) -> Either String ([Event], String)
parseDocument (d,s)
  | (divisions == Nothing) = Left (s ++ ": error reading divisions")
  | (fifths == Nothing) = Left (s ++ ": error reading fifths")
  | (mode == Nothing) = Left (s ++ ": error reading mode")
  | (beats == Nothing) = Left (s ++ ": error reading beats")
  | (beat_type == Nothing) = Left (s ++ ": error reading beat-type")
  | otherwise = either (\e -> Left (s ++ ": " ++ e)) (\_ -> Right (events', s)) t
  where
  (t, events) = mapAccumL (parseMeasure divisions' fifths' mode' timesig) (Right 0) measures
  events' = filter (not.isNullEvent) $ concat events
  content = docContent pos d
  pos = posInNewCxt s Nothing
  [firstpart] = ffirstpart content
  divisions = getDivisions firstmeasure
  divisions' = fromJust divisions
  fifths = getFifths firstmeasure
  fifths' = fromJust fifths
  mode = getMode firstmeasure
  mode' = fromJust mode
  beats = getBeats firstmeasure
  beats' = fromJust beats
  beat_type = getBeatType firstmeasure
  beat_type' = fromJust beat_type
  timesig = beats' * 16 `div` beat_type'
  measures = fmeasures firstpart
  fmeasures = keep /> tag "measure"
  fparts = numbered (keep /> tag "part")
  ffirst 1 = keep
  ffirst _ = none
  ffirstpart = (ffirst `oo` fparts)
  fnumberedmeasures = numbered (keep /> tag "measure")
  ffirstmeasure = (ffirst `oo` fnumberedmeasures)
  [firstmeasure] = ffirstmeasure firstpart
  
  
  
  
  
  
