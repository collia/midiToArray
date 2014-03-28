
-- Copyright (C) 2013 Nikolay Klimchuk
-- Progect https://github.com/collia/midiToArray
-- License: http://www.gnu.org/licenses/gpl.html GPL version 2 or higher


import qualified Sound.MIDI.File.Load                   as LoadMidi
import qualified Sound.MIDI.File                        as MidiFile
import qualified Sound.MIDI.File.Event                  as Event
import qualified Sound.MIDI.File.Event.Meta             as MetaEvent
import qualified Sound.MIDI.Message.Channel             as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice       as Voice
import qualified Haskore.Basic.Pitch                    as Pitch
import qualified Haskore.Basic.Tempo                    as Tempo

import qualified Data.EventList.Relative.TimeBody       as TimeList
import qualified Sound.Honk                             as Honk
import Control.Monad.State
import GHC.Float
import Data.Ratio
import qualified Data.Text                              as Text

import System.Environment                   
import Data.Char
import System.IO
import System.Console.GetOpt
import Data.Maybe ( fromMaybe )


-- Tempo, PPQN, time offset
type MidiFileState = (Integer, Integer, Integer)
-- Time fro prev mks, Freq, Note
type NotesArrayElem = (Integer, Float, Integer)

parsePitch :: Int -> Int -> [NotesArrayElem] ->State MidiFileState [NotesArrayElem]
parsePitch pitch 0 line = do
                   (tempo, ppqn, counter) <- get
                   put (tempo, ppqn, 0)
                   return (line ++ [(((counter * (tempo `div` ppqn)) ),0 , toInteger(pitch))])
parsePitch pitch x line = do
                   (tempo, ppqn, counter) <- get
                   put (tempo, ppqn, 0)
                   return (line ++ [(((counter * (tempo `div` ppqn)) ), ( Pitch.intToFreq pitch), toInteger(pitch))])


parseNote :: Voice.T -> [NotesArrayElem] -> State MidiFileState [NotesArrayElem]
parseNote (Voice.NoteOff pitch velocity) line         = parsePitch (Voice.fromPitch pitch) (Voice.fromVelocity velocity) (line)
parseNote (Voice.NoteOn  pitch velocity) line         = parsePitch (Voice.fromPitch pitch) (Voice.fromVelocity velocity) (line)
parseNote (Voice.PolyAftertouch pitch pressure) line  = return (line)	 
parseNote (Voice.ProgramChange program)	 line         = return (line)
parseNote (Voice.Control t controllerValue)  line     = return (line)
parseNote (Voice.PitchBend pitchBendRange)  line      = return (line)
parseNote (Voice.MonoAftertouch pressure)  line       = return (line)

parseMessage :: ChannelMsg.T -> [NotesArrayElem] -> State MidiFileState [NotesArrayElem]
parseMessage  (ChannelMsg.Cons channel body) line = 
              case body of
                   ChannelMsg.Voice v -> parseNote v line
                   ChannelMsg.Mode  m -> return (line)

parseMetaEvent :: MetaEvent.T -> [NotesArrayElem] -> State MidiFileState [NotesArrayElem]
parseMetaEvent (MetaEvent.SetTempo tempo) line = do
                                   (t, ppqn, counter) <- get
                                   put (toInteger(MetaEvent.fromTempo tempo), ppqn, counter)
                                   return (line)
parseMetaEvent _ line = return line


parseEvent ::  Event.T -> [NotesArrayElem] -> State MidiFileState [NotesArrayElem]
parseEvent (Event.MIDIEvent e) line =  parseMessage e (line )
parseEvent (Event.MetaEvent e) line = parseMetaEvent e line
parseEvent (Event.SystemExclusive e) line = return (line)


parseTimeList :: [NotesArrayElem] -> (MetaEvent.ElapsedTime, Event.T) -> State MidiFileState [NotesArrayElem]
parseTimeList line (time, body) = do
                                (tempo, ppqn, counter) <- get
                                put (tempo, ppqn, (counter + (MetaEvent.fromElapsedTime time)))
                                parseEvent body line


parseTrack :: [NotesArrayElem] -> MidiFile.Track -> State MidiFileState [NotesArrayElem]
parseTrack line track =  foldM parseTimeList line  (TimeList.toPairList track)
--                        return (line ++ " ")



parseMIDIFile :: MidiFile.T -> State MidiFileState [NotesArrayElem]
parseMIDIFile (MidiFile.Cons _ division tracks) =  do
                               (tempo, ppqn, counter) <- get
                               put (tempo, toInteger(MidiFile.fromTempo(MidiFile.ticksPerQuarterNote division)), counter)
                               (foldM parseTrack ([]) tracks)

changeOffsetToDelay :: [NotesArrayElem] -> [NotesArrayElem]
changeOffsetToDelay []  = []
changeOffsetToDelay (a0:a1:as) = let (off0, freq0, note0) = a0
                                     (off1, freq1, note1) = a1 in
                               [(off1, freq0, note0)] ++ changeOffsetToDelay ([a1] ++ as)
changeOffsetToDelay [a]  = [a]

playArray :: [NotesArrayElem] -> IO ()
playArray [] = putStr ""
playArray (a:as) = let (delay, freq, note) = a in
                 do 
                        Honk.playOne (Honk.Note (delay % 1000000) (float2Double freq /10))  
                        (playArray as)

printArray :: [NotesArrayElem] -> String
printArray [] = ""
printArray (a:as) = let (delay, freq, note) = a in
                    if (freq == 0) then
                        show delay ++ "mks, Note off = " ++ show note ++ "\n" ++
                        printArray as
                    else
                        show delay ++ "mks, Note = " ++ show note ++ "(" ++ show freq ++ "Hz) \n" ++
                        printArray as

generateCHeaderElement :: String -> NotesArrayElem -> String
generateCHeaderElement line (delay, freq, note) = line ++
                                             "{ " ++ 
                                             show ((delay `div` 1000) :: Integer) ++
                                             ", " ++
                                             show (ceiling freq) ++ 
                                             ", " ++ 
                                             show note ++
                                             "}, "


generateCHeaderElems :: [NotesArrayElem] -> String
generateCHeaderElems [] = ""
generateCHeaderElems a = foldl generateCHeaderElement "" (take 5 a) ++ "\\\n" ++
                              generateCHeaderElems (drop 5 a)


generateCHeader :: String ->[NotesArrayElem] -> Text.Text
--generateCHeader [] = ""
generateCHeader name a = Text.concat([
                  Text.pack ("// Content: delay in ms, frequency in Hz, MIDI note number \n" ++  
                    "// if freq == 0 - this meaning note off \n" ++
                  "#define " ),
                  Text.toUpper $ Text.replace (Text.pack(".")) (Text.pack("_")) $ Text.pack(name),
                  Text.pack("  { \\\n" ++ generateCHeaderElems a ++ "}\n")]);



data Flag =   
            Version 
          | COutput String 
          | TOutput String
          | Play
       deriving Show
    
options :: [OptDescr Flag]
options =
     [ 
       Option "v?" ["version"] (NoArg Version)        "show version number"
     , Option "C" ["Cout"]    (OptArg outpC "FILE")   "generate C header and output it in FILE"
     , Option "T" ["text"]    (OptArg outpT "FILE")   "generate text FILE"
     , Option "P" [""]        (NoArg Play)            "play music with speaker"
     ]
    
outpC, outpT :: Maybe String -> Flag
outpC = COutput . fromMaybe "stdout"
outpT = TOutput . fromMaybe "stdout"
--inp  = Input  . fromMaybe "stdin"
    
parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argv = 
       case getOpt Permute options argv of
--          (_,[],_  ) -> ioError (userError ("no input files " ++ usageInfo header options))
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage: midiToArray [OPTION...] files..."


generateCFile :: String -> IO String
generateCFile midifile = do
                            --y :: MidiFile.T
                            y <- (LoadMidi.fromFile midifile)
                            return (Text.unpack $ 
                               generateCHeader midifile $ 
                                 changeOffsetToDelay $ 
                                   (evalState (parseMIDIFile y ) (500000,120,0)))

doGenerateCFile :: [String] -> String -> IO()
doGenerateCFile midifiles "stdout" = do
                                     z <-  (sequence (map
                                                generateCFile
                                                midifiles))
                                     putStr (concat z)
                                     return ()
doGenerateCFile midifiles filename = do
                                     z <-  (sequence (map
                                                generateCFile
                                                midifiles))
                                     writeFile filename (concat z)
                                     return ()
                                     
generateTFile :: String -> IO String
generateTFile midifile = do
                            y <- (LoadMidi.fromFile midifile)
                            return (printArray $ 
                                   changeOffsetToDelay $ 
                                       (evalState (parseMIDIFile y) (500000,120,0)))

doGenerateTFile :: [String] -> String -> IO()
doGenerateTFile midifiles "stdout" = do
                                     z <-  (sequence (map
                                                generateTFile
                                                midifiles))
                                     putStr (concat z)
                                     return ()         
doGenerateTFile midifiles filename = do
                                     z <-  (sequence (map
                                                generateTFile
                                                midifiles))
                                     writeFile filename (concat z)
                                     return ()                              


playMidiFile :: String -> IO ()
playMidiFile midifile = do
                            y <- (LoadMidi.fromFile midifile)
                            (playArray $ changeOffsetToDelay $ (evalState (parseMIDIFile y) (500000,120,0)))
                            return ()

doPlayMidi :: [String] -> IO()
doPlayMidi midifiles = do
                          sequence (map
                                    playMidiFile
                                    midifiles)
                          return ()
                                                

doWork:: [String] -> [Flag] -> IO()
doWork  _  []  =  putStrLn "Finish"
doWork  files (flag:other) = case flag of
                           (Version)        -> putStrLn "midiToArray v0.1"
                           (COutput cfile)  -> if (null files) then
                                                  putStrLn "Nothing to do"
                                               else
                                                        do
                                                         putStrLn ("C to file " ++ cfile)
                                                         doGenerateCFile files cfile
                                                         doWork files other
                           (TOutput cfile)  -> if (null files) then
                                                  putStrLn "Nothing to do"
                                               else
                                                        do
                                                         putStrLn ("Text to file " ++ cfile)
                                                         doGenerateTFile files cfile
                                                         doWork files other
                           (Play)  -> if (null files) then
                                                  putStrLn "Nothing to do"
                                               else
                                                        do
                                                         putStrLn ("Play music")
                                                         doPlayMidi files
                                                         doWork files other


main = do
          (as, fs) <- getArgs >>= parseArgs
          doWork 
                 fs
                 as
 
