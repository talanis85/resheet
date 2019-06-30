module Data.Resheet where

import           Control.Monad.State
import           Data.Ratio
import           Data.Char (toLower)
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)
import qualified Data.Music.Chords as Chords
import qualified Data.Music.Scales as Scales
import           Data.Music.Scales (Degree (..), DegreeBase (..), natural, flat, sharp)
import           Data.Music.Tonal
import qualified Data.Set as Set
import           Text.Printf

data Song = Song Meta Sheet
data Meta = Meta
  { metaTitle :: T.Text
  , metaComposer :: T.Text
  , metaSize :: Integer
  }
type Sheet = [Notation]
type Duration = Rational
type HasTie = Bool
data Time = Time Integer Integer
  deriving (Show)

data Voicing
  = VoicingLily T.Text
  | VoicingPercent
  | VoicingRest
  deriving (Show)

data Notation
  = TimeSignature Time
  | KeySignature Pitch
  | RehearsalMark T.Text
  | ExpressionMark T.Text
  | Chord Duration TonalChord Voicing HasTie
  | LineBreak
  | RepeatOpen
  | RepeatClose
  deriving (Show)

data LilySheet = LilySheet
  { lsChords :: T.Text
  , lsVoice :: T.Text
  }

instance Semigroup LilySheet where
  a <> b = LilySheet
    { lsChords = lsChords a <> " " <> lsChords b
    , lsVoice = lsVoice a <> " " <> lsVoice b
    }

instance Monoid LilySheet where
  mempty = LilySheet
    { lsChords = mempty
    , lsVoice = mempty
    }

notationToLilySheet :: Notation -> State (Maybe TonalChord) LilySheet
notationToLilySheet n = case n of
  TimeSignature t -> return $ LilySheet
    { lsChords = ""
    , lsVoice = "\\time " <> formatLilyTime t
    }
  KeySignature k -> return $ LilySheet
    { lsChords = T.pack $ printf "\\key %s \\major" (formatLilyPitch k)
    , lsVoice = T.pack $ printf "\\key %s \\major" (formatLilyPitch k)
    }
  RehearsalMark m -> return $ LilySheet
    { lsChords = ""
    , lsVoice = T.pack $ printf "\\mark \\markup { \\box \\bold \"%s\" } " m
    -- this line fixes vim's syntax highlighting "
    }
  ExpressionMark m -> return $ LilySheet
    { lsChords = "\\cadenzaOn s16 \\cadenzaOff"
    , lsVoice = T.pack $ printf "\\cadenzaOn s16^\\markup { \\italic \"%s\" } \\cadenzaOff " m
    -- this line fixes vim's syntax highlighting "
    }
  Chord dur chord voicing hasTie -> do
    lastChord <- get
    put (Just chord)
    return $ LilySheet
      { lsChords = if fmap show lastChord == Just (show chord)
          then T.pack $ printf "s%s" (formatLilyDuration dur)
          else formatLilyChord dur chord
      , lsVoice = case voicing of
          VoicingRest -> T.pack $ printf "c%s%s" (formatLilyDuration dur) (formatLilyTie hasTie)
          VoicingLily lily -> T.pack $ printf "\\improvisationOff %s \\improvisationOn" lily
          VoicingPercent -> T.pack $ printf "\\makePercent r%s" (formatLilyDuration dur)
      }
  LineBreak -> return $ LilySheet
    { lsChords = ""
    , lsVoice = "\\break"
    }
  RepeatOpen -> do
    put Nothing
    return $ LilySheet
      { lsChords = "\\bar \"[|:\""
      , lsVoice = "\\bar \"[|:\""
      }
  RepeatClose -> do
    put Nothing
    return $ LilySheet
      { lsChords = "\\bar \":|]\""
      , lsVoice = "\\bar \":|]\""
      }

formatLilyChord :: Duration -> TonalChord -> T.Text
formatLilyChord dur chord = T.pack $
  printf "%s%s:%s%s"
         (formatLilyPitch (tchordRoot chord))
         (formatLilyDuration dur)
         (formatLilyChordType (tchordChord chord))
         (formatLilyChordSlash chord)

formatLilyChordSlash :: TonalChord -> T.Text
formatLilyChordSlash tchord =
  let chord = tchordChord tchord
  in if Chords.chordSlash chord == Degree I natural
        then ""
        else "/" <> formatLilyPitch (scaleDegree (TonalScale (tchordRoot tchord) (Chords.chordScale chord)) (Chords.chordSlash chord))

formatLilyTie :: HasTie -> T.Text
formatLilyTie t = if t then "~" else ""

outputSong :: Song -> T.Text
outputSong (Song meta sheet) =
  let lilySheet = mconcat $ evalState (sequence (map notationToLilySheet sheet)) Nothing
  in T.pack $ printf template (metaSize meta) (metaTitle meta) (metaComposer meta) (lsChords lilySheet) (lsVoice lilySheet)

formatLilyTime :: Time -> T.Text
formatLilyTime (Time a b) = T.pack $ printf "%d/%d" a b

timeDuration :: Time -> Duration
timeDuration (Time a b) = a % b

formatLilyDuration :: Duration -> T.Text
formatLilyDuration t
  | numerator t == 1 = T.pack $ show (denominator t)
  | numerator t == 3 && denominator t > 1 = T.pack $ show (denominator t `div` 2) <> "."
  | otherwise = ""

template :: String
template = "\
  \ #(set-global-staff-size %d) \n \
  \  \n \
  \ \\header { \n \
  \   title = \"%s\" \n \
  \   composer = \"%s\" \n \
  \   tagline = ##f \n \
  \ } \n \
  \  \n \
  \ makePercent = \n \
  \ #(define-music-function (parser location note) (ly:music?) \n \
  \    \"Make a percent repeat the same length as NOTE.\" \n \
  \    (make-music 'PercentEvent \n \
  \                'length (ly:music-length note))) \n \
  \  \n \
  \ \\layout { \n \
  \   \\context { \n \
  \     \\Score \n \
  \     \\override RehearsalMark.self-alignment-X = #LEFT \n \
  \   } \n \
  \ } \n \
  \ \\score { \n \
  \  \n \
  \ << \n \
  \   \\new ChordNames { \n \
  \     \\chordmode { \n \
  \       %s \n \
  \     } \n \
  \   } \n \
  \  \n \
  \   \\new Voice \\with { \n \
  \     \\consists \"Pitch_squash_engraver\" \n \
  \   } { \n \
  \     \\improvisationOn \n \
  \     %s \n \
  \   } \n \
  \ >> \n \
  \  \n \
  \ } \n \
  \ "

formatLilyChordType :: Chords.Chord -> T.Text
formatLilyChordType c =
  let degrees = Set.toAscList (Chords.chordDegrees c)
  in fromMaybe "" (lookup degrees lilyChordTypes)

lilyChordTypes =
  [ ( [Degree I natural, Degree III natural, Degree V natural], "" )
  , ( [Degree I natural, Degree III flat, Degree V natural], "m" )
  , ( [Degree I natural, Degree III natural, Degree V natural, Degree VII natural], "maj7" )
  , ( [Degree I natural, Degree III flat, Degree V natural, Degree VII flat], "m7" )
  , ( [Degree I natural, Degree III flat, Degree V natural, Degree VII natural], "m7+" )
  , ( [Degree I natural, Degree III flat, Degree V flat, Degree VII natural], "m7b5" )
  , ( [Degree I natural, Degree III flat, Degree V flat], "dim" )
  , ( [Degree I natural, Degree III flat, Degree V flat, Degree VII flat], "dim" )
  ]

formatLilyPitch :: Pitch -> T.Text
formatLilyPitch (Pitch base acc) = formatBase base <> formatAcc acc
  where
    formatBase x = T.pack $ map toLower (show x)
    formatAcc x = mconcat $ map transAcc (show x)
    transAcc '#' = "is"
    transAcc 'b' = "es"
    transAcc _ = ""
