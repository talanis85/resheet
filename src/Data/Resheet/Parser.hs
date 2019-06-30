{-# LANGUAGE FlexibleContexts #-}
module Data.Resheet.Parser
  ( parseSong
  ) where

import           Data.Ratio
import qualified Data.Text as T
import           Text.Parsec

import           Data.Resheet
import           Data.Music.Chords hiding (Chord, Chord (Chord))
import           Data.Music.Scales
import           Data.Music.Tonal

type Parser = Parsec String Time

parseSong name s = runParser (pSong <* eof) (Time 4 4) name s

tok :: Parser a -> Parser a
tok p = do
  let barline = try (char '|' >> notFollowedBy (char ':'))
  let space' = space <|> (barline >> return ' ') <|> try (pComment *> return ' ')
  _ <- many space'
  r <- p
  _ <- many space'
  return r

pComment :: Parser String
pComment = do
  _ <- string "--"
  anyChar `manyTill` newline

pSong :: Parser Song
pSong = do
  meta <- pMeta
  sheet <- pSheet
  return $ Song meta sheet

pMeta :: Parser Meta
pMeta = do
  title <- option "" (tok (string "title") *> (anyChar `manyTill` newline))
  composer <- option "" (tok (string "composer") *> (anyChar `manyTill` newline))

  return Meta
    { metaTitle = T.pack title
    , metaComposer = T.pack composer
    }

pSheet :: Parser Sheet
pSheet = many1 pNotation

pNotation :: Parser Notation
pNotation = choice
  [ try pTimeSignature
  , try pRehearsalMark
  , try pChord
  , try pLineBreak
  , try pRepeatOpen
  , try pRepeatClose
  ]

pRepeatOpen :: Parser Notation
pRepeatOpen = do
  _ <- tok (string "|:")
  return RepeatOpen

pRepeatClose :: Parser Notation
pRepeatClose = do
  _ <- tok (string ":|")
  return RepeatClose

pLineBreak :: Parser Notation
pLineBreak = do
  _ <- tok (string "\\\\")
  return LineBreak

pTimeSignature :: Parser Notation
pTimeSignature = do
  tok (string "time")
  time <- tok pTime
  putState time
  return (TimeSignature time)

pTime :: Parser Time
pTime = do
  a <- read <$> many1 digit
  _ <- char '/'
  b <- read <$> many1 digit
  return (Time a b)

pRehearsalMark :: Parser Notation
pRehearsalMark = do
  _ <- tok $ char '<'
  t <- anyChar `manyTill` (try (char '>'))
  return (RehearsalMark (T.pack t))

pChord :: Parser Notation
pChord = do
  dur <- tok pDuration
  crd <- tok absChord
  tie <- option False (tok (string "~") *> return True)
  voicing <- pVoicing
  return (Chord dur crd voicing tie)

pVoicing :: Parser Voicing
pVoicing = choice
  [ try pVoicingPercent
  , try pVoicingLily
  , return VoicingRest
  ]

pVoicingPercent :: Parser Voicing
pVoicingPercent = do
  _ <- tok (char '%')
  return VoicingPercent

pVoicingLily :: Parser Voicing
pVoicingLily = do
  _ <- tok (char '[')
  t <- anyChar `manyTill` (try (tok (char ']')))
  return $ VoicingLily $ T.pack t

pDuration :: Parser Duration
pDuration = choice [ try pLongDuration, pShortDuration ]
  where
    pLongDuration = do
      a <- read <$> many1 digit
      _ <- char '/'
      b <- read <$> many1 digit
      return (a % b)
    pShortDuration = do
      a <- read <$> many1 digit
      Time _ b <- getState
      return (a % b)

absChord = do
  base <- chordBase
  modifier <- chordMod
  t <- chordType >>= chordModifiers
  slash <- option (Degree I natural) $ do
    char '/'
    slBase <- chordBase
    slModifier <- chordMod
    return $ degreeOf (TonalScale (Pitch base modifier) (chordScale t)) (Pitch slBase slModifier)
  return $ TonalChord (Pitch base modifier) (t </> slash)

chordBase = do
    n <- oneOf "ABCDEFG"
    case n of
        'A' -> return A
        'B' -> return B
        'C' -> return C
        'D' -> return D
        'E' -> return E
        'F' -> return F
        'G' -> return G
        _   -> fail $ "Unexpected chord name: " ++ (show n)

chordMod = do
    m <- option ' ' (try $ oneOf "b#")
    case m of
        ' ' -> return natural
        '#' -> return sharp
        'b' -> return flat
        _   -> fail $ "Unexpected modifier: " ++ (show m)

majorMinor :: Parser Scale
majorMinor = option ionianScale (try $ char 'm' >> return aeolianScale)

chordType = choice
    [ try (string "maj7") >> return major7Chord
    , try (string "mmaj7") >> return minorMajor7Chord
    , try (string "maj9") >> return major9Chord
    , try (string "7") >> return dominant7Chord
    , try (string "9") >> return dominant9Chord
    , try (string "69") >> return major69Chord
    , try (string "6") >> return major6Chord
    , try (string "m69") >> return minor69Chord
    , try (string "m6") >> return minor6Chord
    , try (string "m7") >> return minor7Chord
    , try (string "m9") >> return minor9Chord
    , try (string "m") >> return minorChord
    , try (string "dim") >> return diminishedChord
    , try (string "+") >> return augmentedChord
    , try (string "sus7") >> return sus7Chord
    , return majorChord
    ]

chordModifiers c =
    (choice
        [ try (string "sus2") >> (return $ applySus2 c)
        , try (string "sus4") >> (return $ applySus4 c)
        , try (string "sus") >> (return $ applySus c)
        , try (string "b5") >> (return $ c `without` V `with` Degree V flat)
        , try (string "5") >> (return $ c `without` III)
        , try (string "#5") >> (return $ c `without` V `with` Degree V sharp)
        , try (string "b9") >> (return $ c `with` Degree II flat)
        , try (string "#9") >> (return $ c `with` Degree II sharp)
        , try (string "add9") >> (return $ c `with` Degree II natural)
        , try (string "9") >> (return $ c `with` Degree VII flat `with` Degree II natural)
        , try (string "7") >> (return $ c `with` Degree VII flat)
        , try (string "maj7") >> (return $ c `with` Degree VII natural)
        , try (string "6") >> (return $ c `with` Degree VI natural)
        , try (string "13") >> (return $ c `with` Degree VII flat `with` Degree VI natural)
        , try (string "b13") >> (return $ c `with` Degree VI flat)
        , try (string "#11") >> (return $ c `with` Degree IV sharp)
        , try (string "11") >> (return $ c `with` Degree IV natural)
        ]
        >>= chordModifiers)
    <|>
    (return c)
