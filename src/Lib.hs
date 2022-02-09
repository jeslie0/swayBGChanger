module Lib
  (Argument(..),
   Arguments,
   Mode(..),
   arguments)
where

import Control.Applicative ( Alternative((<|>)) )
import Parser ( Parser, natural, symbol, word )


-- * Command Line arguments

-- We need a way to deal with command line arguments, that can then be
-- fed into the programme.

-- * Data Types

-- | These are the command line argument options for swaybgchanger.
data Argument
  = Time Int -- Duration image is shown for.
  | Directory FilePath -- Directory of images to be used.
  | Colour String -- Fixed colour for background.
  | Help -- Help menu.
  | Image FilePath -- Fixed image for background.
  | ModeOption Mode -- The option to be applied to each image.
  | Output String -- The output to configure. If blank, then applied
  -- to all outputs.
  | Version -- Displays the version number and quits.
  deriving (Show, Eq)

type Arguments = [Argument]

data Mode
  = Stretch
  | Fill
  | Fit
  | Centre
  | Tile
  deriving (Show, Eq)

-- * Parsing the arguments

argument :: Parser Argument
argument = foldl (<|>) time [directory, colour, help, image, modeOption, output, version]

arguments :: Parser Arguments
arguments = do
  fstArg <- argument
  remainingArgs <- arguments <|> return []
  return $ fstArg:remainingArgs

  
-- ** Time

-- | Format: -t <10>, or --time=<time>.
time :: Parser Argument
time = do
  symbol "-t" <|> symbol "--time="
  Time <$> natural

-- ** Directory

-- | Rudimentary as filepaths with spaces in will break. Format: -f
-- <path>, or --directory=<path>.
directory :: Parser Argument
directory = do
  symbol "-d" <|> symbol "--directory="
  Directory <$> word

-- ** Colour
-- | Format: -c <colour>, --colour=<colour> or --color=<colour>
colour :: Parser Argument
colour = do
  symbol "-c" <|> symbol "--colour=" <|> symbol "--color="
  Colour <$> word

-- ** Help

-- | Format: -h, or --help
help :: Parser Argument
help = do
  symbol "-h" <|> symbol "--help"
  return Help

-- ** Image
-- | Format: -i <path>, or --image=<path>
image :: Parser Argument
image = do
  symbol "-i" <|> symbol "--image="
  Image <$> word

-- ** ModeOption
-- | Format: -m <mode>, or --mode=<mode>
modeOption :: Parser Argument
modeOption = do
  symbol "-m" <|> symbol "--mode="
  ModeOption <$> mode

-- ** Output
-- | Format: -o <output>, or --output=<output>
output :: Parser Argument
output = do
  symbol "-o" <|> symbol "--output="
  Output <$> word

-- ** Version

-- | Format: -v, or --version
version :: Parser Argument
version = do
  symbol "-v" <|> symbol "--version"
  return Version

-- * Parsing the modes

stretch :: Parser Mode
stretch = do
  symbol "stretch"
  return Stretch

fill :: Parser Mode
fill = do
  symbol "fill"
  return Fill

fit :: Parser Mode
fit = do
  symbol "fit"
  return Fit

centre :: Parser Mode
centre = do
  symbol "centre" <|> symbol "center"
  return Centre

tile :: Parser Mode
tile = do
  symbol "tile"
  return Tile

mode :: Parser Mode
mode = foldl (<|>) stretch [fill, fit, centre, tile]
