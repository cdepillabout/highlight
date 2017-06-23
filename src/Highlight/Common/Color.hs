{-# LANGUAGE OverloadedStrings #-}

module Highlight.Common.Color where

import Prelude ()
import Prelude.Compat

import Data.ByteString.Char8 (ByteString, pack)
import Data.IntMap.Strict (IntMap, (!), fromList)
import Data.Monoid ((<>))
import System.Console.ANSI
       (Color(..), ColorIntensity(..), ConsoleIntensity(..),
        ConsoleLayer(..), SGR(..), setSGRCode)

------------------------
-- Application Colors --
------------------------

colorForFileNumber :: Int -> ByteString
colorForFileNumber num = allColorsMap ! (num `mod` allColorsLength)

allColorsMap :: IntMap ByteString
allColorsMap = fromList $ zip [0..] allColorsList

allColorsLength :: Int
allColorsLength = length allColorsList

allColorsList :: [ByteString]
allColorsList =
  [ colorVividBlueBold
  , colorVividGreenBold
  , colorVividCyanBold
  , colorVividMagentaBold
  ]

replaceInRedByteString :: ByteString
replaceInRedByteString = colorVividRedBold <> "$0" <> colorReset


-----------------------
-- Vivid Bold Colors --
-----------------------

colorVividBlackBold :: ByteString
colorVividBlackBold = colorBold `mappend` colorVividBlack

colorVividBlueBold :: ByteString
colorVividBlueBold = colorBold `mappend` colorVividBlue

colorVividCyanBold :: ByteString
colorVividCyanBold = colorBold `mappend` colorVividCyan

colorVividGreenBold :: ByteString
colorVividGreenBold = colorBold `mappend` colorVividGreen

colorVividMagentaBold :: ByteString
colorVividMagentaBold = colorBold `mappend` colorVividMagenta

colorVividRedBold :: ByteString
colorVividRedBold = colorBold `mappend` colorVividRed

colorVividWhiteBold :: ByteString
colorVividWhiteBold = colorBold `mappend` colorVividWhite

colorVividYellowBold :: ByteString
colorVividYellowBold = colorBold `mappend` colorVividYellow

-----------------------
-- Dull Bold Colors --
-----------------------

colorDullBlackBold :: ByteString
colorDullBlackBold = colorBold `mappend` colorDullBlack

colorDullBlueBold :: ByteString
colorDullBlueBold = colorBold `mappend` colorDullBlue

colorDullCyanBold :: ByteString
colorDullCyanBold = colorBold `mappend` colorDullCyan

colorDullGreenBold :: ByteString
colorDullGreenBold = colorBold `mappend` colorDullGreen

colorDullMagentaBold :: ByteString
colorDullMagentaBold = colorBold `mappend` colorDullMagenta

colorDullRedBold :: ByteString
colorDullRedBold = colorBold `mappend` colorDullRed

colorDullWhiteBold :: ByteString
colorDullWhiteBold = colorBold `mappend` colorDullWhite

colorDullYellowBold :: ByteString
colorDullYellowBold = colorBold `mappend` colorDullYellow

------------------
-- Vivid Colors --
------------------

colorVividBlack :: ByteString
colorVividBlack = colorHelper Vivid Black

colorVividBlue :: ByteString
colorVividBlue = colorHelper Vivid Blue

colorVividCyan :: ByteString
colorVividCyan = colorHelper Vivid Cyan

colorVividGreen :: ByteString
colorVividGreen = colorHelper Vivid Green

colorVividMagenta :: ByteString
colorVividMagenta = colorHelper Vivid Magenta

colorVividRed :: ByteString
colorVividRed = colorHelper Vivid Red

colorVividWhite :: ByteString
colorVividWhite = colorHelper Vivid White

colorVividYellow :: ByteString
colorVividYellow = colorHelper Vivid Yellow

------------------
-- Dull Colors --
------------------

colorDullBlack :: ByteString
colorDullBlack = colorHelper Dull Black

colorDullBlue :: ByteString
colorDullBlue = colorHelper Dull Blue

colorDullCyan :: ByteString
colorDullCyan = colorHelper Dull Cyan

colorDullGreen :: ByteString
colorDullGreen = colorHelper Dull Green

colorDullMagenta :: ByteString
colorDullMagenta = colorHelper Dull Magenta

colorDullRed :: ByteString
colorDullRed = colorHelper Dull Red

colorDullWhite :: ByteString
colorDullWhite = colorHelper Dull White

colorDullYellow :: ByteString
colorDullYellow = colorHelper Dull Yellow

--------------------
-- Special Colors --
--------------------

-- | Change the intensity to 'BoldIntensity'.
colorBold :: ByteString
colorBold = setSGRCodeBuilder [SetConsoleIntensity BoldIntensity]

-- | 'Reset' the console color back to normal.
colorReset :: ByteString
colorReset = setSGRCodeBuilder [Reset]

-- | Empty string.
colorNull :: ByteString
colorNull = ""

-------------
-- Helpers --
-------------

-- | Helper for creating a 'ByteString' for an ANSI escape sequence color based on
-- a 'ColorIntensity' and a 'Color'.
colorHelper :: ColorIntensity -> Color -> ByteString
colorHelper colorIntensity color =
  setSGRCodeBuilder [SetColor Foreground colorIntensity color]

-- | Convert a list of 'SGR' to a 'ByteString'.
setSGRCodeBuilder :: [SGR] -> ByteString
setSGRCodeBuilder = pack . setSGRCode
