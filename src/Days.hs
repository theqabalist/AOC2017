module Days (
    days
) where

import Lib (dualTextAdapter, dualTextAdapter')
import qualified Days.Day1 as D1
import qualified Days.Day2 as D2
import qualified Days.Day3 as D3
import qualified Days.Day4 as D4
import qualified Days.Day5 as D5
import qualified Days.Day6 as D6
import qualified Days.Day7 as D7
import qualified Days.Day8 as D8
import qualified Days.Day9 as D9
import qualified Days.Day10 as D10
import qualified Days.Day11 as D11
import qualified Days.Day12 as D12
import qualified Days.Day13 as D13
import qualified Days.Day14 as D14
import qualified Days.Day15 as D15
import qualified Days.Day16 as D16
import qualified Days.Day17 as D17
import qualified Days.Day18 as D18
import qualified Days.Day19 as D19
import qualified Days.Day20 as D20
import qualified Days.Day21 as D21
import qualified Days.Day22 as D22
import Data.Text (Text)

days :: [Text -> Text]
days = [
    dualTextAdapter D1.partOne D1.partTwo,
    dualTextAdapter D2.partOne D2.partTwo,
    dualTextAdapter D3.partOne D3.partTwo,
    dualTextAdapter D4.partOne D4.partTwo,
    dualTextAdapter D5.partOne D5.partTwo,
    dualTextAdapter D6.partOne D6.partTwo,
    dualTextAdapter D7.partOne D7.partTwo,
    dualTextAdapter D8.partOne D8.partTwo,
    dualTextAdapter D9.partOne D9.partTwo,
    dualTextAdapter D10.partOne D10.partTwo,
    dualTextAdapter D11.partOne D11.partTwo,
    dualTextAdapter D12.partOne D12.partTwo,
    dualTextAdapter D13.partOne D13.partTwo,
    dualTextAdapter D14.partOne D14.partTwo,
    dualTextAdapter D15.partOne D15.partTwo,
    dualTextAdapter D16.partOne D16.partTwo,
    dualTextAdapter D17.partOne D17.partTwo,
    dualTextAdapter D18.partOne D18.partTwo,
    dualTextAdapter' D19.partOne D19.partTwo,
    dualTextAdapter D20.partOne D20.partTwo,
    dualTextAdapter D21.partOne D21.partTwo,
    dualTextAdapter D22.partOne D22.partTwo
 ]
