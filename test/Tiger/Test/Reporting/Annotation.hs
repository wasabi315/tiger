{-# LANGUAGE BlockArguments #-}

module Tiger.Test.Reporting.Annotation where

--------------------------------------------------------------------------------

import           Test.Tasty
import           Test.Tasty.HUnit

import           Tiger.Reporting.Annotation

--------------------------------------------------------------------------------

test_Region :: TestTree
test_Region =
    testGroup "Region"
        [ testCase "Region.(<>)" do
            Region (pos 1) (pos 2) <> Region (pos 2) (pos 4)
                @?= Region (pos 1) (pos 4)

            Region (pos 1) (pos 3) <> Region (pos 2) (pos 4)
                @?= Region (pos 1) (pos 4)

            Region (pos 2) (pos 3) <> Region (pos 1) (pos 4)
                @?= Region (pos 1) (pos 4)
        ]
