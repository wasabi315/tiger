{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}

module Tiger.Test.Syntax.Parse.Unescape where

--------------------------------------------------------------------------------

import           Data.Either
import           Test.Tasty
import           Test.Tasty.HUnit

import           Tiger.Syntax.Parse
import           Tiger.Syntax.Parse.Unescape

--------------------------------------------------------------------------------

test_str :: TestTree
test_str =
    testGroup "str"
        [ testCase "parse \"abc\"" do
            parse str "" "\"abc\""
                @?= Right "abc"

        , testCase "parse \"Hello, world!\\n\"" do
            parse str "" "\"Hello, world!\\n\""
                @?= Right "Hello, world!\n"

        , testCase "parse \"Foo\\ \n \t \f  \\Bar\"" do
            parse str "" "\"Foo\\ \n \t \f  \\Bar\""
                @?= Right "FooBar"

        , testCase "parse \"He said \\\"Hello.\\\"\"" do
            parse str "" "\"He said \\\"Hello.\\\"\""
                @?= Right "He said \"Hello.\""

        , testCase "parse \"\\088\\089\\090\"" do
            parse str "" "\"\\088\\089\\090\""
                @?= Right "\088\089\090"

        , testCase "parse \"\\0888\"" do
            parse str "" "\"\\0888\""
                @?= Right "X8"

        , testCase "parse \"\\088\\189\\090\" should fail" do
            isLeft (parse str "" "\"\\088\\189\\090\"")
                @? "invalid character parsed"
        ]
