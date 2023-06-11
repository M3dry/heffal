module Main where

import Heffal.Lexer
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Runners.Console (defaultMain)

tests =
    testGroup "lexer"
        [ testCase
            "general lexing" $
                strToTokens "# heading\n[x] todo complete\n- bullet - /*bold+*italic/" @?=
                [ Heading, Text [Pure "heading"], Newline
                , TodoOpen, TodoState "x", TodoClose, Text [Pure "todo complete"], Newline
                , Bullet, Text [Pure "bullet ", Pure "- ", Italic [Bold [Pure "bold+"], Pure "italic"]]
                ]
        , testCase
            "text lexing" $
                strToTokens "# heading\nhello `this is verbatim` _underlined_, this is -crossed out-" @?=
                [ Heading, Text [Pure "heading"], Newline
                , Text [Pure "hello ", Verbatim [Pure "this is verbatim"], Pure " ", Underline [Pure "underlined"], Pure ", this is ", Crossed [Pure "crossed out"]]
                ]
        ]

main = defaultMain [tests]
