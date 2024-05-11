module ReviewConfig exposing (config)

import MagicToken.AddToType
import Review.Rule exposing (Rule)


rule1 : Rule
rule1 = MagicToken.AddToType.makeAddToTypeRule "FrontendMsg" "Bar"

config : List Rule
config =
    [ rule1
    ]
