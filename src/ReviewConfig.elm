module ReviewConfig exposing (config)

import MagicToken.AddToType
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ MagicToken.AddToType.makeAddToTypeRule "FrontendMsg" "Bar" "\n  | Bar"
    ]
