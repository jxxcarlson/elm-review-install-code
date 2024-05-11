module ReviewConfig exposing (config)

import MagicToken.AddToTypes
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ MagicToken.AddToTypes.rule
    -- other rules...
    ]