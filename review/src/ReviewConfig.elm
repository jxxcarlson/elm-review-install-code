module ReviewConfig exposing (config)

import Install.AddToType
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ Install.AddToType.makeAddToTypeRule "FrontendMsg" "Bar"
    , Install.AddToType.makeAddToTypeRule "FrontendMsg" "Blatzo Int Int"
    ]