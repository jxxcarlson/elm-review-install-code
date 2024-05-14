module ReviewConfig exposing (config)

import Install.ClauseInCase
import Install.FieldInTypeAlias
import Install.Initializer
import Install.TypeVariant
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ Install.TypeVariant.makeRule "Types" "FrontendMsg" "Bar"
    , Install.TypeVariant.makeRule "Types" "ToBackend" "ResetCounter"
    , Install.FieldInTypeAlias.makeRule "Types" "FrontendModel" "quot: String"
    , Install.Initializer.makeRule "Backend" "init" "message" "\"hohoho!\""
    , Install.ClauseInCase.makeRule
        "Backend"
        "updateFromFrontend"
        "ResetCounter"
        "( { model | counter = 0 }, broadcast (CounterNewValue 0 clientId) )"
    ]
