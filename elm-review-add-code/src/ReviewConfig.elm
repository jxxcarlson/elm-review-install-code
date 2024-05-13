module ReviewConfig exposing (config)

import MagicToken.AddToCaseStatementInFunction
import MagicToken.AddToType
import MagicToken.AddToTypeAlias
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ -- MagicToken.AddToType.makeAddToTypeRule "FrontendMsg" "Bar"
      MagicToken.AddToType.makeAddToTypeRule "ToBackend" "ResetCounter"

    -- MagicToken.AddToTypeAlias.makeAddToTypeAliasRule "FrontendModel" "quot: String"
    , MagicToken.AddToCaseStatementInFunction.makeAddToCaseStatementInFunctionRule
        "Backend"
        "updateFromFrontend"
        "ResetCounter"
        "( { model | counter = 0 }, broadcast (CounterNewValue 0 clientId) )"
    ]
