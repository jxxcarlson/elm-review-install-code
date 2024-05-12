module ReviewConfig exposing (config)

import MagicToken.AddToType
import MagicToken.AddToTypeAlias
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ -- MagicToken.AddToType.makeAddToTypeRule "FrontendMsg" "Bar"
      -- , MagicToken.AddToType.makeAddToTypeRule "FrontendMsg" "Blatzo Int Int"
      MagicToken.AddToTypeAlias.makeAddToTypeAliasRule "FrontendModel" "quot: String"
    ]
