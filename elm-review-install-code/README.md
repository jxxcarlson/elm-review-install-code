# elm-review-add-code

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.

## Provided rules

- [`MagicToken.AddToType`](https://package.elm-lang.org/packages/jxxcarlson/elm-review-add-code/1.0.0/MagicToken-AddToType) - Reports REPLACEME.

## Configuration

```elm
module ReviewConfig exposing (config)

import MagicToken.AddToType
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ MagicToken.AddToType.rule
    ]
```

## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template jxxcarlson/elm-review-add-code/example
```
