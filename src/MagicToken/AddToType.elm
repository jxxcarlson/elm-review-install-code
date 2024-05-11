module MagicToken.AddToType exposing (makeAddToTypeRule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node, range)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


makeAddToTypeRule : String -> String -> String -> Rule
makeAddToTypeRule typeName_ variantName_ variantCode_ =
    let
        visitor : Node Declaration -> Context -> ( List (Error {}), Context )
        visitor =
            declarationVisitor typeName_ variantName_ variantCode_
    in
    Rule.newModuleRuleSchemaUsingContextCreator "MagicToken.AddToType" initContext
        |> Rule.withDeclarationEnterVisitor visitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema



--rule : Rule
--rule =
--    Rule.newModuleRuleSchemaUsingContextCreator "NoDebug.Log" initContext
--        |> Rule.withExpressionEnterVisitor expressionVisitor
--        |> Rule.providesFixesForModuleRule
--        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , rangesToIgnore : List Range
    }


initContext : Rule.ContextCreator () Context
initContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , rangesToIgnore = []
            }
        )
        |> Rule.withModuleNameLookupTable


errorWithFix : String -> String -> String -> Node a -> Maybe Range -> Error {}
errorWithFix typeName_ variantName_ variantCode_ node errorRange =
    Rule.errorWithFix
        { message = "Add " ++ variantName_ ++ " to " ++ typeName_
        , details =
            [ "This addition is required to add magic-token authentication to your application"
            ]
        }
        (Node.range node)
        (case errorRange of
            Just range ->
                [ fixMissingVariant range.end variantCode_ ]

            Nothing ->
                []
        )


fixMissingVariant : { row : Int, column : Int } -> String -> Fix
fixMissingVariant { row, column } variantCode =
    Fix.insertAt { row = row, column = column } variantCode

declarationVisitor : String -> String -> String -> Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor typeName_ variantName_ variantCode_ node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration type_ ->
            if Node.value type_.name == typeName_ then
                ( [ errorWithFix typeName_ variantName_ variantCode_ node (Just <| Node.range node) ], context )

            else
                ( [], context )

        _ ->
            ( [], context )


checkForVariant : String -> Range -> { a | constructors : List (Node ValueConstructor) } -> ( List (Error {}), Maybe b )
checkForVariant variantName_ range type_ =
    if List.member variantName_ (List.map variantName type_.constructors) then
        ( [], Nothing )

    else
        ( [ Rule.error
                { message = "FrontendMsg: variant " ++ variantName_ ++ " is missing in the list"
                , details =
                    [ Debug.toString (List.map variantName type_.constructors)
                    , "Error location: " ++ Debug.toString range
                    ]
                }
                range
          ]
        , Nothing
        )


variantName : Node ValueConstructor -> String
variantName node =
    Node.value node |> .name |> Node.value





--type alias Context =
--    Maybe
--        { typeInfo : Node String
--        , constructors : List (Node ValueConstructor)
--        }


type alias ModuleContext =
    { moduleName : String
    }
