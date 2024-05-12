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


makeAddToTypeRule : String -> String  -> Rule
makeAddToTypeRule typeName_ variant_ =
    let
        variantName_ = variant_
          |> String.split " "
          |> List.head
          |> Maybe.withDefault ""
          |> String.trim


        variantCode_= "\n    | " ++ variant_

        visitor : Node Declaration -> Context -> ( List (Error {}), Context )
        visitor =
            declarationVisitor typeName_ variantName_ variantCode_
    in
    Rule.newModuleRuleSchemaUsingContextCreator "MagicToken.AddToType" initContext
        |> Rule.withDeclarationEnterVisitor visitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , rangesToIgnore : List Range
    , variantNamesToIgnore : List String
    }


initContext : Rule.ContextCreator () Context
initContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , rangesToIgnore = []
            , variantNamesToIgnore = []
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
            let
                extendRange : Range -> Context -> Context
                extendRange range context_ =
                    { context_ | rangesToIgnore = range :: context.rangesToIgnore }

                updateVariantNamesToIgnore : String -> Context -> Context
                updateVariantNamesToIgnore variantName context_ =
                    { context_ | variantNamesToIgnore = variantName :: context.variantNamesToIgnore }

                newContext =
                    context
                        |> extendRange (Node.range node)
                        |> updateVariantNamesToIgnore variantName_

                shouldFix : Node Declaration -> Context -> Bool
                shouldFix node_ context_ =
                    let
                        endOfNode =
                            (Node.range node_).end

                        endsToAvoid =
                            context_.rangesToIgnore |> List.map .end
                    in
                    (not <| List.member endOfNode endsToAvoid)
                        && (not <| List.member variantName_ context.variantNamesToIgnore)
            in
            if Node.value type_.name == typeName_ && shouldFix node context then
                ( [ errorWithFix typeName_ variantName_ variantCode_ node (Just <| Node.range node) ]
                , newContext
                )

            else
                ( [], context )

        _ ->
            ( [], context )


type alias ModuleContext =
    { moduleName : String
    }
