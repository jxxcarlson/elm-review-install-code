module Install.TypeVariant exposing (makeRule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node, range)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


makeRule : String -> String -> String -> Rule
makeRule moduleName typeName_ variant_ =
    let
        variantName_ =
            variant_
                |> String.split " "
                |> List.head
                |> Maybe.withDefault ""
                |> String.trim

        variantCode_ =
            "\n    | " ++ variant_

        visitor : Node Declaration -> Context -> ( List (Error {}), Context )
        visitor =
            declarationVisitor moduleName typeName_ variantName_ variantCode_
    in
    Rule.newModuleRuleSchemaUsingContextCreator "Install.TypeVariant" contextCreator
        |> Rule.withDeclarationEnterVisitor visitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { moduleName : ModuleName
    }


contextCreator : Rule.ContextCreator () { moduleName : ModuleName }
contextCreator =
    Rule.initContextCreator
        (\moduleName () ->
            { moduleName = moduleName

            -- ...other fields
            }
        )
        |> Rule.withModuleName


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


declarationVisitor : String -> String -> String -> String -> Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor moduleName_ typeName_ variantName_ variantCode_ node context =
    case Node.value node of
        Declaration.CustomTypeDeclaration type_ ->
            let
                isInCorrectModule =
                    moduleName_ == (context.moduleName |> String.join "")

                shouldFix : Node Declaration -> Context -> Bool
                shouldFix node_ context_ =
                    let
                        variantsOfNode : List String
                        variantsOfNode =
                            case Node.value node_ of
                                Declaration.CustomTypeDeclaration type__ ->
                                    type__.constructors |> List.map (Node.value >> .name >> Node.value)

                                _ ->
                                    []
                    in
                    not <| List.member variantName_ variantsOfNode
            in
            if isInCorrectModule && Node.value type_.name == typeName_ && shouldFix node context then
                ( [ errorWithFix typeName_ variantName_ variantCode_ node (Just <| Node.range node) ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )
