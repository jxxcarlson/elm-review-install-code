module Install.FieldInTypeAlias exposing (..)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Node as Node exposing (Node, range)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


makeAddToTypeAliasRule : String -> String -> Rule
makeAddToTypeAliasRule typeName_ fieldDefinition_ =
    let
        fieldName =
            fieldDefinition_
                |> String.split ":"
                |> List.head
                |> Maybe.withDefault ""
                |> String.trim

        fieldCode =
            "\n    , " ++ fieldDefinition_ ++ "\n    }"

        visitor : Node Declaration -> Context -> ( List (Error {}), Context )
        visitor =
            declarationVisitor typeName_ fieldName fieldCode
    in
    Rule.newModuleRuleSchemaUsingContextCreator "Install.FieldInTypeAlias" initContext
        |> Rule.withDeclarationEnterVisitor visitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    }


initContext : Rule.ContextCreator () Context
initContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            }
        )
        |> Rule.withModuleNameLookupTable


errorWithFix : String -> String -> String -> Node a -> Maybe Range -> Error {}
errorWithFix typeName_ fieldName fieldCode node errorRange =
    Rule.errorWithFix
        { message = "Add " ++ fieldName ++ " to " ++ typeName_
        , details =
            [ "This addition is required to add magic-token authentication to your application"
            ]
        }
        (Node.range node)
        (case errorRange of
            Just range ->
                [ fixMissingField range.end fieldCode ]

            Nothing ->
                []
        )


fixMissingField : { row : Int, column : Int } -> String -> Fix
fixMissingField { row, column } fieldCode =
    let
        range =
            { start = { row = row, column = 0 }, end = { row = row, column = column } }
    in
    Fix.replaceRangeBy range fieldCode


declarationVisitor : String -> String -> String -> Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor typeName_ fieldName_ fieldValueCode_ node context =
    case Node.value node of
        Declaration.AliasDeclaration type_ ->
            let
                shouldFix : Node Declaration -> Context -> Bool
                shouldFix node_ context_ =
                    let
                        fieldsOfNode : List String
                        fieldsOfNode =
                            case Node.value node_ of
                                Declaration.AliasDeclaration typeAlias ->
                                    case typeAlias.typeAnnotation |> Node.value of
                                        TypeAnnotation.Record fields ->
                                            fields
                                                |> List.map (Node.value >> Tuple.first >> Node.value)

                                        _ ->
                                            []

                                _ ->
                                    []
                    in
                    not <| List.member fieldName_ fieldsOfNode
            in
            if Node.value type_.name == typeName_ && shouldFix node context then
                ( [ errorWithFix typeName_ fieldName_ fieldValueCode_ node (Just <| Node.range node) ]
                , context
                )

            else
                ( [], context )

        _ ->
            ( [], context )


type alias ModuleContext =
    { moduleName : String
    }
