module Install.FieldInTypeAlias exposing (..)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node, range)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


makeRule : String -> String -> String -> Rule
makeRule moduleName_ typeName_ fieldDefinition_ =
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
            declarationVisitor moduleName_ typeName_ fieldName fieldCode
    in
    Rule.newModuleRuleSchemaUsingContextCreator "Install.FieldInTypeAlias" contextCreator
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


declarationVisitor : String -> String -> String -> String -> Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor moduleName_ typeName_ fieldName_ fieldValueCode_ node context =
    case Node.value node of
        Declaration.AliasDeclaration type_ ->
            let
                isInCorrectModule =
                    moduleName_ == (context.moduleName |> String.join "")

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
            if isInCorrectModule && Node.value type_.name == typeName_ && shouldFix node context then
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
