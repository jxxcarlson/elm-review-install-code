module MagicToken.AddToCaseStatementInFunction exposing (makeAddToCaseStatementInFunctionRule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node, range)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


makeAddToCaseStatementInFunctionRule : String -> String -> String -> String -> Rule
makeAddToCaseStatementInFunctionRule moduleName functionName clause functionCall =
    let
        visitor : Node Expression -> Context -> ( List (Error {}), Context )
        visitor =
            expressionVisitor moduleName functionName clause functionCall
    in
    Rule.newModuleRuleSchemaUsingContextCreator "MagicToken.AddToCaseStatementInFunction" initContext
        |> Rule.withExpressionEnterVisitor visitor
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


expressionVisitor : String -> String -> String -> String -> Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor moduleName functionName clause functionCall node context =
    case Node.value node of
        Expression.CaseExpression caseBlock ->
            let
                _ =
                    -- if Node.value name == functionName then
                    Debug.log "\n\nCASE BLOCK" caseBlock

                --shouldFix : Node Declaration -> Context -> Bool
                --shouldFix node_ context_ =
                --    let
                --        variantsOfNode : List String
                --        variantsOfNode =
                --            case Node.value node_ of
                --                Declaration.CustomTypeDeclaration type__ ->
                --                    type__.constructors |> List.map (Node.value >> .name >> Node.value)
                --
                --                _ ->
                --                    []
                --    in
                --    not <| List.member variantName_ variantsOfNode
            in
            ( --[ errorWithFix moduleName_ functionName_ node (Just <| Node.range node) ]
              []
            , context
            )

        _ ->
            ( [], context )


type alias ModuleContext =
    { moduleName : String
    }
