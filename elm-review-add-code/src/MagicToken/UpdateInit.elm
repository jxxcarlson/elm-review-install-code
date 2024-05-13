module MagicToken.UpdateInit exposing (..)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..), range)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import MagicToken.Library exposing (..)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


type alias Ignored =
    Set String


makeRule : String -> String -> String -> String -> Rule
makeRule moduleName functionName field fieldValue =
    let
        visitor : Node Declaration -> Context -> ( List (Error {}), Context )
        visitor =
            declarationVisitor functionName field fieldValue
    in
    Rule.newModuleRuleSchemaUsingContextCreator "MagicToken.UpdateInit" initContext
        |> Rule.withDeclarationEnterVisitor visitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , moduleName : String
    }


initContext : Rule.ContextCreator () Context
initContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , moduleName = ""
            }
        )
        |> Rule.withModuleNameLookupTable


declarationVisitor : String -> String -> String -> Node Declaration -> Context -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor functionName clause functionCall (Node _ declaration) context =
    case declaration of
        FunctionDeclaration function ->
            let
                --type alias Function =
                --    { documentation : Maybe (Node Documentation)
                --    , signature : Maybe (Node Signature)
                --    , declaration : Node FunctionImplementation
                --    }
                --_ =
                --    Debug.log "SIGNATURE" function.signature
                --
                --_ =
                --    Debug.log "F_DECLARATION" function.declaration
                name : String
                name =
                    Node.value (Node.value function.declaration).name

                namespace : String
                namespace =
                    context.moduleName ++ "." ++ name
            in
            if name == functionName then
                visitFunction namespace clause functionCall Set.empty function context

            else
                ( [], context )

        _ ->
            ( [], context )


visitFunction : String -> String -> String -> Ignored -> Function -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
visitFunction namespace clause functionCall ignored function context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        range =
            Node.range function.declaration |> Debug.log "range"

        _ =
            Debug.log "\n\n\n\nNAME" declaration.name

        _ =
            Debug.log "\n\nARGS" declaration.arguments

        _ =
            case declaration.expression |> Node.value of
                TupledExpression expressions ->
                    let
                        --record : List (Node Expression)
                        fieldNames =
                            case expressions |> List.map Node.value |> List.head of
                                Just recordExpr ->
                                    MagicToken.Library.fieldNames recordExpr

                                Nothing ->
                                    []
                    in
                    Debug.log "\n\nFIELDS (1)" fieldNames

                _ ->
                    Debug.log "\n\nFIELDS (2)" []
    in
    case declaration.expression |> Node.value of
        CaseExpression { expression, cases } ->
            let
                getPatterns : List Case -> List Pattern
                getPatterns cases_ =
                    cases_
                        |> List.map (\( pattern, _ ) -> Node.value pattern)

                findClause : String -> List Case -> Bool
                findClause clause_ cases_ =
                    List.any
                        (\pattern ->
                            case pattern of
                                NamedPattern qualifiedNameRef _ ->
                                    qualifiedNameRef.name == clause_

                                _ ->
                                    False
                        )
                        (getPatterns cases_)
            in
            if not (findClause clause cases) then
                ( [ errorWithFix clause functionCall declaration.expression (Just <| Node.range declaration.expression) ], context )

            else
                ( [], context )

        _ ->
            ( [], context )


errorWithFix : String -> String -> Node a -> Maybe Range -> Error {}
errorWithFix clause functionCall node errorRange =
    Rule.errorWithFix
        { message = "Add handler for " ++ clause
        , details =
            [ "This addition is required to add magic-token authentication to your application"
            ]
        }
        (Node.range node)
        (case errorRange of
            Just range ->
                let
                    insertionPoint =
                        { row = range.end.row + 2, column = 0 }
                in
                [ addMissingCase insertionPoint clause functionCall ]

            Nothing ->
                []
        )


addMissingCase : { row : Int, column : Int } -> String -> String -> Fix
addMissingCase { row, column } clause functionCall =
    let
        insertion =
            "\n\n        " ++ clause ++ " -> " ++ functionCall
    in
    Fix.insertAt { row = row, column = column } insertion
