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
makeRule moduleName functionName fieldName fieldValue =
    let
        visitor : Node Declaration -> Context -> ( List (Error {}), Context )
        visitor =
            declarationVisitor moduleName functionName fieldName fieldValue
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


declarationVisitor : String -> String -> String -> String -> Node Declaration -> Context -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor moduleName functionName fieldName fieldValue (Node _ declaration) context =
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
                visitFunction namespace moduleName functionName fieldName fieldValue Set.empty function context

            else
                ( [], context )

        _ ->
            ( [], context )


visitFunction : String -> String -> String -> String -> String -> Ignored -> Function -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
visitFunction namespace moduleName functionName fieldName fieldValue ignored function context =
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

        ( fieldNames, lastRange ) =
            case declaration.expression |> Node.value of
                TupledExpression expressions ->
                    let
                        lastRange_ =
                            case expressions |> List.map Node.value |> List.head of
                                Just recordExpr ->
                                    MagicToken.Library.lastRange recordExpr

                                Nothing ->
                                    Elm.Syntax.Range.empty

                        fieldNames_ : List String
                        fieldNames_ =
                            case expressions |> List.map Node.value |> List.head of
                                Just recordExpr ->
                                    MagicToken.Library.fieldNames recordExpr

                                Nothing ->
                                    []
                    in
                    ( fieldNames_, lastRange_ )

                _ ->
                    ( [], Elm.Syntax.Range.empty )

        _ =
            ( fieldNames, lastRange ) |> Debug.log "!!!LAST_RANGE!!!"
    in
    if not <| List.member fieldName fieldNames then
        ( [ errorWithFix fieldName fieldValue function.declaration (Just lastRange) ], context )

    else
        ( [], context )


errorWithFix : String -> String -> Node a -> Maybe Range -> Error {}
errorWithFix fieldName fieldValue node errorRange =
    Rule.errorWithFix
        { message = "Add field " ++ fieldName ++ " with value " ++ fieldValue ++ " to the model"
        , details =
            [ "This addition is required to add magic-token authentication to your application"
            ]
        }
        (Node.range node)
        (case errorRange of
            Just range ->
                let
                    insertionPoint =
                        { row = range.end.row, column = range.end.column } |> Debug.log "INSERTION_POINT"
                in
                [ addMissingCase insertionPoint fieldName fieldValue ]

            Nothing ->
                []
        )


addMissingCase : { row : Int, column : Int } -> String -> String -> Fix
addMissingCase insertionPoint fieldName fieldValue =
    let
        insertion =
            ", " ++ fieldName ++ " = " ++ fieldValue ++ "\n  " |> Debug.log "INSERTION"
    in
    Fix.insertAt ({ row = insertionPoint.row, column = insertionPoint.column } |> Debug.log "{COLUMN, ROW}") insertion
