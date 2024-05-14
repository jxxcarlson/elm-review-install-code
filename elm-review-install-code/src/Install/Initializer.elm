module Install.Initializer exposing (..)

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..), range)
import Elm.Syntax.Range exposing (Range)
import Install.Library exposing (..)
import Review.Fix as Fix exposing (Fix)
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
    Rule.newModuleRuleSchemaUsingContextCreator "Install.Initializer" contextCreator
        |> Rule.withDeclarationEnterVisitor visitor
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


type alias Context =
    { moduleName : ModuleName
    }



--type alias Context =
--    { lookupTable : ModuleNameLookupTable
--    , moduleName : String
--    }
--initContext : Rule.ContextCreator () Context
--initContext =
--    Rule.initContextCreator
--        (\lookupTable () ->
--            { lookupTable = lookupTable
--            , moduleName = ""
--            }
--        )
--        |> Rule.withModuleNameLookupTable
-- contextCreator : Rule.ContextCreator () Context


contextCreator : Rule.ContextCreator () { moduleName : ModuleName }
contextCreator =
    Rule.initContextCreator
        (\moduleName () ->
            { moduleName = moduleName

            -- ...other fields
            }
        )
        |> Rule.withModuleName


declarationVisitor : String -> String -> String -> String -> Node Declaration -> Context -> ( List (Rule.Error {}), Context )
declarationVisitor moduleName functionName fieldName fieldValue (Node _ declaration) context =
    case declaration of
        FunctionDeclaration function ->
            let
                name : String
                name =
                    Node.value (Node.value function.declaration).name

                namespace : String
                namespace =
                    String.join "." context.moduleName ++ "." ++ name
            in
            if name == functionName then
                visitFunction namespace moduleName functionName fieldName fieldValue Set.empty function context

            else
                ( [], context )

        _ ->
            ( [], context )


visitFunction : String -> String -> String -> String -> String -> Ignored -> Function -> Context -> ( List (Rule.Error {}), Context )
visitFunction namespace moduleName functionName fieldName fieldValue ignored function context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        isInCorrectModule =
            moduleName == (context.moduleName |> String.join "")

        ( fieldNames, lastRange ) =
            case declaration.expression |> Node.value of
                TupledExpression expressions ->
                    let
                        lastRange_ =
                            case expressions |> List.map Node.value |> List.head of
                                Just recordExpr ->
                                    Install.Library.lastRange recordExpr

                                Nothing ->
                                    Elm.Syntax.Range.empty

                        fieldNames_ : List String
                        fieldNames_ =
                            case expressions |> List.map Node.value |> List.head of
                                Just recordExpr ->
                                    Install.Library.fieldNames recordExpr

                                Nothing ->
                                    []
                    in
                    ( fieldNames_, lastRange_ )

                _ ->
                    ( [], Elm.Syntax.Range.empty )

        _ =
            ( fieldNames, lastRange )
    in
    if
        isInCorrectModule
            && (not <| List.member fieldName fieldNames)
    then
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
                        { row = range.end.row, column = range.end.column }
                in
                [ addMissingCase insertionPoint fieldName fieldValue ]

            Nothing ->
                []
        )


addMissingCase : { row : Int, column : Int } -> String -> String -> Fix
addMissingCase insertionPoint fieldName fieldValue =
    let
        insertion =
            ", " ++ fieldName ++ " = " ++ fieldValue ++ "\n  "
    in
    Fix.insertAt { row = insertionPoint.row, column = insertionPoint.column } insertion
