module MagicToken.AddToCaseStatementInFunction exposing (makeAddToCaseStatementInFunctionRule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Case, CaseBlock, Expression(..), Function, FunctionImplementation, Lambda, LetBlock, LetDeclaration(..))
import Elm.Syntax.Node as Node exposing (Node(..), range)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


type alias Ignored =
    Set String


makeAddToCaseStatementInFunctionRule : String -> String -> String -> String -> Rule
makeAddToCaseStatementInFunctionRule moduleName functionName clause functionCall =
    let
        visitor : Node Declaration -> Context -> ( List (Error {}), Context )
        visitor =
            declarationVisitor functionName
    in
    Rule.newModuleRuleSchemaUsingContextCreator "MagicToken.AddToCaseStatementInFunction" initContext
        -- Required: ModuleRuleSchema { } Context → ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
        -- Found: ModuleRuleSchema { } ModuleContext → ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
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


declarationVisitor : String -> Node Declaration -> Context -> ( List (Rule.Error {}), ModuleContext )
declarationVisitor functionName (Node _ declaration) context =
    case declaration of
        FunctionDeclaration function ->
            let
                name : String
                name =
                    Node.value (Node.value function.declaration).name |> Debug.log "FUNCTION NAME"

                namespace : String
                namespace =
                    context.moduleName ++ "." ++ name
            in
            if name == functionName then
                ( [], visitFunction namespace Set.empty function context )

            else
                ( [], context )

        _ ->
            ( [], context )


upsert : comparable1 -> comparable2 -> Dict comparable1 (Set comparable2) -> Dict comparable1 (Set comparable2)
upsert key value dict =
    Dict.insert key (Set.insert value <| Maybe.withDefault Set.empty (Dict.get key dict)) dict


visitFunction : String -> Ignored -> Function -> ModuleContext -> ModuleContext
visitFunction namespace ignored function context =
    let
        declaration : FunctionImplementation
        declaration =
            Node.value function.declaration

        newIgnored : Ignored
        newIgnored =
            extractNamesFromPatterns declaration.arguments ignored
                |> Set.insert (Node.value declaration.name)

        foo =
            case declaration.expression |> Node.value of
                CaseExpression caseBlock ->
                    Debug.toString caseBlock |> Debug.log "CASE BLOCK"

                _ ->
                    "-"

        --Debug.log "DECL EXPRESSION" (declaration.expression |> Node.value)
    in
    visitExpression namespace newIgnored declaration.expression context


expressionVisitor : String -> String -> String -> String -> Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor moduleName functionName clause functionCall node context =
    case Node.value node of
        CaseExpression caseBlock ->
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
    , lookupTable : ModuleNameLookupTable
    }


extractNamesFromPatterns : List (Node Pattern) -> Set String -> Set String
extractNamesFromPatterns patterns set =
    List.foldl extractNamesFromPattern set patterns


extractNamesFromPattern : Node Pattern -> Set String -> Set String
extractNamesFromPattern (Node _ pattern) set =
    case pattern of
        VarPattern v ->
            Set.insert v set

        RecordPattern fields ->
            List.foldl (\(Node _ field) -> Set.insert field) set fields

        UnConsPattern head tail ->
            extractNamesFromPatterns [ head, tail ] set

        ListPattern children ->
            extractNamesFromPatterns children set

        TuplePattern children ->
            extractNamesFromPatterns children set

        NamedPattern _ children ->
            extractNamesFromPatterns children set

        AsPattern child (Node _ var) ->
            extractNamesFromPattern child (Set.insert var set)

        ParenthesizedPattern child ->
            extractNamesFromPattern child set

        _ ->
            set


visitExpression : String -> Ignored -> Node Expression -> ModuleContext -> ModuleContext
visitExpression namespace ignored ((Node _ expression) as expressionNode) context =
    case expression of
        FunctionOrValue moduleName name ->
            case ModuleNameLookupTable.fullModuleNameFor context.lookupTable expressionNode of
                Nothing ->
                    context

                Just fullModuleName ->
                    let
                        fullModuleNameJoined : String
                        fullModuleNameJoined =
                            String.join "." fullModuleName
                    in
                    if
                        List.isEmpty moduleName && Set.member name ignored
                        --|| Set.member fullModuleNameJoined coreModules
                    then
                        context

                    else
                        let
                            fullName : String
                            fullName =
                                fullModuleNameJoined
                                    ++ "."
                                    ++ name
                        in
                        context

        IfBlock c t f ->
            visitExpressions namespace ignored [ c, t, f ] context

        OperatorApplication _ _ l r ->
            visitExpressions namespace ignored [ l, r ] context

        Application children ->
            visitExpressions namespace ignored children context

        TupledExpression children ->
            visitExpressions namespace ignored children context

        ListExpr children ->
            visitExpressions namespace ignored children context

        Negation child ->
            visitExpression namespace ignored child context

        ParenthesizedExpression child ->
            visitExpression namespace ignored child context

        RecordAccess child _ ->
            visitExpression namespace ignored child context

        LetExpression letBlock ->
            visitLetBlock namespace ignored letBlock context

        CaseExpression caseBlock ->
            visitCaseBlock namespace ignored caseBlock context

        LambdaExpression lambda ->
            visitLambda namespace ignored lambda context

        RecordExpr recordSetters ->
            visitRecordSetters namespace ignored recordSetters context

        RecordUpdateExpression _ recordSetters ->
            visitRecordSetters namespace ignored recordSetters context

        _ ->
            context


visitExpressions : String -> Ignored -> List (Node Expression) -> ModuleContext -> ModuleContext
visitExpressions namespace ignored expressions context =
    List.foldl (visitExpression namespace ignored) context expressions


visitRecordSetters : String -> Ignored -> List (Node Elm.Syntax.Expression.RecordSetter) -> ModuleContext -> ModuleContext
visitRecordSetters namespace ignored recordSetters context =
    List.foldl
        (\(Node _ ( _, expression )) -> visitExpression namespace ignored expression)
        context
        recordSetters


visitLambda : String -> Ignored -> Lambda -> ModuleContext -> ModuleContext
visitLambda namespace ignored { args, expression } context =
    visitExpression namespace
        (extractNamesFromPatterns args ignored)
        expression
        context


visitCaseBlock : String -> Ignored -> CaseBlock -> ModuleContext -> ModuleContext
visitCaseBlock namespace ignored caseBlock context =
    List.foldl
        (visitCase namespace ignored)
        (visitExpression namespace ignored caseBlock.expression context)
        caseBlock.cases


visitCase : String -> Ignored -> Case -> ModuleContext -> ModuleContext
visitCase namespace ignored ( pattern, expression ) context =
    visitExpression namespace
        (extractNamesFromPattern pattern ignored)
        expression
        context


visitLetBlock : String -> Ignored -> LetBlock -> ModuleContext -> ModuleContext
visitLetBlock namespace ignored { declarations, expression } context =
    let
        newIgnored : Ignored
        newIgnored =
            List.foldl extractNamesFromLetDeclaration ignored declarations

        contextAfterDeclarations : ModuleContext
        contextAfterDeclarations =
            List.foldl
                (visitLetDeclaration namespace newIgnored)
                context
                declarations
    in
    visitExpression namespace newIgnored expression contextAfterDeclarations


extractNamesFromLetDeclaration : Node LetDeclaration -> Ignored -> Ignored
extractNamesFromLetDeclaration (Node _ letDeclaration) ignored =
    case letDeclaration of
        LetDestructuring pattern _ ->
            extractNamesFromPattern pattern ignored

        LetFunction function ->
            let
                name : String
                name =
                    function.declaration
                        |> Node.value
                        |> .name
                        |> Node.value
            in
            Set.insert name ignored


visitLetDeclaration : String -> Ignored -> Node LetDeclaration -> ModuleContext -> ModuleContext
visitLetDeclaration namespace ignored (Node _ letDeclaration) context =
    case letDeclaration of
        LetDestructuring pattern expression ->
            let
                newIgnored : Set String
                newIgnored =
                    extractNamesFromPattern pattern ignored
            in
            visitExpression namespace
                newIgnored
                expression
                context

        LetFunction function ->
            visitFunction namespace ignored function context
