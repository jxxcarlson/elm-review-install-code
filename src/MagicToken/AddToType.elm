module MagicToken.AddToType exposing (makeAddToTypeRule)

{-|

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node, range)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Type exposing (ValueConstructor)
import Review.Rule as Rule exposing (Error, Rule)


makeAddToTypeRule : String -> String -> Rule
makeAddToTypeRule typeName_ variantName_ =
    Rule.newModuleRuleSchema "MagicToken.AddToType" Nothing
        |> Rule.withDeclarationEnterVisitor (declarationVisitor typeName_ variantName_)
        |> Rule.fromModuleRuleSchema


declarationVisitor : String -> String -> Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor typeName variantName_ node _ =
    case Node.value node of
        Declaration.CustomTypeDeclaration type_ ->
            if Node.value type_.name == typeName then
                checkForVariant variantName_ (Node.range node) type_

            else
                ( [], Nothing )

        _ ->
            ( [], Nothing )


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


contextOfNode : Node Declaration -> Context
contextOfNode node =
    case Node.value node of
        Declaration.CustomTypeDeclaration tipe ->
            --Just { typeInfo =  node.name
            --    , constructors = Node.value node
            --    }
            Nothing

        _ ->
            Nothing


type alias Context =
    Maybe
        { typeInfo : Node String
        , constructors : List (Node ValueConstructor)
        }


type alias ModuleContext =
    { moduleName : String
    }
