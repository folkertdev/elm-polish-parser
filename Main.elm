module Main where 

import Html exposing (..) 
import Html.Events exposing (..) 
import Html.Attributes exposing (..) 

import Task
import String
import Debug

import Json.Decode as Decode exposing (..) 

import Effects exposing (..)
import StartApp exposing (start)




type alias Model = 
    { input : String 
    , error : Maybe String 
    , output : List Val
    } 

init = 
    { input = ""
    , error = Nothing 
    , output = [] 
    } 

type Val 
    = Pure Float 
    | Operator (Float -> Float -> Float) 

type Action 
    = NoOp
    | ChangeInput String 
    | ParseInput 

update : Action -> Model -> (Model, Effects Action) 
update action model = 
    case action of 
        NoOp -> 
            (model, Effects.none) 

        ChangeInput newInput -> 
            ( { model 
                    | input = newInput
              }
            , Effects.task << Task.succeed <| ParseInput
            )

        ParseInput -> 
            let result = parse model.input
            in 
               case result of 
                   Err e -> 
                       ( { model 
                                | output = [] 
                                , error = Just e
                         }
                       , Effects.none
                       )

                   Ok v -> 
                       ( { model 
                                | output = v
                                , error = Nothing 
                         }
                       , Effects.none
                       )

maybe : b -> (a -> b) -> Maybe a -> b
maybe default f val = 
    case val of 
        Nothing -> 
            default 

        Just v -> 
            f v 

until                   : (a -> Bool) -> (a -> a) -> a -> a
until p f = 
   let go x = 
        if p x then 
          x
        else 
          go (f x)
   in go

until' : (a -> Maybe a) -> a -> a
until' f x = maybe x (until' f) (f x)

converge f = 
    until' <| (\x -> 
        let y = f x 
        in if x == y then Nothing else Just y
    )
     


reduce : Val -> List Val -> List Val 
reduce val accum = 
    let result = 
        case accum of 
            a::g::xs -> 
               -- try to reduce this expression 
               case (val, a, g) of 
                   (Operator f, Pure x, Pure y) -> 
                       Pure (f x y) :: xs
                   _ -> 
                       val :: accum 
            _ -> 
                val :: accum 
        _ = Debug.log "reduce" (toString accum ++ " -> " ++ toString val ++ " -> " ++ toString result) 
    in result 


polish : List Val -> Result String Val 
polish input = 
    case input of 
        [] -> Err "empty"
        [x] -> case x of 
                    Pure v -> Ok (Pure v)
                    Operator _ -> Err "operator has no arguments"
        a::b::f'::xs -> 
            let reduced = converge (List.foldr reduce []) input 
            in if List.length reduced /= List.length input then 
                  polish reduced 
                else 
                    Err "reduced does not make progress. ill-formed input"
        _ -> 
            Err "operator has too few arguments"

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))

toOperator : String -> Result String Val
toOperator op = 
    case op of 
        "+" -> Ok <| Operator (+)
        "-" -> Ok <| Operator (-)
        "*" -> Ok <| Operator (*)
        ":" -> Ok <| Operator (/) 
        "/" -> Ok <| Operator (/)
        --"%" -> Ok (%) 
        _   -> Err <| "operator `" ++ op ++ "` is not supported"  

parse : String -> Result String (List Val) 
parse input = 
    let tokens = String.split " " input 
                    |> List.filter ((/=) "") 
        operators = 
            Decode.customDecoder Decode.string toOperator

        literal = Decode.map Pure Decode.float  

        --parser = Decode.decodeString (Decode.oneOf [ operators, litera])
        --parser = Decode.decodeString operators
        parser v = 
            case toOperator v of 
                Err _ -> 
                    Decode.decodeString literal v 
                Ok v ->
                    Ok v 

        parsed = List.map parser tokens

        errors = 
            let func : Result String a -> List String -> List String 
                func val accum = 
                    case val of 
                        Err s -> s :: accum 
                        Ok  v -> accum 
            in List.foldr func [] parsed 
    in 
       case errors of 
           [] -> 
              Ok <| List.map (Result.withDefault (Pure 0)) parsed 
           es -> 
               Err <| String.join ", " errors 



view address model = 
    div []
        [ input [ type' "text", onInput address (\s -> ChangeInput s) ] [] 
        , text << toString <| model.output
        , text << toString <| model.error
        , text << toString <| polish model.output
        ] 

app = 
    StartApp.start 
        { init = (init, Effects.none)
        , view = view
        , update = update
        , inputs = []
        }

main = 
    app.html


port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks

