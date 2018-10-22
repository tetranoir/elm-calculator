module Calculator exposing (..)

import Regex exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as Attrs exposing (..)

main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type FloatInput = FloatInput (Maybe Float) String

type AppliedOp = AppliedOp BinaryOp Float

type alias Model =
  { activeValue : FloatInput
  , currentValue : Maybe Float
  , operation : Maybe BinaryOp
  , prevApply: Maybe AppliedOp
  }

model : Model
model =
  { activeValue = FloatInput Nothing "0"
  , currentValue = Nothing
  , operation = Nothing
  , prevApply = Nothing
  }


-- UPDATE

type Append
  = App0
  | App1
  | App2
  | App3
  | App4
  | App5
  | App6
  | App7
  | App8
  | App9
  | AppDot
  | AppNeg
  | AppDel


type BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  | Eq

type UnaryOp
  = Neg
  | Square
  | MulInv

type Operation
  = Binary BinaryOp
  | Unary UnaryOp

type Msg = Clear | ClearAll | Append Append | Set String | Operation Operation | Apply

floatInputString : FloatInput -> String
floatInputString floatInput =
  case floatInput of
    FloatInput _ input ->
      input

floatInputFloat : FloatInput -> Maybe Float
floatInputFloat floatInput =
  case floatInput of
    FloatInput float _ ->
      float

maybeOperationString : Maybe BinaryOp -> String
maybeOperationString mBinOp =
  case mBinOp of
    Just op ->
      case op of
        Add ->
          "+"
        Sub ->
          "-"
        Mul ->
          "*"
        Div ->
          "/"
        Pow ->
          "^"
        Eq ->
          "="
    Nothing ->
      " "

maybeFloatString : Maybe Float -> String
maybeFloatString mFloat =
  case mFloat of
    Just float ->
      toString float
    Nothing ->
      ""

maybeApplyedOpOp : Maybe AppliedOp -> Maybe BinaryOp
maybeApplyedOpOp mAOp =
  case mAOp of
    Just (AppliedOp op _) ->
      Just op
    Nothing ->
      Nothing

maybeApplyedOpFloat : Maybe AppliedOp -> Maybe Float
maybeApplyedOpFloat mAOp =
  case mAOp of
    Just (AppliedOp _ float) ->
      Just float
    Nothing ->
      Nothing

appendToVal : Append -> String -> String
appendToVal a oldStr =
  case a of
    AppDel ->
      String.dropRight 1 oldStr
    _ ->
      oldStr ++ case a of
        App0 -> "0"
        App1 -> "1"
        App2 -> "2"
        App3 -> "3"
        App4 -> "4"
        App5 -> "5"
        App6 -> "6"
        App7 -> "7"
        App8 -> "8"
        App9 -> "9"
        AppDot -> "."
        AppNeg -> "-"
        AppDel -> ""
      |> trimZero

applyOperation : BinaryOp -> Float -> Float -> Float
applyOperation binOp v1 v2 =
  case binOp of
    Add -> 
      v1 + v2
    Sub ->
      v1 - v2
    Mul ->
      v1 * v2
    Div ->
      v1 / v2
    Pow ->
      v1 ^ v2
    Eq ->
      v2

trimZero : String -> String
trimZero numStr = 
  case Regex.find (Regex.AtMost 1) (Regex.regex "^(0*)((?:0\\.)?\\d+)$") numStr of
    [ { submatches } ] ->
      case submatches of
        [ Just a, Just b ] ->
          b
        [ Just a ] ->
          a
        _ ->
          numStr
    _ ->
      numStr

maybeNegateValue : Maybe Float -> Maybe Float
maybeNegateValue mf =
  case mf of
    Just f ->
      Just (f * -1)
    Nothing ->
      mf

clearAll : Model -> Model
clearAll model =
  { model |
    activeValue = FloatInput Nothing "0",
    currentValue = Nothing,
    operation = Nothing,
    prevApply = Nothing
  }

clearValue : Model -> Model
clearValue model =
  case (model.activeValue, model.operation) of
    (FloatInput _ "0", Nothing) ->
      { model | currentValue = Nothing }
    (FloatInput _ "0", Just op) ->
      { model | operation = Nothing }
    (FloatInput _ "", _) -> -- if theres an operation but no active value, erase operation too
      { model | activeValue = FloatInput Nothing "0", operation = Nothing }
    _ ->
      { model | activeValue = FloatInput Nothing "0" }

setValue : Model -> String -> Model
setValue model inputStr = 
  case String.right 1 inputStr of
    "." ->
      if String.dropRight 1 inputStr |> String.contains "." then
        model
      else
        { model | activeValue = FloatInput Nothing inputStr }
    "-" ->
      case String.toList inputStr of
        a :: b ->
          if a == '-' then
            { model | activeValue = FloatInput (Just 0) (String.fromList b |> String.dropRight 1) }
          else
            let
              negatedVal = floatInputFloat model.activeValue |> maybeNegateValue
            in
              { model |
                activeValue = FloatInput negatedVal (String.dropRight 1 inputStr |> String.cons '-')
              }
        _ ->
          { model | activeValue = FloatInput (Just 0) "-" }
    "" ->
        { model | activeValue = FloatInput (Just 0) inputStr }
    _ ->
      case String.toFloat inputStr of
        Ok float ->
          { model | activeValue = FloatInput (Just float) (trimZero inputStr) }
        Err _ ->
          model

setOperation : Model -> BinaryOp -> Model
setOperation model op =
  case (model.activeValue, model.operation, model.currentValue) of
    (FloatInput (Just avFloat) _, Just oldOp, Just cvFloat) ->
      { model |
        currentValue = Just (applyOperation oldOp cvFloat avFloat),
        activeValue = FloatInput Nothing "0",
        operation = Just op,
        prevApply = Just (AppliedOp oldOp avFloat)
      }
    (FloatInput (Just avFloat) _, _, Nothing) ->
      { model |
        currentValue = Just avFloat,
        activeValue = FloatInput Nothing "0",
        operation = Just op
      }
    (FloatInput (Just avFloat) _, _, Just cvFloat) ->
      { model |
        operation = Just op
      }
    (FloatInput Nothing _, _, _) ->
      { model |
        operation = Just op
      }

applyUnaryOp: UnaryOp -> Float -> Float
applyUnaryOp op float =
  case op of
    Neg ->
      -1 * float
    Square ->
      float * float
    MulInv ->
      1 / float

unaryOperate : Model -> UnaryOp -> Model
unaryOperate model op =
  case (model.currentValue, model.activeValue) of
    (_, FloatInput (Just avFloat) _) ->
      let 
        appliedVal = applyUnaryOp op avFloat
      in
        Debug.log "unary active value" { model |
          activeValue = FloatInput (Just appliedVal) (toString appliedVal)
        }
    (Just cvFloat, FloatInput Nothing input) ->
      if String.isEmpty input || input == "0" then
        Debug.log "unary current value" { model |
          currentValue = Just (applyUnaryOp op cvFloat)
        }
      else
        Debug.log "unary active nan" model -- invalid floatinput, this is an error
    (Nothing, _) ->
      Debug.log "unary nothing" model

applyValue : Model -> Model
applyValue model =
  case (model.activeValue, model.operation, model.currentValue, model.prevApply) of
    (_, Nothing, Just cvFloat, Just (AppliedOp prevOp prevAVFloat)) ->
      { model |
        currentValue = Just (applyOperation prevOp cvFloat prevAVFloat)
      }
    (FloatInput (Just avFloat) _, Just op, Just cvFloat, _) ->
      { model |
        currentValue = Just (applyOperation op cvFloat avFloat),
        activeValue = FloatInput Nothing "",
        operation = Nothing,
        prevApply = Just (AppliedOp op avFloat)
      }
    (FloatInput (Just avFloat) _, _, Nothing, _) -> -- switches active to current
      { model |
        activeValue = FloatInput Nothing "",
        currentValue = Just avFloat
      }
    (_, _, Nothing, _) ->
      Debug.log "apply with no current value" model -- this is an Error
    (_, Nothing, _, _) ->
      Debug.log "apply with no operation" model -- this is an Error
    (FloatInput Nothing _, _, _, _) ->
      Debug.log "apply with no active value" model -- this is an Error

update : Msg -> Model -> Model
update msg model =
  case msg of
    Clear ->
      clearValue model
    ClearAll ->
      clearAll model
    Set inputStr ->
      setValue model inputStr
    Append a ->
      appendToVal a (floatInputString model.activeValue)
      |> setValue model
    Operation op ->
      case op of 
        Binary binOp ->
          setOperation model binOp
        Unary unOp ->
          unaryOperate model unOp
    Apply ->
      applyValue model


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ Html.node "link" [ Attrs.rel "stylesheet", Attrs.href "Calculator.css" ] []
    , Html.node "link" [ Attrs.rel "stylesheet", Attrs.href "https://fonts.googleapis.com/css?family=Lato" ] []
    , div [ class "calculator" ]
      [ div [ class "screen" ]
        [ div [ class "output" ] [ maybeFloatString model.currentValue |> text ]
        , div [ class "operation" ] 
          [ div [] [ maybeApplyedOpOp model.prevApply |> maybeOperationString |> text ]
          , div [] [ maybeOperationString model.operation |> text ]
          ]
        , div [ class "input" ]
          [ div [] [ maybeApplyedOpFloat model.prevApply |> maybeFloatString |> text ]
          , input [ value (floatInputString model.activeValue), onInput Set ] []
          ]
        , div [ class "clearBtns"] 
          [ button [ onClick (Append AppDel) ] [ text "‹x" ] ]
        ]
      , div [ class "pad" ]
        [ div [ class "centerPad" ]
          [ div [ class "btnRow topBtns"]
            [ button [ onClick (Operation (Binary Add)) ] [ text "+" ]
            , button [ onClick (Operation (Binary Sub)) ] [ text "-" ]
            , button [ onClick (Operation (Binary Mul)) ] [ text "*" ]
            , button [ onClick (Operation (Binary Div)) ] [ text "/" ]
            , button [ onClick (Operation (Binary Pow)) ] [ text "^" ]
            ]
          , div [ class "midBtns" ]
            [ div [ class "btnRow" ]
              [ button [ onClick (Append App7) ] [ text "7" ]
              , button [ onClick (Append App8) ] [ text "8" ]
              , button [ onClick (Append App9) ] [ text "9" ]
              ]
            , div [ class "btnRow" ]
              [ button [ onClick (Append App4) ] [ text "4" ]
              , button [ onClick (Append App5) ] [ text "5" ]
              , button [ onClick (Append App6) ] [ text "6" ]
              ]
            , div [ class "btnRow" ]
              [ button [ onClick (Append App1) ] [ text "1" ]
              , button [ onClick (Append App2) ] [ text "2" ]
              , button [ onClick (Append App3) ] [ text "3" ]
              ]
            , div [ class "btnRow" ]
              [ button [ class "doubleWidth", onClick (Append App0) ] [ text "0" ]
              , button [ onClick (Append AppDot) ] [ text "." ]
              ]
            ]
          ]
        , div [ class "sidePad"]
          [ button [ onClick Clear ] [ text "c" ]
          , button [ onClick ClearAll ] [ text "ac" ]
          , button [ onClick (Operation (Unary Neg))] [ text "±" ]
          , button [ onClick (Operation (Unary Square))] [ text "x²" ]
          , button [ onClick (Operation (Unary MulInv))] [ text "x⁻¹" ]
          , button [ onClick (Apply) ] [ text "=" ]
          ]
        ]
      ]
    ]
