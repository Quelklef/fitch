module View exposing (..)

import Types exposing (Model, Message)

import Html exposing (Html, Attribute, button, div, span, text, input, label, p, pre, a)
import Html.Attributes exposing (value, class, id, style, type_, checked, href, target)
import Html.Events exposing (onInput, keyCode, preventDefaultOn, onClick)
import Json.Decode as Json
import Url

import ListUtil
import StringUtil

import Types exposing (Proofy(..), Path, Model, Message(..), DecoratedLine, KnowledgeBox(..))
import Proof
import Path
import Semantics
import TextStyle
import Formula
import Decorate
import Serialize

view : Model -> Html Message
view model =
  let proof = viewProof 0 model (Decorate.decorate model.proof)
  in
    div [ id "wrap" ]
    [ div [ id "left" ]
      [ Html.h1 [] [ text "Fitch-Style Proof Helper" ]
      , p [ class "options" ]
        [ checkbox (not model.useUnicode) ToggleUseUnicode "force plain symbols"
        , text " | "
        , checkbox model.showDebugInfo ToggleDebugMode "show debug info"
        , text " | "
        , button [ onClick CopyProofToClipboard ] [ text "copy proof to clipboard" ]
        , text " | "
        , button [ onClick (SetProofTo <| ProofBlock [""] []) ] [ text "reset proof" ]
        ]
      , if model.showDebugInfo then pre [ class "debug-info" ] [ text ("serialized: " ++ Serialize.serialize model.proof) ] else text ""
      , proof
      ]
    , div [ id "right" ]
      [ Html.h3 [] [ text "Usage" ]
      , keyboardControlsHtml model.useUnicode
      , Html.h3 [] [ text "Examples" ]
      , examplesHtml model.useUnicode
      , Html.h3 [] [ text "Rules (click for example)" ]
      , rulesHtml model.useUnicode
      , Html.h3 [] [ text "Github" ]
      , p [] [ a [ target "_blank", href "https://github.com/quelklef/fitch" ] [ text "Github" ] ]
      ]
    ]

-- vv Modified from https://stackoverflow.com/a/41072936/4608364 and https://stackoverflow.com/a/61734163/4608364
onKeydown : ({ keyCode : Int, shiftKey : Bool } ->  { msg : Message, preventDefault : Bool }) -> Attribute Message
onKeydown respond =
    let getInfo = Json.map2 (\keyCode shiftKey -> { keyCode = keyCode, shiftKey = shiftKey })
          (Json.field "keyCode" Json.int)
          (Json.field "shiftKey" Json.bool)
        tupleRespond = respond >> (\{ msg, preventDefault } -> (msg, preventDefault))
    in preventDefaultOn "keydown" (getInfo |> Json.andThen (\info -> Json.succeed <| tupleRespond info))

-- vv Modified from https://github.com/dwyl/learn-elm/blob/master/examples/checkboxes.elm
checkbox : Bool -> msg -> String -> Html msg
checkbox isChecked msg name =
  label
    [ ]
    [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
    , text <| " " ++ name
    ]

keyboardControlsHtml : Bool -> Html a
keyboardControlsHtml useUnicode =
  [ { keys = [".", "&", "*"], label = "conjunction (∧)" }
  , { keys = ["|",  "v", "+"], label = "disjunction (∨)" }
  , { keys = ["->"], label = "implication (→)" }
  , { keys = ["-", "~", "!"], label = "negation (¬)" }
  , { keys = ["_", "#"], label = "bottom (⊥)" }
  , { keys = ["<->"], label = "biconditional (↔)" }
  , { keys = ["\\", "V"], label = "forall (∀)" }
  , { keys = ["E", "@"], label = "exists (∃)" }
  , { keys = ["="], label = "equals (=)" }
  , { keys = ["/="], label = "does not equal (≠)" }
  , { keys = ["enter"], label = "new line" }
  , { keys = ["shift+enter"], label = "additional assumption" }
  , { keys = ["tab"], label = "up a block" }
  , { keys = ["shift+tab"], label = "down a block" }
  ]
  |> List.map (\{ keys, label } ->
    let keysHtml = keys |> ListUtil.flatMap (\key -> [ span [ class "key" ] [ text key ], text " " ])
    in p [] ( keysHtml ++ [ text <| TextStyle.map useUnicode label ] ))
  |> div []

viewProof : Int -> Model -> Proofy DecoratedLine -> Html Message
viewProof depth model proof = case proof of
  ProofLine { text, formula, path, lineno, knowledge, justification } ->
    let isValid = justification |> Result.map (always True) |> Result.withDefault False
        isLastAssumption = Path.targetsLastAssumption model.proof path
    in div [ class <| "line" ++ StringUtil.if_ (not isValid) " --invalid" ++ StringUtil.if_ isLastAssumption " --last-assumption" ]
      [ span [ class "line:number" ] [ Html.text <| String.fromInt lineno ]
      , input
        [ class "line:input"
        , value (Formula.prettifyText model.useUnicode text)
        , id (Path.toId path)
        , onInput (SetFormulaAt path)
        , onKeydown (lineOnKeydown model.proof path)
        ] []
      , span [ class "line:justification" ] [ (Html.text << TextStyle.map model.useUnicode) <| case justification of
          Ok justn -> justn
          Err err -> err ]
      , if model.showDebugInfo
        then let info =
                   "path: " ++ Path.pretty path ++ "\n" ++
                   "formula: " ++ (formula |> Maybe.map (Formula.pretty >> TextStyle.map model.useUnicode) |> Maybe.withDefault "(invalid)") ++ "\n" ++
                   "knowledge: " ++ prettifyKnowledge knowledge |> TextStyle.map model.useUnicode
             in pre [ class "debug-info" ] [ Html.text info ]
        else Html.text ""
      ]

  ProofBlock head body ->
    let blockStyle =
          ListUtil.modGet depth blockColors
          |> Maybe.map (\{ backgroundColor, borderColor } -> [ style "background-color" backgroundColor, style "border-color" borderColor ])
          |> Maybe.withDefault []

    in div ([ class "block" ] ++ blockStyle) <|
      List.append
        (head |> List.map (ProofLine >> viewProof (depth + 1) model))
        (body |> List.map (viewProof (depth + 1) model))

prettifyKnowledge : KnowledgeBox -> String
prettifyKnowledge (KnowledgeBox knowledge) =
  knowledge
  |> List.map (\known -> case known of
    ProofLine line ->
      String.fromInt line.lineno ++ "."
      ++ (line.formula
      |> Maybe.map Formula.pretty
      |> Maybe.withDefault "??")
    ProofBlock _ _ ->
      let getLineNo = Maybe.map .lineno >> Maybe.map String.fromInt >> Maybe.withDefault "??"
      in (Proof.firstLine known |> getLineNo) ++ ".." ++ (Proof.lastLine known |> getLineNo))
  |> String.join " and "

blockColors : List { borderColor: String, backgroundColor : String }
blockColors =
  [ { borderColor = "rgb(100, 100, 100)", backgroundColor = "rgb(243, 243, 243)" }
  , { borderColor = "rgb(000, 000, 200)", backgroundColor = "rgb(236, 236, 251)" }
  , { borderColor = "rgb(255, 100, 100)", backgroundColor = "rgb(255, 243, 243)" }
  , { borderColor = "rgb(120, 255, 120)", backgroundColor = "rgb(245, 255, 245)" }
  , { borderColor = "rgb(120, 050, 120)", backgroundColor = "rgb(245, 240, 245)" }
  ]

lineOnKeydown : Proofy String -> Path -> { keyCode : Int, shiftKey : Bool } -> { msg : Message, preventDefault : Bool }
lineOnKeydown wholeProof path { keyCode, shiftKey } =
  case (keyCode, shiftKey) of
    -- vv (Shift+)Enter pressed
    (13, _) ->
      let preferAssumption = shiftKey
      in { msg = NewLineAfter path preferAssumption, preventDefault = True }
    -- vv Tab pressed
    (9, False) -> { msg = IndentAt path, preventDefault = True }
    -- vv Shift+Tab pressed
    (9, True) -> { msg = DedentAt path, preventDefault = True }
    -- vv Up arrow key pressed
    (38, False) ->
      let msg = Path.linearPred wholeProof path |> Maybe.map (\newPath -> SetFocusTo newPath) |> Maybe.withDefault Noop
      in { msg = msg, preventDefault = True }
    -- vv Down arrow key pressed
    (40, False) ->
      let msg = Path.linearSucc wholeProof path |> Maybe.map (\newPath -> SetFocusTo newPath) |> Maybe.withDefault Noop
      in { msg = msg, preventDefault = True }
    -- vv Backspace key pressed
    (8, False) -> { msg = BackspaceAt path, preventDefault = False }
    -- vv Any other key pressed
    _ -> { msg = Noop, preventDefault = False }

-- --

examplesHtml : Bool -> Html Message
examplesHtml useUnicode =
  [ { label = "DeMorgan's Law (∨)", proof =
        ProofBlock
        [ "" ]
        [ ProofBlock
          [ "-(P|Q)" ]
          [ ProofBlock
            [ "P" ]
            [ ProofLine "P|Q"
            , ProofLine "(P|Q).-(P|Q)"
            , ProofLine "#"
            ]
          , ProofLine "-P"
          , ProofBlock
            [ "Q" ]
            [ ProofLine "P|Q"
            , ProofLine "(P|Q).-(P|Q)"
            , ProofLine "#"
            ]
          , ProofLine "-Q"
          , ProofLine "-P.-Q"
          ]
        , ProofBlock
          [ "-P.-Q" ]
          [ ProofBlock
            [ "P|Q" ]
            [ ProofBlock
              [ "P" ]
              [ ProofLine "-P"
              , ProofLine "P.-P"
              , ProofLine "#"
              ]
            , ProofBlock
              [ "Q" ]
              [ ProofLine "-Q"
              , ProofLine "Q.-Q"
              , ProofLine "#"
              ]
            , ProofLine "#"
            ]
          , ProofLine "-(P|Q)"
          ]
        , ProofLine "-(P|Q)<->(-P.-Q)"
        ]
  }
  , { label = "DeMorgan's Law (∧)", proof =
        ProofBlock
        [ "" ]
        [ ProofBlock
          [ "-(P.Q)" ]
          [ ProofBlock
            [ "-(-P|-Q)" ]
            [ ProofBlock
              [ "-P" ]
              [ ProofLine "-P|-Q"
              , ProofLine "(-P|-Q).-(-P|-Q)"
              , ProofLine "#"
              ]
            , ProofLine "--P"
            , ProofLine "P"
            , ProofBlock
              [ "-Q" ]
              [ ProofLine "-P|-Q"
              , ProofLine "(-P|-Q).-(-P|-Q)"
              , ProofLine "#"
              ]
            , ProofLine "--Q"
            , ProofLine "Q"
            , ProofLine "P.Q"
            , ProofLine "(P.Q).-(P.Q)"
            , ProofLine "#"
            ]
          , ProofLine "--(-P|-Q)"
          , ProofLine "-P|-Q"
          ]
        , ProofBlock
          [ "-P|-Q" ]
          [ ProofBlock
            [ "P.Q" ]
            [ ProofBlock
              [ "-P" ]
              [ ProofLine "P"
              , ProofLine "P.-P"
              , ProofLine "#"
              ]
            , ProofBlock
              [ "-Q" ]
              [ ProofLine "Q"
              , ProofLine "Q.-Q"
              , ProofLine "#"
              ]
            , ProofLine "#"
            ]
          , ProofLine "-(P.Q)"
          ]
        , ProofLine "-(P.Q)<->(-P|-Q)"
        ]
  }
  , { label = "DeMorgan's Law (∃)", proof =
        ProofBlock
        [ "" ]
        [ ProofBlock
          [ "-ExPx" ]
          [ ProofBlock
            [ "[a]" ]
            [ ProofBlock
              [ "Pa" ]
              [ ProofLine "ExPx"
              , ProofLine "(ExPx).-(ExPx)"
              , ProofLine "#"
              ]
            , ProofLine "-Pa"
            ]
          , ProofLine "Vx-Px"
          ]
        , ProofBlock
          [ "Vx-Px" ]
          [ ProofBlock
            [ "ExPx" ]
            [ ProofBlock
              [ "[a]", "Pa" ]
              [ ProofLine "-Pa"
              , ProofLine "Pa.-Pa"
              , ProofLine "#"
              ]
            , ProofLine "#"
            ]
          , ProofLine "-ExPx"
          ]
        , ProofLine "(-ExPx)<->(Vx-Px)"
        ]
  }
  , { label = "DeMorgan's Law (∀)", proof =
        ProofBlock
        [ "" ]
        [ ProofBlock
          [ "-VxPx" ]
          [ ProofBlock
            [ "-Ex-Px" ]
            [ ProofBlock
              [ "[a]" ]
              [ ProofBlock
                [ "-Pa" ]
                [ ProofLine "Ex-Px"
                , ProofLine "(Ex-Px).-(Ex-Px)"
                , ProofLine "#"
                ]
              , ProofLine "--Pa"
              , ProofLine "Pa"
              ]
            , ProofLine "VxPx"
            , ProofLine "(VxPx).-(VxPx)"
            , ProofLine "#"
            ]
          , ProofLine "--Ex-Px"
          , ProofLine "Ex-Px"
          ]
        , ProofBlock
          [ "Ex-Px" ]
          [ ProofBlock
            [ "VxPx" ]
            [ ProofBlock
              [ "[a]", "-Pa" ]
              [ ProofLine "Pa"
              , ProofLine "Pa.-Pa"
              , ProofLine "#"
              ]
            , ProofLine "#"
            ]
          , ProofLine "-VxPx"
          ]
        , ProofLine "(-VxPx)<->(Ex-Px)"
        ]
  }
  ]
  |> makeExamples useUnicode
  |> div []

rulesHtml : Bool -> Html Message
rulesHtml useUnicode =
  [ { label = "RI (reiteration): P ∴ P", proof = ProofBlock ["P"] [ProofLine "P"] }
  , { label = "∧I: P , Q ∴ P∧Q", proof = ProofBlock ["P", "Q"] [ProofLine "P.Q"] }
  , { label = "∧E: P∧Q ∴ P", proof = ProofBlock ["P.Q"] [ProofLine "P"] }
  , { label = "∧E: P∧Q ∴ Q", proof = ProofBlock ["P.Q"] [ProofLine "Q"] }
  , { label = "∨I: P ∴ P∨Q", proof = ProofBlock ["P"] [ProofLine "P|Q"] }
  , { label = "∨E: P∨Q , P⊢R , Q⊢R ∴ R", proof = ProofBlock ["P|(P.Q)"] [ProofBlock ["P"] [ProofLine "P"], ProofBlock ["P.Q"] [ProofLine "P"], ProofLine "P"] }
  , { label = "→I: P⊢Q ∴ P→Q", proof = ProofBlock ["Q"] [ProofBlock ["P"] [ProofLine "Q"], ProofLine "P->Q"] }
  , { label = "→E: P→Q , P ∴ Q", proof = ProofBlock ["P->Q", "P"] [ProofLine "Q"] }
  , { label = "↔I: P⊢Q , Q⊢P ∴ P↔Q", proof = ProofBlock ["P.Q"] [ProofBlock ["P"] [ProofLine "Q"], ProofBlock ["Q"] [ProofLine "P"], ProofLine "P<->Q"] }
  , { label = "↔E: P↔Q , P ∴ Q", proof = ProofBlock ["P<->Q", "P"] [ProofLine "Q"] }
  , { label = "↔E: P↔Q , Q ∴ P", proof = ProofBlock ["P<->Q", "Q"] [ProofLine "P"] }
  , { label = "⊥I: P∧¬P ∴ ⊥", proof = ProofBlock ["P.-P"] [ProofLine "#"] }
  , { label = "¬I: P⊢⊥ ∴ ¬P", proof = ProofBlock ["-(P|Q)"] [ProofBlock ["P"] [ProofLine "P|Q", ProofLine "(P|Q).-(P|Q)", ProofLine "#"], ProofLine "-P"] }
  , { label = "¬E: ¬¬P ∴ P", proof = ProofBlock ["--P"] [ProofLine "P"] }
  , { label = "∀I: [x]⊢Px ∴ ∀xPx", proof = ProofBlock [""] [ProofBlock ["[a]"] [ProofLine "a=a"], ProofLine "Vx x=x"] }
  , { label = "∀E: ∀xPx ∴ Py", proof = ProofBlock ["VxPx"] [ProofBlock ["[a]"] [ProofLine "Pa"]] }
  , { label = "∃I: Px ∴ ∃yPy", proof = ProofBlock [""] [ProofBlock ["[a]", "Pa"] [ProofLine "ExPx"]] }
  , { label = "∃E: ∃xPx , [y]Py⊢R ∴ R", proof = ProofBlock ["ExPx", "VxPx->Qx"] [ProofBlock ["[a]", "Pa"] [ProofLine "Pa->Qa", ProofLine "Qa", ProofLine "ExQx"], ProofLine "ExQx"] }
  , { label = "NE (domain nonempty): [x]⊢P ∴ P", proof = ProofBlock [""] [ProofBlock ["[a]"] [ProofLine "a=a", ProofLine "Ex x=x"], ProofLine "Ex x=x"] }
  , { label = "=I: x=x", proof = ProofBlock ["[a]"] [ProofLine "a=a"] }
  , { label = "=E: Px , x=y ∴ Py", proof = ProofBlock ["[a]", "Pa"] [ProofBlock ["[b]", "a=b"] [ProofLine "Pb"]] }
  ]
  |> makeExamples useUnicode
  |> ListUtil.push (p [] [ text <| TextStyle.map useUnicode "a≠b is treated as ¬(a=b)" ])
  |> div []

makeExamples useUnicode =
  List.map (\{ label, proof } -> p [] [ a [ onClick (SetProofTo proof) ] [ text <| TextStyle.map useUnicode label ] ])
