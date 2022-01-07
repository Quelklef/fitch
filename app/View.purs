module Fitch.View where

import Prelude
import Effect (Effect)
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Foldable (intercalate)
import Data.Either (Either (..), either)
import Data.Monoid (guard)

import Html as Html
import Html (Html, button, div, span, text, input, label, p, pre, a)
import Attribute (Attribute, value, addClass, id, style, type_, checked, href, target, onInput, onClick, on, charset, rel)

import Fitch.Types (Proofy (..), Path, Model, Message (..), DecoratedLine, KnowledgeBox (..))
import Fitch.Proof as Proof
import Fitch.Path as Path
import Fitch.TextStyle as TextStyle
import Fitch.Formula as Formula
import Fitch.Decorate as Decorate
import Fitch.Serialize as Serialize
import Fitch.Util.ArrayUtil as ArrayUtil

view :: Model -> { head :: Array (Html Message), body :: Array (Html Message) }
view model =
  let proof = viewProof 0 model (Decorate.decorate model.proof)
  in template $
    div [ id "wrap" ]
    [ div [ id "left" ]
      [ Html.h1 [] [ text "Fitch-Style Proof Helper" ]
      , p [ addClass "options" ]
        [ checkbox (not model.useUnicode) ToggleUseUnicode "force plain symbols"
        , text " | "
        , checkbox model.showDebugInfo ToggleDebugMode "show debug info"
        , text " | "
        , button [ onClick CopyProofToClipboard ] [ text "copy proof to clipboard" ]
        , text " | "
        , button [ onClick (SetProofTo $ ProofBlock [""] []) ] [ text "reset proof" ]
        ]
      , if model.showDebugInfo then pre [ addClass "debug-info" ] [ text ("serialized: " <> Serialize.serialize model.proof) ] else text ""
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

    where
    template body =
      { head:
          [ Html.meta [ charset "utf-8" ]
          , Html.title "Fitch-style Proof Helper"
          , Html.link [ rel "stylesheet", type_ "text/css", href "./css.css" ]
          ]
      , body: [body]
      }

onKeydown :: ({ keyCode :: Int, shiftKey :: Boolean } ->  { msg :: Message, preventDefault :: Boolean }) -> Attribute Message
onKeydown respond =
    on "keydown" \event -> do
      let { msg, preventDefault } = respond (getKeyCodeShiftKey event)
      when preventDefault $ doPreventDefault event
      pure (Just msg)

foreign import getKeyCodeShiftKey :: forall ev. ev -> { keyCode :: Int, shiftKey :: Boolean }
foreign import doPreventDefault :: forall ev. ev -> Effect Unit

checkbox :: forall msg. Eq msg => Boolean -> msg -> String -> Html msg
checkbox isChecked msg name =
  label
    [ ]
    [ input [ type_ "checkbox", onClick msg ]
        -- ↑ n.b. checked state technically ought to be synced, but this works
    , text $ " " <> name
    ]

keyboardControlsHtml :: forall a. Boolean -> Html a
keyboardControlsHtml useUnicode =
  [ { keys: [".", "&", "*"], label: "conjunction (∧)" }
  , { keys: ["|",  "v", "+"], label: "disjunction (∨)" }
  , { keys: [">"], label: "implication (→)" }
  , { keys: ["-", "~", "!"], label: "negation (¬)" }
  , { keys: ["_", "#"], label: "bottom (⊥)" }
  , { keys: ["<>"], label: "biconditional (↔)" }
  , { keys: ["\\", "V"], label: "forall (∀)" }
  , { keys: ["E", "@"], label: "exists (∃)" }
  , { keys: ["="], label: "equals (=)" }
  , { keys: ["/="], label: "does not equal (≠)" }
  , { keys: ["enter"], label: "new line" }
  , { keys: ["shift+enter"], label: "additional assumption" }
  , { keys: ["tab"], label: "up a block" }
  , { keys: ["shift+tab"], label: "down a block" }
  ]
  # map (\{ keys, label } ->
    let keysHtml = keys >>= (\key -> [ span [ addClass "key" ] [ text key ], text " " ])
    in p [] ( keysHtml <> [ text $ TextStyle.map useUnicode label ] ))
  # div []

viewProof :: Int -> Model -> Proofy DecoratedLine -> Html Message
viewProof depth model proof = case proof of
  ProofLine { text, formula, path, lineno, knowledge, justification } ->
    let isValid = either (const false) (const true) justification
        isLastAssumption = Path.targetsLastAssumption model.proof path
    in div [ addClass "line", guard (not isValid) (addClass "--invalid"), guard isLastAssumption (addClass "--last-assumption") ]
      [ span [ addClass "line:number" ] [ Html.text $ show lineno ]
      , input
        [ addClass "line:input"
        , value (Formula.prettifyText model.useUnicode text)
        , id (Path.toId path)
        , onInput (SetFormulaAt path)
        , onKeydown (lineOnKeydown model.proof path)
        ]
      , span [ addClass "line:justification" ] [ (Html.text <<< TextStyle.map model.useUnicode) $ case justification of
          Right justn -> justn
          Left err -> err ]
      , if model.showDebugInfo
        then let info =
                   "path: " <> Path.pretty path <> "\n" <>
                   "formula: " <> (formula <#> (Formula.pretty >>> TextStyle.map model.useUnicode) # fromMaybe "(invalid)") <> "\n" <>
                   "knowledge: " <> prettifyKnowledge knowledge # TextStyle.map model.useUnicode
             in pre [ addClass "debug-info" ] [ Html.text info ]
        else Html.text ""
      ]

  ProofBlock head body ->
    let blockStyle =
          ArrayUtil.modGet depth blockColors
          # map (\{ backgroundColor, borderColor } -> style ( "background-color: " <> backgroundColor <> ";"
                                                           <> "border-color: " <> borderColor ))
          # fromMaybe mempty

    in div [ addClass "block", blockStyle ] $
      (<>)
        (head # map (ProofLine >>> viewProof (depth + 1) model))
        (body # map (viewProof (depth + 1) model))

prettifyKnowledge :: KnowledgeBox -> String
prettifyKnowledge (KnowledgeBox knowledge) =
  knowledge
  # map (\known -> case known of
    ProofLine line ->
      show line.lineno
    ProofBlock _ _ ->
      let getLineNo = map _.lineno >>> map show >>> fromMaybe "??"
      in (Proof.firstLine known # getLineNo) <> ".." <> (Proof.lastLine known # getLineNo))
  # intercalate ", "

blockColors :: Array { borderColor :: String, backgroundColor :: String }
blockColors =
  [ { borderColor: "rgb(100, 100, 100)", backgroundColor: "rgb(243, 243, 243)" }
  , { borderColor: "rgb(000, 000, 200)", backgroundColor: "rgb(236, 236, 251)" }
  , { borderColor: "rgb(255, 100, 100)", backgroundColor: "rgb(255, 243, 243)" }
  , { borderColor: "rgb(120, 255, 120)", backgroundColor: "rgb(245, 255, 245)" }
  , { borderColor: "rgb(120, 050, 120)", backgroundColor: "rgb(245, 240, 245)" }
  ]

lineOnKeydown :: Proofy String -> Path -> { keyCode :: Int, shiftKey :: Boolean } -> { msg :: Message, preventDefault :: Boolean }
lineOnKeydown wholeProof path { keyCode, shiftKey } =
  case keyCode /\ shiftKey of
    -- ↓ (Shift+)Enter pressed
    13 /\ _ ->
      let preferAssumption = shiftKey
      in { msg: NewLineAfter path preferAssumption, preventDefault: true }
    -- ↓ Tab pressed
    9 /\ false -> { msg: IndentAt path, preventDefault: true }
    -- ↓ Shift+Tab pressed
    9 /\ true -> { msg: DedentAt path, preventDefault: true }
    -- ↓ Up arrow key pressed
    38 /\ false ->
      let msg = Path.linearPred wholeProof path <#> (\newPath -> SetFocusTo newPath) # fromMaybe Noop
      in { msg: msg, preventDefault: true }
    -- ↓ Down arrow key pressed
    40 /\ false ->
      let msg = Path.linearSucc wholeProof path <#> (\newPath -> SetFocusTo newPath) # fromMaybe Noop
      in { msg: msg, preventDefault: true }
    -- ↓ Backspace key pressed
    8 /\ false -> { msg: BackspaceAt path, preventDefault: false }
    -- ↓ Any other key pressed
    _ -> { msg: Noop, preventDefault: false }

-- --

examplesHtml :: Boolean -> Html Message
examplesHtml useUnicode =
  [ { label: "DeMorgan's Law (∨)", proof:
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
        , ProofLine "-(P|Q)<>(-P.-Q)"
        ]
  }
  , { label: "DeMorgan's Law (∧)", proof:
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
        , ProofLine "-(P.Q)<>(-P|-Q)"
        ]
  }
  , { label: "DeMorgan's Law (∃)", proof:
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
        , ProofLine "(-ExPx)<>(Vx-Px)"
        ]
  }
  , { label: "DeMorgan's Law (∀)", proof:
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
        , ProofLine "(-VxPx)<>(Ex-Px)"
        ]
  }
  ]
  # makeExamples useUnicode
  # div []

rulesHtml :: Boolean -> Html Message
rulesHtml useUnicode =
  [ { label: "RI (reiteration): P ∴ P", proof: ProofBlock ["P"] [ProofLine "P"] }
  , { label: "∧I: P , Q ∴ P∧Q", proof: ProofBlock ["P", "Q"] [ProofLine "P.Q"] }
  , { label: "∧E: P∧Q ∴ P", proof: ProofBlock ["P.Q"] [ProofLine "P"] }
  , { label: "∧E: P∧Q ∴ Q", proof: ProofBlock ["P.Q"] [ProofLine "Q"] }
  , { label: "∨I: P ∴ P∨Q", proof: ProofBlock ["P"] [ProofLine "P|Q"] }
  , { label: "∨E: P∨Q , P⊢R , Q⊢R ∴ R", proof: ProofBlock ["P|(P.Q)"] [ProofBlock ["P"] [ProofLine "P"], ProofBlock ["P.Q"] [ProofLine "P"], ProofLine "P"] }
  , { label: "→I: P⊢Q ∴ P→Q", proof: ProofBlock ["Q"] [ProofBlock ["P"] [ProofLine "Q"], ProofLine "P>Q"] }
  , { label: "→E: P→Q , P ∴ Q", proof: ProofBlock ["P>Q", "P"] [ProofLine "Q"] }
  , { label: "↔I: P⊢Q , Q⊢P ∴ P↔Q", proof: ProofBlock ["P.Q"] [ProofBlock ["P"] [ProofLine "Q"], ProofBlock ["Q"] [ProofLine "P"], ProofLine "P<>Q"] }
  , { label: "↔E: P↔Q , P ∴ Q", proof: ProofBlock ["P<>Q", "P"] [ProofLine "Q"] }
  , { label: "↔E: P↔Q , Q ∴ P", proof: ProofBlock ["P<>Q", "Q"] [ProofLine "P"] }
  , { label: "⊥I: P∧¬P ∴ ⊥", proof: ProofBlock ["P.-P"] [ProofLine "#"] }
  , { label: "¬I: P⊢⊥ ∴ ¬P", proof: ProofBlock ["-(P|Q)"] [ProofBlock ["P"] [ProofLine "P|Q", ProofLine "(P|Q).-(P|Q)", ProofLine "#"], ProofLine "-P"] }
  , { label: "¬E: ¬¬P ∴ P", proof: ProofBlock ["--P"] [ProofLine "P"] }
  , { label: "∀I: [x]⊢Px ∴ ∀xPx", proof: ProofBlock [""] [ProofBlock ["[a]"] [ProofLine "a=a"], ProofLine "Vx x=x"] }
  , { label: "∀E: ∀xPx ∴ Py", proof: ProofBlock ["VxPx"] [ProofBlock ["[a]"] [ProofLine "Pa"]] }
  , { label: "∃I: Px ∴ ∃yPy", proof: ProofBlock [""] [ProofBlock ["[a]", "Pa"] [ProofLine "ExPx"]] }
  , { label: "∃E: ∃xPx , [y]Py⊢R ∴ R", proof: ProofBlock ["ExPx", "Vx(Px>Qx)"] [ProofBlock ["[a]", "Pa"] [ProofLine "Pa>Qa", ProofLine "Qa", ProofLine "ExQx"], ProofLine "ExQx"] }
  , { label: "NE (domain nonempty): ∃x", proof: ProofBlock [""] [ProofLine "Ex", ProofBlock ["[a]"] [ProofLine "a=a", ProofLine "Ex x=x"], ProofLine "Ex x=x"] }
  , { label: "=I: x=x", proof: ProofBlock ["[a]"] [ProofLine "a=a"] }
  , { label: "=E: Px , x=y ∴ Py", proof: ProofBlock ["[a]", "Pa"] [ProofBlock ["[b]", "a=b"] [ProofLine "Pb"]] }
  ]
  # makeExamples useUnicode
  # (\ar -> Array.snoc ar (p [] [ text $ TextStyle.map useUnicode "a≠b is treated as ¬(a=b)" ]))
  # div []

makeExamples useUnicode =
  map (\{ label, proof } -> p [] [ a [ onClick (SetProofTo proof) ] [ text $ TextStyle.map useUnicode label ] ])
