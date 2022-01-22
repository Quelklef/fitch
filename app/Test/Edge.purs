module Test.Edge where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (fromJust)
import Data.Array as Array
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (for_)
import Partial.Unsafe (unsafePartial)

import Fitch.Types (Proofy (..))
import Fitch.Decorate (decorate, toText)
import Fitch.Serialize (serialize)

main :: Effect Unit
main = do

  edgecase "Bug #13; ∀I w/ tricky variable replacement" """
    │ 1.
    ├──────────
    ││ 2. ∃xPx             assumed
    │├──────────
    │││ 3. [x]
    │││ 4. Px              assumed
    ││├──────────
    ││││ 5. [y]
    │││├──────────
    ││││ 6. Px             RI:4
    │││ 7. ∀xPx            invalid
    ││ 8. ∀xPx             ∃E:2,3-7
    │ 9. (∃xPx)→(∀xPx)     →I:2-8
  """

  edgecase "Bug #10; variable shadowing" """
    │ 1.
    ├──────────
    ││ 2. ∃xPx             assumed
    │├──────────
    │││ 3. [x]
    │││ 4. Px              assumed
    ││├──────────
    ││││ 5. [x]            'x' is shadowed
    │││├──────────
    ││││ 6. Px             RI:4
    │││ 7. ∀xPx            invalid
    ││ 8. ∀xPx             ∃E:2,3-7
    │ 9. (∃xPx)→(∀xPx)     →I:2-8
  """

  edgecase "DeMorgan's (∨)" """
    │ 1.
    ├──────────
    ││ 2. ¬(P∨Q)             assumed
    │├──────────
    │││ 3. P                 assumed
    ││├──────────
    │││ 4. P∨Q               ∨I:3
    │││ 5. (P∨Q)∧¬(P∨Q)      ∧I:4,2
    │││ 6. ⊥                 ⊥I:5
    ││ 7. ¬P                 ¬I:3-6
    │││ 8. Q                 assumed
    ││├──────────
    │││ 9. P∨Q               ∨I:8
    │││ 10. (P∨Q)∧¬(P∨Q)     ∧I:9,2
    │││ 11. ⊥                ⊥I:10
    ││ 12. ¬Q                ¬I:8-11
    ││ 13. ¬P∧¬Q             ∧I:7,12
    │
    ││ 14. ¬P∧¬Q             assumed
    │├──────────
    │││ 15. P∨Q              assumed
    ││├──────────
    ││││ 16. P               assumed
    │││├──────────
    ││││ 17. ¬P              ∧E:14
    ││││ 18. P∧¬P            ∧I:16,17
    ││││ 19. ⊥               ⊥I:18
    │││
    ││││ 20. Q               assumed
    │││├──────────
    ││││ 21. ¬Q              ∧E:14
    ││││ 22. Q∧¬Q            ∧I:20,21
    ││││ 23. ⊥               ⊥I:22
    │││ 24. ⊥                ∨E:15,16-19,20-23
    ││ 25. ¬(P∨Q)            ¬I:15-24
    │ 26. ¬(P∨Q)↔(¬P∧¬Q)     ↔I:2-13,14-25
  """

  edgecase "DeMorgan's (∧)" """
    │ 1.
    ├──────────
    ││ 2. ¬(P∧Q)                  assumed
    │├──────────
    │││ 3. ¬(¬P∨¬Q)               assumed
    ││├──────────
    ││││ 4. ¬P                    assumed
    │││├──────────
    ││││ 5. ¬P∨¬Q                 ∨I:4
    ││││ 6. (¬P∨¬Q)∧¬(¬P∨¬Q)      ∧I:5,3
    ││││ 7. ⊥                     ⊥I:6
    │││ 8. ¬¬P                    ¬I:4-7
    │││ 9. P                      ¬E:8
    ││││ 10. ¬Q                   assumed
    │││├──────────
    ││││ 11. ¬P∨¬Q                ∨I:10
    ││││ 12. (¬P∨¬Q)∧¬(¬P∨¬Q)     ∧I:11,3
    ││││ 13. ⊥                    ⊥I:12
    │││ 14. ¬¬Q                   ¬I:10-13
    │││ 15. Q                     ¬E:14
    │││ 16. P∧Q                   ∧I:9,15
    │││ 17. (P∧Q)∧¬(P∧Q)          ∧I:16,2
    │││ 18. ⊥                     ⊥I:17
    ││ 19. ¬¬(¬P∨¬Q)              ¬I:3-18
    ││ 20. ¬P∨¬Q                  ¬E:19
    │
    ││ 21. ¬P∨¬Q                  assumed
    │├──────────
    │││ 22. P∧Q                   assumed
    ││├──────────
    ││││ 23. ¬P                   assumed
    │││├──────────
    ││││ 24. P                    ∧E:22
    ││││ 25. P∧¬P                 ∧I:24,23
    ││││ 26. ⊥                    ⊥I:25
    │││
    ││││ 27. ¬Q                   assumed
    │││├──────────
    ││││ 28. Q                    ∧E:22
    ││││ 29. Q∧¬Q                 ∧I:28,27
    ││││ 30. ⊥                    ⊥I:29
    │││ 31. ⊥                     ∨E:21,23-26,27-30
    ││ 32. ¬(P∧Q)                 ¬I:22-31
    │ 33. ¬(P∧Q)↔(¬P∨¬Q)          ↔I:2-20,21-32
  """

  edgecase "DeMorgan's (∃)" """
    │ 1.
    ├──────────
    ││ 2. ¬∃xPx                assumed
    │├──────────
    │││ 3. [a]
    ││├──────────
    ││││ 4. Pa                 assumed
    │││├──────────
    ││││ 5. ∃xPx               ∃I:4[a→x]
    ││││ 6. (∃xPx)∧¬(∃xPx)     ∧I:5,2
    ││││ 7. ⊥                  ⊥I:6
    │││ 8. ¬Pa                 ¬I:4-7
    ││ 9. ∀x¬Px                ∀I:3-8[a→x]
    │
    ││ 10. ∀x¬Px               assumed
    │├──────────
    │││ 11. ∃xPx               assumed
    ││├──────────
    ││││ 12. [a]
    ││││ 13. Pa                assumed
    │││├──────────
    ││││ 14. ¬Pa               ∀E:10[x→a]
    ││││ 15. Pa∧¬Pa            ∧I:13,14
    ││││ 16. ⊥                 ⊥I:15
    │││ 17. ⊥                  ∃E:11,12-16
    ││ 18. ¬∃xPx               ¬I:11-17
    │ 19. (¬∃xPx)↔(∀x¬Px)      ↔I:2-9,10-18
  """

  edgecase "DeMorgan's (∀)" """
    │ 1.
    ├──────────
    ││ 2. ¬∀xPx                   assumed
    │├──────────
    │││ 3. ¬∃x¬Px                 assumed
    ││├──────────
    ││││ 4. [a]
    │││├──────────
    │││││ 5. ¬Pa                  assumed
    ││││├──────────
    │││││ 6. ∃x¬Px                ∃I:5[a→x]
    │││││ 7. (∃x¬Px)∧¬(∃x¬Px)     ∧I:6,3
    │││││ 8. ⊥                    ⊥I:7
    ││││ 9. ¬¬Pa                  ¬I:5-8
    ││││ 10. Pa                   ¬E:9
    │││ 11. ∀xPx                  ∀I:4-10[a→x]
    │││ 12. (∀xPx)∧¬(∀xPx)        ∧I:11,2
    │││ 13. ⊥                     ⊥I:12
    ││ 14. ¬¬∃x¬Px                ¬I:3-13
    ││ 15. ∃x¬Px                  ¬E:14
    │
    ││ 16. ∃x¬Px                  assumed
    │├──────────
    │││ 17. ∀xPx                  assumed
    ││├──────────
    ││││ 18. [a]
    ││││ 19. ¬Pa                  assumed
    │││├──────────
    ││││ 20. Pa                   ∀E:17[x→a]
    ││││ 21. Pa∧¬Pa               ∧I:20,19
    ││││ 22. ⊥                    ⊥I:21
    │││ 23. ⊥                     ∃E:16,18-22
    ││ 24. ¬∀xPx                  ¬I:17-23
    │ 25. (¬∀xPx)↔(∃x¬Px)         ↔I:2-15,16-24
  """


  where

  edgecase :: String -> String -> Effect Unit
  edgecase label proofString =
    let
      expected = trim4 proofString
      proof = decorate $ unsafeParseProof proofString
      actual = trim4 $ toText proof
      errors = linewiseDiff expected actual
               <#> (\(exp /\ act) -> "Expected " <> show exp <> " but got " <> show act)
    in do
      log $ "Case: " <> label <> " ..."
      if Array.length errors == 0
      then pure unit
      else do
        log "Failed!"
        for_ errors (("- " <> _) >>> log)
        log $ "Url: http://localhost:8000/?proof=" <> serialize (_.text <$> proof)
          -- ^ TODO: probably put this url-creation into some util module
        pure unit


foreign import trim4 :: String -> String


foreign import linewiseDiff_f
  :: { mkTup :: forall a b. a -> b -> a /\ b }
  -> String -> String -> Array (Nullable String /\ Nullable String)

linewiseDiff :: String -> String -> Array (Nullable String /\ Nullable String)
linewiseDiff = linewiseDiff_f { mkTup: (/\) }


foreign import parseProof_f
  :: { mkLine :: forall ln. ln -> Proofy ln
     , mkBlock :: forall ln. Array ln -> Array (Proofy ln) -> Proofy ln
     }
  -> String
  -> Nullable (Proofy String)

unsafeParseProof :: String -> Proofy String
unsafeParseProof string =
  unsafePartial $ fromJust $ Nullable.toMaybe $ parse string

  where
  parse = parseProof_f
    { mkLine: ProofLine
    , mkBlock: ProofBlock
    }
