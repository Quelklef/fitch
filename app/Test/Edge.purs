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

  edgecase "Bug #10; name variable shadowing" """
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
    │││ 7. ∀xPx            ∀I:5-6[x→x]
    ││ 8. ∀xPx             ∃E:2,3-7
    │ 9. (∃xPx)→(∀xPx)     →I:2-8
  """

  edgecase "FOL variable shadowing" """
    │ 1. ∀x∃xP     'x' is shadowed
    ├──────────
  """

  edgecase "Bug #15; empty formula parsing" """
    │ 1. P∧     malformed
    │ 2. P∨     malformed
    │ 3. P→     malformed
    │ 4. P↔     malformed
    │ 5. ∀x     assumed
    │ 6. ∃x     assumed
    ├──────────
    │ 7.
  """

  edgecase "Bug #16; ∀I with no variable sub" """
    │ 1.          
    ├──────────
    ││ 2. [x]     
    │├──────────
    ││ 3. Px      invalid
    │ 4.          
    │ 5. ∀xPx     ∀I:2-3[x→x]
    │ 6. ∀aPa     ∀I:2-3[x→a]
  """

  edgecase "Cannot quantify over predicates" """
    │ 1. Q        assumed
    │ 2. ∀P Q     malformed
    ├──────────
    │ 3.          
  """

  edgecase "Free variables properly detected" """
    │ 1. [a]     
    │ 2. Pa      assumed
    │ 3. Pb      'b' is free
    ├──────────
    │ 4.         
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

  edgecase "Propositional packet #1" """
    │ 1.                               
    ├──────────
    ││ 2. A→((B→C)→D)                  assumed
    │├──────────
    │││ 3. ¬B                          assumed
    ││├──────────
    ││││ 4. A                          assumed
    │││├──────────
    ││││ 5. (B→C)→D                    →E:2,4
    │││││ 6. B                         assumed
    ││││├──────────
    ││││││ 7. ¬C                       assumed
    │││││├──────────
    ││││││ 8. B∧¬B                     ∧I:6,3
    ││││││ 9. ⊥                        ⊥I:8
    │││││ 10. ¬¬C                      ¬I:7-9
    │││││ 11. C                        ¬E:10
    ││││ 12. B→C                       →I:6-11
    ││││ 13. D                         →E:5,12
    │││ 14. A→D                        →I:4-13
    ││ 15. ¬B→(A→D)                    →I:3-14
    │ 16. (A→((B→C)→D))→(¬B→(A→D))     →I:2-15
  """

  edgecase "Propositional packet #2" """
    │ 1.                                 
    ├──────────
    ││ 2. P→(Q∧¬R)                       assumed
    │├──────────
    │││ 3. P                             assumed
    ││├──────────
    │││ 4. Q∧¬R                          →E:2,3
    │││ 5. Q                             ∧E:4
    ││ 6. P→Q                            →I:3-5
    │││ 7. ¬(¬P∨¬R)                      assumed
    ││├──────────
    ││││ 8. ¬P                           assumed
    │││├──────────
    ││││ 9. ¬P∨¬R                        ∨I:8
    ││││ 10. (¬P∨¬R)∧¬(¬P∨¬R)            ∧I:9,7
    ││││ 11. ⊥                           ⊥I:10
    │││ 12. ¬¬P                          ¬I:8-11
    │││ 13. P                            ¬E:12
    ││││ 14. ¬R                          assumed
    │││├──────────
    ││││ 15. ¬P∨¬R                       ∨I:14
    ││││ 16. (¬P∨¬R)∧¬(¬P∨¬R)            ∧I:15,7
    ││││ 17. ⊥                           ⊥I:16
    │││ 18. ¬¬R                          ¬I:14-17
    │││ 19. R                            ¬E:18
    │││ 20. Q∧¬R                         →E:2,13
    │││ 21. ¬R                           ∧E:20
    │││ 22. R∧¬R                         ∧I:19,21
    │││ 23. ⊥                            ⊥I:22
    ││ 24. ¬¬(¬P∨¬R)                     ¬I:7-23
    ││ 25. ¬P∨¬R                         ¬E:24
    ││ 26. (P→Q)∧(¬P∨¬R)                 ∧I:6,25
    │
    ││ 27. (P→Q)∧(¬P∨¬R)                 assumed
    │├──────────
    │││ 28. P                            assumed
    ││├──────────
    │││ 29. P→Q                          ∧E:27
    │││ 30. Q                            →E:29,28
    ││││ 31. R                           assumed
    │││├──────────
    │││││ 32. ¬P                         assumed
    ││││├──────────
    │││││ 33. P∧¬P                       ∧I:28,32
    │││││ 34. ⊥                          ⊥I:33
    ││││
    │││││ 35. ¬R                         assumed
    ││││├──────────
    │││││ 36. R∧¬R                       ∧I:31,35
    │││││ 37. ⊥                          ⊥I:36
    ││││ 38. ¬P∨¬R                       ∧E:27
    ││││ 39. ⊥                           ∨E:38,32-34,35-37
    │││ 40. ¬R                           ¬I:31-39
    │││ 41. Q∧¬R                         ∧I:30,40
    ││ 42. P→(Q∧¬R)                      →I:28-41
    │ 43. (P→(Q∧¬R))↔((P→Q)∧(¬P∨¬R))     ↔I:2-26,27-42
  """

  edgecase "Propositional packet #3" """
    │ 1.
    ├──────────
    ││ 2. B↔(A∨C)                                   assumed
    │├──────────
    │││ 3. ¬((B→A)∨(¬C→¬B))                         assumed
    ││├──────────
    ││││ 4. ¬C                                      assumed
    │││├──────────
    │││││ 5. B                                      assumed
    ││││├──────────
    │││││ 6. A∨C                                    ↔E:2,5
    ││││││ 7. A                                     assumed
    │││││├──────────
    │││││││ 8. B                                    assumed
    ││││││├──────────
    │││││││ 9. A                                    RI:7
    ││││││ 10. B→A                                  →I:8-9
    ││││││ 11. (B→A)∨(¬C→¬B)                        ∨I:10
    ││││││ 12. ((B→A)∨(¬C→¬B))∧¬((B→A)∨(¬C→¬B))     ∧I:11,3
    ││││││ 13. ⊥                                    ⊥I:12
    │││││
    ││││││ 14. C                                    assumed
    │││││├──────────
    ││││││ 15. C∧¬C                                 ∧I:14,4
    ││││││ 16. ⊥                                    ⊥I:15
    │││││ 17. ⊥                                     ∨E:6,7-13,14-16
    ││││ 18. ¬B                                     ¬I:5-17
    │││ 19. ¬C→¬B                                   →I:4-18
    │││ 20. (B→A)∨(¬C→¬B)                           ∨I:19
    │││ 21. ((B→A)∨(¬C→¬B))∧¬((B→A)∨(¬C→¬B))        ∧I:20,3
    │││ 22. ⊥                                       ⊥I:21
    ││ 23. ¬¬((B→A)∨(¬C→¬B))                        ¬I:3-22
    ││ 24. (B→A)∨(¬C→¬B)                            ¬E:23
    │ 25. (B↔(A∨C))→((B→A)∨(¬C→¬B))                 →I:2-24
  """

  edgecase "Predicate packet #1" """
    │ 1.
    ├──────────
    ││ 2. ∀y∃x¬Pxy                    assumed
    │├──────────
    │││ 3. ∃y∀xPxy                    assumed
    ││├──────────
    ││││ 4. [b]
    ││││ 5. ∀xPxb                     assumed
    │││├──────────
    ││││ 6. ∃x¬Pxb                    ∀E:2[y→b]
    │││││ 7. [a]
    │││││ 8. ¬Pab                     assumed
    ││││├──────────
    │││││ 9. Pab                      ∀E:5[x→a]
    │││││ 10. Pab∧¬Pab                ∧I:9,8
    │││││ 11. ⊥                       ⊥I:10
    ││││ 12. ⊥                        ∃E:6,7-11
    │││ 13. ⊥                         ∃E:3,4-12
    ││ 14. ¬∃y∀xPxy                   ¬I:3-13
    │
    ││ 15. ¬∃y∀xPxy                   assumed
    │├──────────
    │││ 16. [b]
    ││├──────────
    ││││ 17. ¬∃x¬Pxb                  assumed
    │││├──────────
    │││││ 18. [a]
    ││││├──────────
    ││││││ 19. ¬Pab                   assumed
    │││││├──────────
    ││││││ 20. ∃x¬Pxb                 ∃I:19[a→x]
    ││││││ 21. (∃x¬Pxb)∧¬(∃x¬Pxb)     ∧I:20,17
    ││││││ 22. ⊥                      ⊥I:21
    │││││ 23. ¬¬Pab                   ¬I:19-22
    │││││ 24. Pab                     ¬E:23
    ││││ 25. ∀xPxb                    ∀I:18-24[a→x]
    ││││ 26. ∃y∀xPxy                  ∃I:25[b→y]
    ││││ 27. (∃y∀xPxy)∧¬(∃y∀xPxy)     ∧I:26,15
    ││││ 28. ⊥                        ⊥I:27
    │││ 29. ¬¬∃x¬Pxb                  ¬I:17-28
    │││ 30. ∃x¬Pxb                    ¬E:29
    ││ 31. ∀y∃x¬Pxy                   ∀I:16-30[b→y]
    │ 32. (∀y∃x¬Pxy)↔(¬∃y∀xPxy)       ↔I:2-14,15-31
  """

  edgecase "Predicate packet #2" """
    │ 1.
    ├──────────
    ││ 2. ¬∀x∃y(Px→Qy)                           assumed
    │├──────────
    │││ 3. ¬∃x∀y(Px∧¬Qy)                         assumed
    ││├──────────
    ││││ 4. [a]
    │││├──────────
    │││││ 5. ¬∃y(Pa→Qy)                          assumed
    ││││├──────────
    ││││││ 6. [b]
    │││││├──────────
    │││││││ 7. Pa→Qb                             assumed
    ││││││├──────────
    │││││││ 8. ∃y(Pa→Qy)                         ∃I:7[b→y]
    │││││││ 9. (∃y(Pa→Qy))∧¬(∃y(Pa→Qy))          ∧I:8,5
    │││││││ 10. ⊥                                ⊥I:9
    ││││││ 11. (Pa→Qb)→⊥                         →I:7-10
    │││││││ 12. ¬Pa                              assumed
    ││││││├──────────
    ││││││││ 13. Pa                              assumed
    │││││││├──────────
    │││││││││ 14. ¬Qb                            assumed
    ││││││││├──────────
    │││││││││ 15. Pa∧¬Pa                         ∧I:13,12
    │││││││││ 16. ⊥                              ⊥I:15
    ││││││││ 17. ¬¬Qb                            ¬I:14-16
    ││││││││ 18. Qb                              ¬E:17
    │││││││ 19. Pa→Qb                            →I:13-18
    │││││││ 20. ⊥                                →E:11,19
    ││││││ 21. ¬¬Pa                              ¬I:12-20
    ││││││ 22. Pa                                ¬E:21
    │││││││ 23. Qb                               assumed
    ││││││├──────────
    ││││││││ 24. Pa                              assumed
    │││││││├──────────
    ││││││││ 25. Qb                              RI:23
    │││││││ 26. Pa→Qb                            →I:24-25
    │││││││ 27. ⊥                                →E:11,26
    ││││││ 28. ¬Qb                               ¬I:23-27
    ││││││ 29. Pa∧¬Qb                            ∧I:22,28
    │││││ 30. ∀y(Pa∧¬Qy)                         ∀I:6-29[b→y]
    │││││ 31. ∃x∀y(Px∧¬Qy)                       ∃I:30[a→x]
    │││││ 32. (∃x∀y(Px∧¬Qy))∧¬(∃x∀y(Px∧¬Qy))     ∧I:31,3
    │││││ 33. ⊥                                  ⊥I:32
    ││││ 34. ¬¬∃y(Pa→Qy)                         ¬I:5-33
    ││││ 35. ∃y(Pa→Qy)                           ¬E:34
    │││ 36. ∀x∃y(Px→Qy)                          ∀I:4-35[a→x]
    │││ 37. (∀x∃y(Px→Qy))∧¬(∀x∃y(Px→Qy))         ∧I:36,2
    │││ 38. ⊥                                    ⊥I:37
    ││ 39. ¬¬∃x∀y(Px∧¬Qy)                        ¬I:3-38
    ││ 40. ∃x∀y(Px∧¬Qy)                          ¬E:39
    │ 41. (¬∀x∃y(Px→Qy))→(∃x∀y(Px∧¬Qy))          →I:2-40
  """

  edgecase "Predicate packet #3" """
    │ 1.
    ├──────────
    ││ 2. ∃x∀y(yRx↔¬∃z(yRz∧zRy))              assumed
    │├──────────
    │││ 3. [a]
    │││ 4. ∀y(yRa↔¬∃z(yRz∧zRy))               assumed
    ││├──────────
    │││ 5. aRa↔¬∃z(aRz∧zRa)                   ∀E:4[y→a]
    ││││ 6. ¬∃z(aRz∧zRa)                      assumed
    │││├──────────
    ││││ 7. aRa                               ↔E:5,6
    ││││ 8. aRa∧aRa                           ∧I:7,7
    ││││ 9. ∃z(aRz∧zRa)                       ∃I:8[a→z]
    ││││ 10. (∃z(aRz∧zRa))∧¬(∃z(aRz∧zRa))     ∧I:9,6
    ││││ 11. ⊥                                ⊥I:10
    │││ 12. ¬¬∃z(aRz∧zRa)                     ¬I:6-11
    │││ 13. ∃z(aRz∧zRa)                       ¬E:12
    ││││ 14. [b]
    ││││ 15. aRb∧bRa                          assumed
    │││├──────────
    ││││ 16. aRb                              ∧E:15
    ││││ 17. bRa                              ∧E:15
    ││││ 18. bRa∧aRb                          ∧I:17,16
    ││││ 19. ∃z(bRz∧zRb)                      ∃I:18[a→z]
    ││││ 20. bRa↔¬∃z(bRz∧zRb)                 ∀E:4[y→b]
    ││││ 21. ¬∃z(bRz∧zRb)                     ↔E:20,17
    ││││ 22. (∃z(bRz∧zRb))∧¬(∃z(bRz∧zRb))     ∧I:19,21
    ││││ 23. ⊥                                ⊥I:22
    │││ 24. ⊥                                 ∃E:13,14-23
    ││ 25. ⊥                                  ∃E:2,3-24
    │ 26. ¬∃x∀y(yRx↔¬∃z(yRz∧zRy))             ¬I:2-25
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
