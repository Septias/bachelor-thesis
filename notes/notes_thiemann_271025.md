
* [x] Mit "type system" haben die ersten Abschnitte von 2. noch nichts zu tun. Erst definierst du die Syntax (2.1 - 2.4), dann die Reduktionssemantik (2.5 und 2.6 - dort fehlen noch die Kontextregeln, mit denen du die evaluation order festlegst), den Titel type system würde ich erst ab 2.7 verwenden.

* [~] du hast call-by-need erwähnt. Das ist formal ziemlich nervig und ich würde daher davon abraten (es sei denn du willst etwas über Effizienz beweisen). Stattdessen würde ich call-by-name verwenden. Das ist nämlich observationally equivalent zu call-by-need und formal viel einfacher.

* [ ] in 2.5 ist bei einigen Regeln unklar, welche Indizes eine Wiederholung abkürzen sollen. Manche Autoren verwenden ein overline um das anzudeuten. Beispiel: bei R-Fun-Pat-Default ist mir nicht mehr klar, was genau passieren soll. Mittlerweile ist mir klar, was du willst, aber die Nebenbedingung kannst du so nicht hinschreiben.

* [x] was soll das "\l" in R-Lookup-Null? Es ist keine Syntax und ich denke du willst sagen, dass l nicht in dem Record vorkommt. Also so was wie `{ l_i:t_i } .l -> null  if not exists j, l_j = l
(der "removed" Operator funktioniert da nicht, auch nicht "I use the syntax to create")

* [x] R-Let: l_i statt v_i
* [x] R-With: statt /= soll es wohl := heissen? In R-Let, R-Fun-Par-Default wird tw nur = verwendet. Soll das was anderes sein? (bzgl /= ich verstehe nicht wie existierende bindings nicht verborgen werden)
-> Dafür der neue Operator (jetzt ⊜)

* [x] R-Array-Concat: auf der rechten Seite sollten doch schon Arrays stehen? Das kommt hier nicht zum Ausdruck
* [x] R-Record-Concat: dito, 

* 2.7 Unterschied { ...} und <...>
* [x] option type: du braucht keinen allgemeinen option type, sondern nur eine Typsyntax für Funktionsargumente, die wie patterns funktioniert. D.h. im record type darf auch l_i :? \tau vorkommen, aber nur in Funktionsargumenten.

* [x] Patterns und Kinds verstehe ich gerade nicht

## Typing Rules

* [x] in der Typsyntax kommt kein \forall vor, aber in 2.8 schon.

* [~] T-Abs umfasst nicht den Fall von R-Fun-Pat

* [x] T-Check die assumption sollte sein e : {}; aktuell wird ja verlangt, dass das Label l vorhanden ist.

* [x] T-Or so verlangt die assumption wieder, dass Label l vorhanden ist: wenn es vorhanden ist, dann ist der Typ $τ_1$; andernfalls kommt die Regel nicht zur Anwendung! Man könnte höchstens eine weitere Regel machen mit t1 : {} und Ergebnistyp $τ_2$. Ob das so funktioniert, kann ich so nicht sagen. Da muss man die Beweise durchgehen.

* [x] T-Lst-Hom und T-Lst-Agg schliessen sich auch nicht gegenseitig aus. muss man auch schauen ob die Metatheorie damit durchgeht.

* [x] T-Rec-Concat: die Operatoren in a\b\cup b sind nicht definiert.

* [~] T-Multi-Let: woher kommen die \forall? Hier kannst du eine GEN Funktion a la Hindley-Milner verwenden.
-> TODO: was ist eine GEN Funktion?

* [x] T-With: die Typregel spricht nur über Records, die sich ohne Konflikt auspacken lassen. Ich sehe jetzt gerade nicht, wie das zur Reduktionsregel passt. Sehe gerade den Kommentar: ich denke, das funktioniert so wie es ist, weil das Subtyping die Labels rausnehmen kann, die mit Bindings in \Gamma in Konflikt stehen.

* [x] T-Assert: was heisst As<bool>?

* [x] S-Depth: was mach das Dreieck noch? und was das andere Dreieck in S-Assume?

* [x] S-Weaken, S-Assume, S-Hyp: was ist H? bei welchen Regeln wird \Sigma erweitert?

* [x] C-Var-* verstehe ich nicht. Muss es nicht einen neuen upper bound geben?

* [x] das mit dem @ und extrude verstehe ich nicht

* [x] C-*-Var: hier ist der lower bound korrekt.

* [~] 5.3 das ist kein neue Problem. Man könnte ein Refinement des String types machen und dort _bis zu einem gewissen Grad den Wert des Strings nachhalten_. Das ist nicht so schwer, aber man muss auch den Fall handhaben, dass eben kein bestimmter Wert herauskommt und dann ist der Ergebnistyp der LUB über alle Feldtypen des Records...

* [x] 6.4 würde ich erstmal ignorieren.
