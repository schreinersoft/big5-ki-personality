Residualmatrix sollte wenig Werte 

# Chi-Quadrat
Der Chi-Quadrat-Wert in der principal()-Analyse testet die Modellpassung (Goodness of Fit):
Was wird getestet?
Nullhypothese (H₀): Das Faktorenmodell passt perfekt zu den Daten
Alternativhypothese (H₁): Das Modell passt nicht perfekt
Interpretation:
Niedriger Chi-Quadrat-Wert + hoher p-Wert (> 0.05):

✅ Gute Modellpassung
Nullhypothese wird nicht verworfen
Das Modell reproduziert die Korrelationsmatrix gut

Hoher Chi-Quadrat-Wert + niedriger p-Wert (< 0.05):

❌ Schlechte Modellpassung
Nullhypothese wird verworfen
Das Modell weicht signifikant von den Daten ab

## Wichtige Hinweise:

Sensitivität: Chi-Quadrat reagiert sehr empfindlich auf große Stichproben
Bei großen N: Oft signifikant, auch bei akzeptabler Passung
Ergänzende Maße: Daher immer auch RMSEA, TLI etc. betrachten
Praxis: Ein nicht-signifikanter Chi-Quadrat ist wünschenswert, aber nicht immer realistisch

Der Chi-Quadrat-Test sollte daher nie isoliert, sondern immer im Kontext anderer Fit-Maße interpretiert werden.