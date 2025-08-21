Es gibt mehrere Möglichkeiten der Verbesserung des Modells, von denen nur einzelne ausprobiert werden und bei einer Verbesserung weiterbenutzt werden:
* Verbesserung des Prompt Templates: Einführungtext besser / anders
* Benutzung von mehr Facetten
* 8-Punkt-Skala gegen Tendenz zur Mitte
* Auswahl der Facetten je nach Ergebnis: "best of"
* Auch möglichkeit, schlecht messbare Faktoren weg zu lassen! (NEO als Kern?)


# Version 1
* Prompt: Prompt 1
* Modell: gpt-5-mini-2025-08-07
* Essays: 1 bis 50
* Wiederholungen pro Essay: 10
* Kosten: $1.60 
* 5 * 3 BFI-2 Facetten

# Version 2
* Prompt: Prompt 2
* Modell: gpt-5-mini-2025-08-07
* Essays: 1 bis 50
* Wiederholungen pro Essay: 5
* Nutzung einer function
* Kosten: ca. $0.80 
- Function-Nutzung unklar innere Funktion: nicht mehr nehmen!
- Komplett fehlende Normalverteilung und ANOVA-unterschiede. Sehr "chaotisches" Ergebnis: wird verworfen!

# Version 3
* Prompt: Prompt 3: wie Prompt 1, aber mit mehr Facetten
* Modell: gpt-5-mini-2025-08-07
* Essays: 1 bis 50
* Wiederholungen pro Essay: 10
* Kosten: $1.46 
* 5 * 6 NEO-PI-R-Facetten
of1 = 'Fantasy'
of2 = 'Aesthetics'
of3 = 'Feelings'
of4 = 'Actions'
of5 = 'Ideas'
of6 = 'Values'
cf1 = 'Competence'
cf2 = 'Order'
cf3 = 'Dutifulness'
cf4 = 'Achievement striving'
cf5 = 'Self-Discipline'
cf6 = 'Deliberation'
ef1 = 'Warmth'
ef2 = 'Gregariousness'
ef3 = 'Assertiveness'
ef4 = 'Activity'
ef5 = 'Excitement seeking'
ef6 = 'Positive emotions'
af1 = 'Trust'
af2 = 'Straightforwardness'
af3 = 'Altruism'
af4 = 'Compliance'
af5 = 'Modesty'
af6 = 'Tender-mindedness'
nf1 = 'Anxiety'
nf2 = 'Angry hostility'
nf3 = 'Depression'
nf4 = 'Self-consciousness'
nf5 = 'Impulsiveness'
nf6 = 'Vulnerability'
## Alpha
O   0.6
C   0.83
E   0.82
A   0.58
N   0.61
## CFA
Estimator ML
Comparative Fit Index (CFI)                    0.376       0.321
Tucker-Lewis Index (TLI)                       0.313       0.252
                                                                
Robust Comparative Fit Index (CFI)                         0.360
Robust Tucker-Lewis Index (TLI)                            0.295
RMSEA                                          0.221       0.235
Robust RMSEA                                               0.225
SRMR                                           0.221       0.221
## ANOVA
o_binary     1   4.48   4.481   2.016  0.162
Residuals   48 106.68   2.222               
            Df Sum Sq Mean Sq F value Pr(>F)
c_binary     1   2.80   2.802   2.002  0.164
Residuals   48  67.17   1.399               
            Df Sum Sq Mean Sq F value Pr(>F)  
e_binary     1   4.79   4.785   2.959 0.0919 .
Residuals   48  77.64   1.617                 
            Df Sum Sq Mean Sq F value Pr(>F)  
a_binary     1   5.16   5.157   3.638 0.0625 .
Residuals   48  68.05   1.418                 
            Df Sum Sq Mean Sq F value  Pr(>F)   
n_binary     1  16.44  16.442   9.536 0.00335 **
Residuals   48  82.76   1.724                   
## Optische Beurteilung
of1 = 'Fantasy'             links-schief
of2 = 'Aesthetics'          links-schief
of3 = 'Feelings'            rechts-schief
of4 = 'Actions'
of5 = 'Ideas'
of6 = 'Values'
cf1 = 'Competence'
cf2 = 'Order'               links-schief
cf3 = 'Dutifulness'
cf4 = 'Achievement striving'
cf5 = 'Self-Discipline'     links-schief
cf6 = 'Deliberation'        links-schief, 2 + 3 sehr häufig
ef1 = 'Warmth'              rechts-schief
ef2 = 'Gregariousness'
ef3 = 'Assertiveness'
ef4 = 'Activity'
ef5 = 'Excitement seeking'  doppel hoch
ef6 = 'Positive emotions' 
af1 = 'Trust'
af2 = 'Straightforwardness' rechts-schief, 7 extrem häufig!
af3 = 'Altruism'            links-schief
af4 = 'Compliance'
af5 = 'Modesty'             doppel hoch
af6 = 'Tender-mindedness'   rechts-schief
nf1 = 'Anxiety'             rechts-schief
nf2 = 'Angry hostility'
nf3 = 'Depression'
nf4 = 'Self-consciousness'
nf5 = 'Impulsiveness'       rechts-schief
nf6 = 'Vulnerability'
## Aktion: Entfernen von schlechten Variablen (nach Alpha-Analyse)
of4 = 'Actions'             (negativ gepolt)
ef3 = 'Assertiveness'       ( -> 0.85)
af2 = 'Straightforwardness' rechts-schief, 7 extrem häufig! (negativ gepolt)
nf5 = 'Impulsiveness'       rechts-schief   (negativ gepolt)
## Alpha optimiert
O   0.7
C   0.83
E   0.85
A   0.7
N   0.75
