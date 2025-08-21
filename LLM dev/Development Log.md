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
O   0.66
C   0.82
E   0.82
A   0.63
N   0.57
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
            Df Sum Sq Mean Sq F value Pr(>F)  
o_binary     1  4.506   4.506   6.944 0.0113 *
Residuals   48 31.148   0.649                 
            Df Sum Sq Mean Sq F value Pr(>F)  
c_binary     1   4.09   4.091   4.186 0.0462 *
Residuals   48  46.90   0.977                 
            Df Sum Sq Mean Sq F value Pr(>F)
e_binary     1   0.77  0.7651    0.69   0.41
Residuals   48  53.21  1.1085               
            Df Sum Sq Mean Sq F value Pr(>F)
a_binary     1  0.795  0.7949   1.246   0.27
Residuals   48 30.628  0.6381               
            Df Sum Sq Mean Sq F value  Pr(>F)   
n_binary     1   8.53   8.534   10.11 0.00258 **
Residuals   48  40.50   0.844                   
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
af5 = 'Modesty'             doppel hoch 
nf2 = 'Angry hostility'     (-> 0.83)
nf5 = 'Impulsiveness'       rechts-schief   (negativ gepolt)
## Alpha optimiert
O   0.73
C   0.82
E   0.85
A   0.79
N   0.83
## CFA
Estimator ML
Comparative Fit Index (CFI)                    0.498       0.434
Tucker-Lewis Index (TLI)                       0.427       0.354
                                                                
Robust Comparative Fit Index (CFI)                         0.479
Robust Tucker-Lewis Index (TLI)                            0.406

RMSEA                                          0.221       0.239
Robust RMSEA                                               0.226
SRMR                                           0.199       0.199
## Kreuzkorrelationen
> inspect(fit, "cor.lv")
        Ofactr Cfactr Efactr Afactr Nfactr
Ofactor  1.000                            
Cfactor -0.001  1.000                     
Efactor  0.110  0.187  1.000              
Afactor  0.265  0.238  0.620  1.000        -> A - E stark!
Nfactor  0.186  0.000 -0.096  0.519  1.000
## ANOVA
            Df Sum Sq Mean Sq F value  Pr(>F)   
o_binary     1    7.6   7.601    8.09 0.00652 **
Residuals   48   45.1   0.940                   
            Df Sum Sq Mean Sq F value Pr(>F)  
c_binary     1   4.09   4.091   4.186 0.0462 *
Residuals   48  46.90   0.977                 
            Df Sum Sq Mean Sq F value Pr(>F)
e_binary     1   0.92  0.9206   0.665  0.419
Residuals   48  66.49  1.3851               
            Df Sum Sq Mean Sq F value Pr(>F)
a_binary     1   2.80   2.801   2.203  0.144
Residuals   48  61.01   1.271               
            Df Sum Sq Mean Sq F value  Pr(>F)   
n_binary     1  12.21  12.213   9.296 0.00373 **
Residuals   48  63.06   1.314                    
## Aktion: Entfernen von schlechten Variablen (nach Alpha-Analyse)
of3 = 'Feelings'            (geringe korr mit gesamt)
## Aktion: Kreuzkorrelation:
ef1 - af1, af3, af6 > 0.6  -> ef1 entfernen
ef6 - af1 > 0.6    (wird nochmal drin gelassen)


