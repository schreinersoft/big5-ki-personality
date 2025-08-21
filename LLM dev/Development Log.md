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
ef6 - af1 > 0.6    
## Alpha
O   0.77
C   0.82
E   0.82
A   0.79
N   0.83
## Korrelationsmatrix
ef6 = 'Positive emotions'  - af1 = 'Trust'  0.7 !
## CFA
Estimator ML
Comparative Fit Index (CFI)                    0.544
Tucker-Lewis Index (TLI)                       0.471
                                                                
Robust Comparative Fit Index (CFI)             0.479
Robust Tucker-Lewis Index (TLI)                0.406

RMSEA                                          0.213

SRMR                                           0.185
## Kreuzkorrelationen
> inspect(fit, "cor.lv")
        Ofactr Cfactr Efactr Afactr Nfactr
Ofactor  1.000                            
Cfactor  0.017  1.000                     
Efactor  0.026  0.176  1.000              
Afactor  0.231  0.244  0.419  1.000       
Nfactor  0.170  0.000 -0.231  0.521  1.000    -> N und A recht stark
## PCA
Call: principal(r = facets, nfactors = 5, rotate = "varimax")
Standardized loadings (pattern matrix) based upon correlation matrix
      RC4   RC2   RC1   RC3   RC5   h2    u2 com
of1 -0.25  0.21  0.11  0.74  0.18 0.70 0.295 1.6
of2 -0.16 -0.09 -0.19  0.87  0.07 0.84 0.161 1.2
of5  0.20 -0.06  0.01  0.88 -0.21 0.87 0.129 1.2
of6  0.41  0.26  0.31  0.70  0.04 0.82 0.181 2.4
cf1  0.79 -0.30  0.15  0.26  0.23 0.85 0.150 1.8
cf2  0.77 -0.11  0.02 -0.10  0.01 0.61 0.390 1.1
cf3  0.67  0.32  0.12 -0.49  0.00 0.80 0.195 2.4
cf4  0.64  0.37  0.18 -0.03  0.25 0.64 0.363 2.2
cf5  0.88 -0.13  0.17 -0.15 -0.05 0.84 0.163 1.2
cf6  0.79  0.21  0.09  0.29 -0.12 0.77 0.228 1.5
ef2  0.03 -0.08  0.46 -0.09  0.73 0.77 0.233 1.7
ef4  0.19 -0.14  0.09 -0.05  0.85 0.79 0.207 1.2
ef5 -0.14 -0.06  0.01  0.09  0.88 0.81 0.187 1.1
ef6  0.13 -0.50  0.50  0.22  0.59 0.91 0.086 3.3   -> sehr hohe kommunalität: raus!
af1  0.15 -0.41  0.77  0.02  0.28 0.86 0.142 1.9
af3  0.21  0.40  0.71  0.07  0.19 0.75 0.255 2.0
af4  0.21  0.19  0.85 -0.21 -0.01 0.85 0.154 1.4
af6  0.02  0.46  0.73  0.24  0.22 0.85 0.152 2.1
nf1  0.10  0.94 -0.03 -0.07 -0.01 0.89 0.107 1.0
nf3 -0.21  0.74  0.01 -0.07 -0.37 0.74 0.263 1.7
nf4  0.11  0.65  0.38  0.23 -0.16 0.65 0.345 2.1
nf6  0.00  0.88  0.21  0.13 -0.08 0.84 0.156 1.2
# Aktion: ef6 raus
## Alpha
O   0.77
C   0.82
E   0.77  -> schlechter
A   0.79
N   0.83
## Korrelationsmatrix
af1 = 'Trust' - ef2 'Gregariousness' .51 !
## CFA
Estimator ML
Comparative Fit Index (CFI)                    0.568
Tucker-Lewis Index (TLI)                       0.494
                                                                
Robust Comparative Fit Index (CFI)             0.479
Robust Tucker-Lewis Index (TLI)                0.406

RMSEA                                          0.206

SRMR                                           0.182
## Kreuzkorrelationen
> inspect(fit, "cor.lv")
        Ofactr Cfactr Efactr Afactr Nfactr
Ofactor  1.000                            
Cfactor  0.035  1.000                     
Efactor -0.116  0.134  1.000              
Afactor  0.222  0.244  0.461  1.000       
Nfactor  0.175 -0.001 -0.076  0.525  1.000    N und A immer noch stark
## PCA
Call: principal(r = facets, nfactors = 5, rotate = "varimax")
Standardized loadings (pattern matrix) based upon correlation matrix
      RC5   RC2   RC3   RC1   RC4   h2   u2 com
of1 -0.25  0.22  0.75  0.10  0.17 0.70 0.30 1.6
of2 -0.16 -0.09  0.87 -0.19  0.07 0.84 0.16 1.2
of5  0.21 -0.05  0.88  0.00 -0.23 0.87 0.13 1.3
of6  0.41  0.25  0.70  0.33  0.03 0.83 0.17 2.4
cf1  0.79 -0.30  0.26  0.15  0.21 0.85 0.15 1.8
cf2  0.77 -0.10 -0.10  0.02  0.00 0.61 0.39 1.1
cf3  0.67  0.31 -0.49  0.13  0.01 0.80 0.20 2.4
cf4  0.64  0.37 -0.03  0.19  0.25 0.64 0.36 2.2
cf5  0.87 -0.13 -0.15  0.17 -0.05 0.84 0.16 1.2
cf6  0.79  0.22  0.28  0.08 -0.13 0.77 0.23 1.5
ef2  0.03 -0.10 -0.08  0.48  0.73 0.78 0.22 1.8
ef4  0.20 -0.16 -0.04  0.11  0.85 0.80 0.20 1.2
ef5 -0.14 -0.08  0.10  0.02  0.88 0.81 0.19 1.1
af1  0.15 -0.41  0.03  0.76  0.25 0.84 0.16 1.9
af3  0.21  0.38  0.08  0.73  0.18 0.75 0.25 1.9
af4  0.21  0.18 -0.21  0.85 -0.02 0.84 0.16 1.4
af6  0.01  0.44  0.24  0.75  0.21 0.86 0.14 2.1
nf1  0.09  0.95 -0.08 -0.03  0.00 0.91 0.09 1.0
nf3 -0.22  0.73 -0.08  0.02 -0.35 0.72 0.28 1.7
nf4  0.11  0.66  0.22  0.36 -0.17 0.66 0.34 2.0
nf6  0.00  0.89  0.13  0.20 -0.07 0.85 0.15 1.2

                       RC5  RC2  RC3  RC1  RC4
SS loadings           3.98 3.77 3.21 3.08 2.53
Proportion Var        0.19 0.18 0.15 0.15 0.12
Cumulative Var        0.19 0.37 0.52 0.67 0.79
Proportion Explained  0.24 0.23 0.19 0.19 0.15
Cumulative Proportion 0.24 0.47 0.66 0.85 1.00
## Aktion: ef6 wieder rein
alpha dadurch besser

## Kommentar
insgesamt spannend, dass das LLM offensichtlich die Bedeutung der Worte versteht






