# Interpretation der Ergebnisse
Here's a comprehensive guide to interpreting CFA results from lavaan in R:

## 1. **Model Fit Indices**

```r
summary(fit, fit.measures = TRUE, standardized = TRUE)
```

### Key Fit Statistics to Check:

**Absolute Fit:**
- **Chi-square (χ²)**: Want non-significant (p > 0.05), but sensitive to sample size
- **RMSEA**: Root Mean Square Error of Approximation
  - < 0.05 = excellent fit
  - 0.05-0.08 = acceptable fit
  - \> 0.10 = poor fit
- **SRMR**: Standardized Root Mean Square Residual
  - < 0.05 = good fit
  - < 0.08 = acceptable fit

**Incremental Fit:**
- **CFI/TLI**: Comparative Fit Index / Tucker-Lewis Index
  - \> 0.95 = excellent fit
  - \> 0.90 = acceptable fit

## 2. **Factor Loadings**

### Unstandardized Loadings:
```r
parameterEstimates(fit)
```
- Raw coefficients relating indicators to factors
- Interpreted in original variable units

### Standardized Loadings:
```r
standardizedSolution(fit)
```
- **Range**: -1 to +1 (like correlations)
- **Interpretation**:
  - > 0.7 = strong loading
  - 0.5-0.7 = moderate loading
  - < 0.5 = weak loading (consider removing)

## 3. **Statistical Significance**

Look for:
- **Estimate**: The loading value
- **Std.Err**: Standard error
- **z-value**: Test statistic
- **P(>|z|)**: p-value (want < 0.05 for significance)

## 4. **Reliability and Validity**

### Calculate Reliability:
```r
library(semTools)
reliability(fit)  # Composite reliability
# Or manually calculate Cronbach's alpha
```

### Convergent Validity:
- All loadings should be significant and > 0.5
- Average Variance Extracted (AVE) > 0.5

### Discriminant Validity:
- Square root of AVE should exceed correlations between factors

## 5. **Factor Correlations**

```r
inspect(fit, "cor.lv")  # Factor correlations
```
- Check if factors are too highly correlated (> 0.85 suggests poor discriminant validity)

## 6. **Modification Indices**

```r
modificationIndices(fit, sort = TRUE)
```
- Suggests which parameters to add/modify
- **MI > 10**: Consider modification (but theory should guide decisions)

## 7. **Residuals**

```r
residuals(fit, type = "standardized")
```
- Large residuals (> |2.58|) indicate poor fit for specific item pairs

## Example Interpretation:

```r
# Good model indicators:
# ✓ RMSEA = 0.045 (< 0.05)
# ✓ CFI = 0.967 (> 0.95)  
# ✓ TLI = 0.951 (> 0.95)
# ✓ SRMR = 0.032 (< 0.05)
# ✓ All loadings > 0.6 and significant
# ✓ No modification indices > 10
```

## Red Flags:
- Non-convergent solutions
- Negative error variances (Heywood cases)
- Loadings > 1.0
- Very high factor correlations (> 0.90)

The key is balancing statistical fit with theoretical meaningfulness!

# Wahl des Estimators

Die Wahl des besten Estimators für eine CFA mit 5 Faktoren und 15 Messvariablen hängt von **Ihren Daten** ab. Hier sind die wichtigsten Optionen:

## **1. Maximum Likelihood (ML) - Standard**

```r
fit <- cfa(model, data = data, estimator = "ML")
```

**Verwenden wenn:**
- Kontinuierliche, normalverteilte Variablen
- Keine fehlenden Werte
- Stichprobe > 200

**Vorteile:** Standard-Estimator, gut interpretierbar, alle Fit-Indizes verfügbar

## **2. MLR (Robust ML) - Meist beste Wahl**

```r
fit <- cfa(model, data = data, estimator = "MLR")
```

**Verwenden wenn:**
- Kontinuierliche Variablen, aber **nicht normalverteilt**
- Moderate Stichprobengrößen (N > 150)
- Standard-Empfehlung für die meisten Fälle

**Vorteile:** Robust gegen Verletzung der Normalverteilung, korrigierte Standardfehler

## **3. WLSMV - Für ordinale/kategoriale Daten**

```r
fit <- cfa(model, data = data, 
           estimator = "WLSMV",
           ordered = c("var1", "var2", ...))  # Ordinale Variablen angeben
```

**Verwenden wenn:**
- Likert-Skalen (1-5, 1-7 etc.)
- Ordinale oder binäre Variablen
- Stichprobe > 250

## **4. MLM - Bei fehlenden Werten**

```r
fit <- cfa(model, data = data, 
           estimator = "ML",
           missing = "fiml")  # Full Information ML
```

**Verwenden wenn:**
- Fehlende Werte vorhanden
- Kontinuierliche, normalverteilte Daten

## **Empfehlung für Ihr Modell:**

Bei **5 Faktoren und 15 Variablen** (durchschnittlich 3 Indikatoren pro Faktor):

```r
# Erste Wahl - MLR für Robustheit
fit <- cfa(model, data = data, estimator = "MLR")

# Falls ordinale Daten (z.B. Likert-Skalen):
fit <- cfa(model, data = data, 
           estimator = "WLSMV",
           ordered = c("alle_ordinalen_variablen"))
```

## **Entscheidungsbaum:**

1. **Ordinale/Likert-Daten** → **WLSMV**
2. **Kontinuierliche Daten** → **MLR** (robust)
3. **Fehlende Werte** → **ML + FIML**
4. **Kleine Stichprobe** (N < 150) → **ULS** oder **DWLS**

## **Zusätzliche Überlegungen:**

```r
# Modell-Identifikation prüfen
lavInspect(fit, "identification")

# Verschiedene Estimatoren vergleichen
fit_ml <- cfa(model, data = data, estimator = "ML")
fit_mlr <- cfa(model, data = data, estimator = "MLR")

# Fit-Vergleich
anova(fit_ml, fit_mlr)
```

**Meine Empfehlung:** Beginnen Sie mit **MLR** - er ist robust und funktioniert in den meisten Situationen gut!