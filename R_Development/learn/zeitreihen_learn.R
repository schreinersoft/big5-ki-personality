# Pakete laden
library(zoo)
library(xts)
library(lubridate)
library(ggplot2)
library(dplyr)
library(mgcv)
library(forecast)

# Beispieldaten mit unregelmäßigen Zeitpunkten
set.seed(123)
zeiten <- as.POSIXct("2020-01-01") + cumsum(rexp(100, rate = 1/86400)) # Zufällige Zeitabstände
werte <- 10 + 2*sin(as.numeric(zeiten - min(zeiten))/86400/30*2*pi) + rnorm(100, 0, 0.5)

# Als data.frame
daten <- data.frame(Zeit = zeiten, Wert = werte)

# Visualisierung
ggplot(daten, aes(x = Zeit, y = Wert)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(title = "Unregelmäßige Zeitreihe")

# Zeitabstände analysieren
time_diffs <- diff(daten$Zeit)
summary(as.numeric(time_diffs, units = "hours"))
hist(as.numeric(time_diffs, units = "hours"), 
     main = "Verteilung der Zeitabstände", 
     xlab = "Stunden")


# Interpolation
# Regelmäßiges Zeitraster erstellen
start_time <- min(daten$Zeit)
end_time <- max(daten$Zeit)
regular_times <- seq(start_time, end_time, by = "1 day")  # Täglich

# Zoo-Objekt für Interpolation
zoo_data <- zoo(daten$Wert, daten$Zeit)

# Verschiedene Interpolationsmethoden
# Lineare Interpolation
interpolated_linear <- na.approx(zoo_data, xout = regular_times, na.rm = FALSE)

# Spline-Interpolation
interpolated_spline <- na.spline(zoo_data, xout = regular_times, na.rm = FALSE)

# LOCF (Last Observation Carried Forward)
interpolated_locf <- na.locf(zoo_data, xout = regular_times, na.rm = FALSE)

# Vergleich der Methoden
plot_data <- data.frame(
  Zeit = rep(regular_times, 3),
  Wert = c(as.numeric(interpolated_linear), 
           as.numeric(interpolated_spline), 
           as.numeric(interpolated_locf)),
  Methode = rep(c("Linear", "Spline", "LOCF"), each = length(regular_times))
)

ggplot() +
  geom_point(data = daten, aes(x = Zeit, y = Wert), color = "red", size = 2) +
  geom_line(data = plot_data, aes(x = Zeit, y = Wert, color = Methode)) +
  theme_minimal() +
  labs(title = "Interpolationsmethoden im Vergleich")


# GAM Glättung
# Zeit als numerisch für GAM
daten$Zeit_num <- as.numeric(daten$Zeit)

# GAM-Modell
gam_model <- gam(Wert ~ s(Zeit_num, k = 20), data = daten)
summary(gam_model)

# Vorhersagen für Visualisierung
pred_times <- seq(min(daten$Zeit_num), max(daten$Zeit_num), length.out = 200)
pred_df <- data.frame(Zeit_num = pred_times)
pred_df$Wert_pred <- predict(gam_model, pred_df)
pred_df$Zeit <- as.POSIXct(pred_df$Zeit_num, origin = "1970-01-01")

# Plot
ggplot() +
  geom_point(data = daten, aes(x = Zeit, y = Wert), alpha = 0.6) +
  geom_line(data = pred_df, aes(x = Zeit, y = Wert_pred), color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "GAM-Glättung für unregelmäßige Zeitreihe")


# LOESS-Regression
loess_model <- loess(Wert ~ Zeit_num, data = daten, span = 0.3)

# Vorhersagen
pred_df$Wert_loess <- predict(loess_model, pred_df$Zeit_num)

# Plot
ggplot() +
  geom_point(data = daten, aes(x = Zeit, y = Wert), alpha = 0.6) +
  geom_line(data = pred_df, aes(x = Zeit, y = Wert_loess), color = "green", size = 1) +
  theme_minimal() +
  labs(title = "LOESS-Glättung")



# install.packages("irregts") # Falls nicht vorhanden
library(irregts)

# Irregts-Objekt erstellen
its_data <- irregts(daten$Wert, daten$Zeit)

# Plot
plot(its_data, main = "Unregelmäßige Zeitreihe mit irregts")

# Grundlegende Statistiken
summary(its_data)


#### REST SIEHE claude.ai
