# ============================================================
#   TEST DE LEVENE — aucun package requis
# ============================================================

set.seed(42)
groupe  <- factor(rep(c("A", "B", "C"), each = 30))
valeurs <- c(rnorm(30, 10, 2.0),
             rnorm(30, 12, 2.5),
             rnorm(30, 11, 1.8))
donnees <- data.frame(groupe, valeurs)

# --- TEST MANUEL (base R) ---
groupes    <- levels(groupe)
deviations <- numeric(length(valeurs))

for (g in groupes) {
  idx            <- which(groupe == g)
  med_g          <- median(valeurs[idx])
  deviations[idx] <- abs(valeurs[idx] - med_g)
}

resultat_lev <- oneway.test(deviations ~ groupe, var.equal = TRUE)
print(resultat_lev)

# --- INTERPRÉTATION ---
if (resultat_lev$p.value < 0.05) {
  cat("-> Variances HETEROGENES (p =", round(resultat_lev$p.value, 4), ")\n")
} else {
  cat("-> Variances HOMOGENES (p =", round(resultat_lev$p.value, 4), ")\n")
}

# --- GRAPHIQUE ---
boxplot(deviations ~ groupe,
        main   = "Test de Levene — Deviations absolues a la mediane",
        xlab   = "Groupe",
        ylab   = "|Xi - Mediane du groupe|",
        col    = c("salmon", "lightblue", "lightgreen"),
        border = "darkgray")

