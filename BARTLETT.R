set.seed(42)
groupe  <- factor(rep(c("A", "B", "C"), each = 30))
valeurs <- c(rnorm(30, 10, 2.0),
             rnorm(30, 12, 2.5),
             rnorm(30, 11, 1.8))
donnees <- data.frame(groupe, valeurs)

resultat <- bartlett.test(valeurs ~ groupe, data = donnees)
print(resultat)

if (resultat$p.value < 0.05) {
  cat("-> Variances HETEROGENES (p =", round(resultat$p.value, 4), ")\n")
} else {
  cat("-> Variances HOMOGENES (p =", round(resultat$p.value, 4), ")\n")
}

# Ouvre une fenêtre externe
x11(width = 8, height = 6)

boxplot(valeurs ~ groupe, data = donnees,
        main   = "Test de Bartlett — Variances par groupe",
        xlab   = "Groupe", ylab = "Valeurs",
        col    = c("lightblue", "lightgreen", "lightyellow"),
        border = "darkgray")