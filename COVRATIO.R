set.seed(42)
groupe <- factor(rep(c("A", "B", "C"), each = 30))
var1   <- c(rnorm(30, 10, 2.0), rnorm(30, 12, 2.5), rnorm(30, 11, 1.8))
var2   <- c(rnorm(30,  5, 1.0), rnorm(30,  6, 1.2), rnorm(30,  5.5, 0.9))
donnees <- data.frame(groupe, var1, var2)

modele   <- lm(var1 ~ var2 + groupe, data = donnees)
cov_vals <- covratio(modele)
n        <- nrow(donnees)
p        <- length(coef(modele))
seuil_h  <- 1 + (3 * p / n)
seuil_b  <- 1 - (3 * p / n)

influents <- which(cov_vals > seuil_h | cov_vals < seuil_b)
cat("Seuil haut :", round(seuil_h, 3), "\n")
cat("Seuil bas  :", round(seuil_b, 3), "\n")
cat("Influents  :", length(influents), "| Indices :", influents, "\n")

# Fenêtre séparée
windows(width = 9, height = 6, title = "COVRATIO")
couleurs <- ifelse(cov_vals > seuil_h | cov_vals < seuil_b,
                   "red", "steelblue")
plot(seq_along(cov_vals), cov_vals,
     col  = couleurs, pch = 19, cex = 0.9,
     main = "COVRATIO — Influence sur les coefficients",
     xlab = "Indice observation", ylab = "COVRATIO")
abline(h = seuil_h, lty = 2, col = "red",    lwd = 1.5)
abline(h = seuil_b, lty = 2, col = "red",    lwd = 1.5)
abline(h = 1,       lty = 1, col = "gray50", lwd = 1.0)
legend("topright",
       legend = c("Normal", "Influent"),
       col    = c("steelblue", "red"), pch = 19)