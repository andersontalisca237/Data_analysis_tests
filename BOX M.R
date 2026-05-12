set.seed(42)
groupe <- factor(rep(c("A", "B", "C"), each = 30))
var1   <- c(rnorm(30, 10, 2.0), rnorm(30, 12, 2.5), rnorm(30, 11, 1.8))
var2   <- c(rnorm(30,  5, 1.0), rnorm(30,  6, 1.2), rnorm(30,  5.5, 0.9))
donnees <- data.frame(groupe, var1, var2)

groupes <- levels(groupe)
n_total <- nrow(donnees)
k       <- length(groupes)
ns      <- tapply(var1, groupe, length)

S_list  <- lapply(groupes, function(g) {
  idx <- which(groupe == g)
  cov(donnees[idx, c("var1","var2")])
})
S_pool  <- Reduce("+", mapply(function(S, n) (n-1)*S,
                              S_list, ns, SIMPLIFY = FALSE)) / (n_total - k)
M       <- (n_total - k) * log(det(S_pool)) -
  sum(mapply(function(S, n) (n-1)*log(det(S)), S_list, ns))
c_fact  <- 1 - (2*2^2 + 3*2 - 1) / (6*(2+1)*(k-1)) *
  (sum(1/(ns-1)) - 1/(n_total-k))
chi2_stat <- M * c_fact
p_val     <- 1 - pchisq(chi2_stat, 2*(2+1)*(k-1)/2)

cat("Chi2 =", round(chi2_stat, 4), "| p =", round(p_val, 4), "\n")
if (p_val < 0.05) {
  cat("-> Matrices de covariance DIFFERENTES\n")
} else {
  cat("-> Homogeneite des covariances CONFIRMEE\n")
}

# Fenêtre séparée
windows(width = 8, height = 6, title = "Test de Box M")
plot(donnees$var1, donnees$var2,
     col  = c("blue","red","darkgreen")[as.integer(groupe)],
     pch  = 19, cex = 0.8,
     main = "Test de Box M — Nuage de points par groupe",
     xlab = "Variable 1", ylab = "Variable 2")
for (i in seq_along(groupes)) {
  idx   <- which(groupe == groupes[i])
  xg    <- donnees$var1[idx]; yg <- donnees$var2[idx]
  theta <- seq(0, 2*pi, length.out = 100)
  ex    <- mean(xg) + 2*sd(xg) * cos(theta)
  ey    <- mean(yg) + 2*sd(yg) * (cor(xg,yg)*cos(theta) +
                                    sqrt(1-cor(xg,yg)^2)*sin(theta))
  lines(ex, ey, col = c("blue","red","darkgreen")[i], lwd = 2)
}
legend("topright", legend = groupes,
       col = c("blue","red","darkgreen"), pch = 19)