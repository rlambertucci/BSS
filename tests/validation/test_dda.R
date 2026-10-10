# Script de Teste Diagnostico para Ciclo 3 (dda)
library(MASS)
data(iris)

fit_1cov <- lda(Species ~ Sepal.Length, data = iris)
pred_1cov <- predict(fit_1cov)
scores_1cov <- pred_1cov$x

cat("--- cor with 1 cov ---\n")
cormat <- stats::cor(iris[, "Sepal.Length", drop=FALSE], scores_1cov)
print(cormat)
print(class(cormat))
print(rownames(cormat))
