# Teste de jmvcore::format com zto
library(jmvcore)

cat("zto on 0.5:", jmvcore::format(0.5, "zto"), "\n")
cat("zto on 1.5:", jmvcore::format(1.5, "zto"), "\n")
cat("zto on 9.9655:", jmvcore::format(9.9655, "zto"), "\n")
cat("zto on -2.34:", jmvcore::format(-2.34, "zto"), "\n")
