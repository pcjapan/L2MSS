set.seed(123)
my_data <- read.csv("/Users/pc/Downloads/data.csv", header = TRUE, sep = ",")
library(psych)
N <- nrow(my_data)
indices <- 1:N
indices_EFA <- sample(indices, floor(0.5 * N))
indices_CFA <- indices[!(indices %in% indices_EFA)]
data_EFA <- my_data[indices_EFA, ]
data_CFA <- my_data[indices_CFA, ]
group5 <- c(24:32,45:66)
efa_results <- fa(my_data[, group5], nfactors = 3, rotate = "geominT")
efa_results <- fa.sort(efa_results)
print(efa_results)
biplot.psych(efa_results)
fa.plot(efa_results)
fa.diagram(efa_results)
fa.parallel(data_EFA[, group5])
