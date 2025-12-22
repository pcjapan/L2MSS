set.seed(123)
my_data <- read.csv("data.csv", header = TRUE, sep = ",")
library(psych)
N <- nrow(my_data)
indices <- 1:N
indices_EFA <- sample(indices, floor(0.5 * N))
indices_CFA <- indices[!(indices %in% indices_EFA)]
data_EFA <- my_data[indices_EFA, ]
data_CFA <- my_data[indices_CFA, ]
group5 <- c(24:33,45:66)
efa_results <- fa(data_EFA[, group5], nfactors = 2, rotate = "geominT")
efa_results <- fa.sort(efa_results)
print(efa_results)
biplot.psych(efa_results)
fa.plot(efa_results)
fa.diagram(efa_results)
fa.parallel(data_EFA[, group5])

install.packages("lavaan") ## For CFA
install.packages("semPlot") ## Used for creating factor diagrams
# load CFA required libraries
library(lavaan)
library(semPlot)

# Define CFA models
Group1 <- ' 
One_factor =~ Intend1 + Intend2 + Intend3 + Intend4 + Intend5 + Intend6 + Intend7
+ Intend8 + Intend9 + Intend10 + ATLE1 + ATLE2 + ATLE3 + ATLE4 + ATLE5
'
# Fit model to data
fit <- cfa(Group1, data_CFA, std.lv = TRUE)
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
semCors(fit)

Group2 <- ' Group2_1 =~ Comm1 + Comm2 + Comm3 + Comm4 + Culture1 + Culture2 + Culture3 + Culture4 '
fit2 <- cfa(Group2, data_CFA, std.lv = TRUE)

Group2a <- ' Comm    =~ Comm1 + Comm2 + Comm3 + Comm4
             Culture =~ Culture1 + Culture2 + Culture3 + Culture4
             Comm ~~ Culture '
fit2a <- cfa(Group2a, data_CFA, std.lv = TRUE)
summary(fit2a, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
anova(fit2, fit2a)

oughtTo <- ' oneFactor =~ Ought1 + Ought2 + Ought3 + Ought4 + Ought5 + Ought6 + Ought7 + Ought8 + Ought9 + Ought10 
                          + Prom1 + Prom2 + Prom3 + Prom4 + Prom5 + Prom6 + Prom7 + Prom8 + Prom9 + Prom10 + Prom11 + Prom12 + Prom13 + Prom14
                          + Prev1 + Prev2 + Prev3 + Prev4 + Prev5 + Prev6 + Prev7 + Prev8 '
oughtTo1 <- cfa(oughtTo, data_CFA, std.lv = TRUE)
summary(oughtTo1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

oughtTob <- ' Factor1 =~ Ought1 + Ought2 + Ought3 + Ought4 + Ought5 + Ought6 + Ought7 + Ought8 + Ought9 + Ought10 
             Factor2 =~ Prom1 + Prom2 + Prom3 + Prom4 + Prom5 + Prom6 + Prom7 + Prom8 + Prom9 + Prom10 + Prom11 + Prom12 + Prom13 + Prom14
             Factor3 =~ Prev1 + Prev2 + Prev3 + Prev4 + Prev5 + Prev6 + Prev7 + Prev8 '
oughtTo2 <- cfa(oughtTob, data_CFA, std.lv = TRUE)
summary(oughtTo2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
