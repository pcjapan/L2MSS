set.seed(123) # Set seed for reproducibility of analysis

# Read in data
my_data <- read.csv("data.csv", header = TRUE, sep = ",")

# Install required packages if not already installed
install.packages("psych")  # For EFA
install.packages("lavaan") # For CFA

# load EFA package
library(psych)

# Split dataset for EFA and CFA
N <- nrow(my_data)
indices <- 1:N
indices_EFA <- sample(indices, floor(0.5 * N))
indices_CFA <- indices[!(indices %in% indices_EFA)]
data_EFA <- my_data[indices_EFA, ]
data_CFA <- my_data[indices_CFA, ]

## EFA ##

# Set up groupings of variables for EFA
group5 <- c(24:33,45:66)
efa_results <- fa(data_EFA[, group5], nfactors = 2, rotate = "geominT")
efa_results <- fa.sort(efa_results)
print(efa_results)

biplot.psych(efa_results)
fa.plot(efa_results)
fa.diagram(efa_results)
fa.parallel(data_EFA[, group5])

# load CFA package
library(lavaan)

## Define CFA models ##

# Group 1: Intended Effort and Attitudes to Learning English
Group1.1 <- ' factor1 =~ Intend1 + Intend2 + Intend3 + Intend4 + Intend5 + Intend6 + Intend7 +
              Intend8 + Intend9 + Intend10 + ATLE1 + ATLE2 + ATLE3 + ATLE4 + ATLE5 '

Group1.2 <- ' factor1 =~ Intend1 + Intend2 + Intend3 + Intend4 + Intend5 + Intend6 + Intend7 +
              Intend8 + Intend9 + Intend10
              factor2 =~ ATLE1 + ATLE2 + ATLE3 + ATLE4 + ATLE5 '

# Group 2: Attitudes to L2 Community and Cultural Interest
Group2.1 <- ' factor1 =~ Comm1 + Comm2 + Comm3 + Comm4 + Culture1 + Culture2 + Culture3 + Culture4 '

Group2.2 <- ' factor1 =~ Comm1 + Comm2 + Comm3 + Comm4
              factor2 =~ Culture1 + Culture2 + Culture3 + Culture4 '

# Group 3: Auditory Style and Visual Style
Group3.1 <- ' factor1 =~ Audit1 + Audit2 + Audit3 + Audit4 + Audit5 +
              Visual1 + Visual2 + Visual3 + Visual4 + Visual5 '

Group3.2 <- ' factor1 =~ Audit1 + Audit2 + Audit3 + Audit4 + Audit5
              factor2 =~ Visual1 + Visual2 + Visual3 + Visual4 + Visual5 '

# Group 4: Feared L2 Self and Negative Changes of the Future L2 Self-image
Group4.1 <- ' factor1 =~ Feared1 + Feared2 + Feared3 + Feared4 +
              Neg1 + Neg2 '

Group4.2 <- ' factor1 =~ Feared1 + Feared2 + Feared3 + Feared4
              factor2 =~ Neg1 + Neg2 '

# Group 5: Ought-to L2 Self, Instrumentality–Promotion, and Instrumentality–Prevention
Group5.1 <- ' factor1 =~ Ought1 + Ought2 + Ought3 + Ought4 + Ought5 + Ought6 + Ought7 + Ought8 + Ought9 + Ought10 +
              Prom1 + Prom2 + Prom3 + Prom4 + Prom5 + Prom6 + Prom7 + Prom8 + Prom9 + Prom10 + Prom11 + 
              Prom12 + Prom13 + Prom14 + Prev1 + Prev2 + Prev3 + Prev4 + Prev5 + Prev6 + Prev7 + Prev8 '

Group5.2 <- ' factor1 =~ Ought1 + Ought2 + Ought3 + Ought4 + Ought5 + Ought6 + Ought7 + Ought8 + Ought9 + Ought10 
              factor2 =~ Prom1 + Prom2 + Prom3 + Prom4 + Prom5 + Prom6 + Prom7 + Prom8 + Prom9 + Prom10 + Prom11 + 
              Prom12 + Prom13 + Prom14
              factor3 =~ Prev1 + Prev2 + Prev3 + Prev4 + Prev5 + Prev6 + Prev7 + Prev8 '

# Group 6: Ideal L2 Self, Linguistic Self-Confidence, Imagery Capacity, and Positive Changes 
#          of the Future L2 Self-image
Group6.1 <- ' factor1 =~ Ideal1 + Ideal2 + Ideal3 + Ideal4 + Ideal5 + Ideal6 + Ideal7 + Ideal8 + Ideal9 + Ideal10 +
              Conf1 + Conf2 + Conf3 + Conf4 + iCapa1 + iCapa2 + iCapa3 + iCapa4 + iCapa5 + Pos1 + Pos2 + Pos3 '

Group6.2 <- ' factor1 =~ Ideal1 + Ideal2 + Ideal3 + Ideal4 + Ideal5 + Ideal6 + Ideal7 + Ideal8 + Ideal9 + Ideal10
              factor2 =~ Conf1 + Conf2 + Conf3 + Conf4
              factor3 =~ iCapa1 + iCapa2 + iCapa3 + iCapa4 + iCapa5
              factor4 =~ Pos1 + Pos2 + Pos3  '

##Fit models to data ##

# Group 1
fit1.1 <- cfa(Group1.1, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
fit1.2 <- cfa(Group1.2, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
# Compare models
av1 <- anova(fit1.1, fit1.2)

# Group 2
fit2.1 <- cfa(Group2.1, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
fit2.2 <- cfa(Group2.2, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
# Compare models
av2 <- anova(fit2.1, fit2.2)

# Group 3
fit3.1 <- cfa(Group3.1, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
fit3.2 <- cfa(Group3.2, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
# Compare models
av3 <- anova(fit3.1, fit3.2)

# Group 4
fit4.1 <- cfa(Group4.1, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
fit4.2 <- cfa(Group4.2, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
# Compare models
av4 <- anova(fit4.1, fit4.2)

# Group 5
fit5.1 <- cfa(Group5.1, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
fit5.2 <- cfa(Group5.2, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
# Compare models
av5 <- anova(fit5.1, fit5.2)

# Group 6
fit6.1 <- cfa(Group6.1, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
fit6.2 <- cfa(Group6.2, data_CFA, std.lv = TRUE, estimator = "WLSMV", ordered = TRUE)
# Compare models
av6 <- anova(fit6.1, fit6.2)

# Print results
summary(fit1.1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit1.2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
