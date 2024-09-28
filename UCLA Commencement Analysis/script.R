knitr::opts_chunk$set(echo = TRUE)

library(lavaan)
library(GPArotation)
library(readxl)
library(cowplot)
library(REdaS)
library(corrplot)
library(ggplot2)
library(dplyr)
library(psych)
library(car)
library(paran)
library(sem)
library(semPlot)
library(Hmisc)
library(igraph)
library(OpenMx)

# Data Loading
commencement <- read.csv("/Users/AhyoungJu/Desktop/Portfolio\ Data/commencement_data.csv")

head(commencement)

# Data Cleaning
commencement <- data.frame(lapply(commencement, function(x) {
  gsub("5 Strongly Prefer", "5", x)
}))
commencement <- data.frame(lapply(commencement, function(x) {
  gsub("0 Do Not Prefer at All", "0", x)
}))

commencement[3:19] <- data.frame(lapply(commencement[3:19], function(x) {
  as.integer(x)
}))

commencement <- commencement[complete.cases(commencement), ]

colnames(commencement)[3:19] <- c("Item1", "Item2", "Item3", "Item4", "Item5", "Item6", "Item7", "Item8", "Item9", "Item10", "Item11", "Item12", "Item13", "Item14", "Item15", "Item16", "Item17")

write.csv(commencement, "/Users/AhyoungJu/Desktop/cleaned_commencement_data.csv", row.names = FALSE)

### Create a new data frame with only the question columns for analysis
commencement <- read.csv("/Users/AhyoungJu/Desktop/Portfolio\ Data/cleaned_commencement_data.csv")

commencement_item <- commencement[3:19]

### EDA
## Histogram

table(commencement_item[1])
p1 <- ggplot(commencement_item, aes(x = Item1)) + 
  geom_histogram()

table(commencement_item[2])
p2 <- ggplot(commencement_item, aes(x = Item2)) + 
  geom_histogram()

table(commencement_item[3])
p3 <- ggplot(commencement_item, aes(x = Item3)) + 
  geom_histogram()

table(commencement_item[4])
p4 <- ggplot(commencement_item, aes(x = Item4)) + 
  geom_histogram()

table(commencement_item[5])
p5 <- ggplot(commencement_item, aes(x = Item5)) + 
  geom_histogram()

table(commencement_item[6])
p6 <- ggplot(commencement_item, aes(x = Item6)) + 
  geom_histogram()

table(commencement_item[7])
p7 <- ggplot(commencement_item, aes(x = Item7)) + 
  geom_histogram()

table(commencement_item[8])
p8 <- ggplot(commencement_item, aes(x = Item8)) + 
  geom_histogram()

table(commencement_item[9])
p9 <- ggplot(commencement_item, aes(x = Item9)) + 
  geom_histogram()

table(commencement_item[10])
p10 <- ggplot(commencement_item, aes(x = Item10)) + 
  geom_histogram()

table(commencement_item[11])
p11 <- ggplot(commencement_item, aes(x = Item11)) + 
  geom_histogram()

table(commencement_item[12])
p12 <- ggplot(commencement_item, aes(x = Item12)) + 
  geom_histogram()

table(commencement_item[13])
p13 <- ggplot(commencement_item, aes(x = Item13)) + 
  geom_histogram()

table(commencement_item[14])
p14 <- ggplot(commencement_item, aes(x = Item14)) + 
  geom_histogram()

table(commencement_item[15])
p15 <- ggplot(commencement_item, aes(x = Item15)) + 
  geom_histogram()

table(commencement_item[16])
p16 <- ggplot(commencement_item, aes(x = Item16)) + 
  geom_histogram()

table(commencement_item[17])
p17 <- ggplot(commencement_item, aes(x = Item17)) + 
  geom_histogram()

figure <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, ncol = 3)
figure

## Descriptive Statistics

commencement_col <- colnames(commencement_item)

descriptive_stats <- data.frame()

for (item in commencement_col) {
  mean_value <- mean(commencement_item[[item]])
  median_value <- median(commencement_item[[item]])
  mode_value <- as.numeric(names(which.max(table(commencement_item[[item]]))))
  sd_value <- sd(commencement_item[[item]])
  range_value <- range(commencement_item[[item]])
  
  descriptive_stats <- rbind(descriptive_stats, c(item, mean_value, median_value, mode_value, sd_value, range_value[1], range_value[2]))
}
colnames(descriptive_stats) <- c("Item", "Mean", "Median", "Mode", "SD", "Min", "Max")

descriptive_stats$Item <- factor(descriptive_stats$Item, levels = descriptive_stats$Item)
print(descriptive_stats)

ggplot(descriptive_stats, aes(Item, as.numeric(Mean), group = 1))+
  geom_line()+
  geom_point()+
  labs(y = "Mean Score", x = "Item")

## Correlation Matrix

cor_matrix <- cor(commencement_item)

corrplot(cor_matrix, method = 'square', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n',col = COL2('PiYG'))

det(cor_matrix)

new_commencement <- commencement_item %>% mutate(sum = rowSums(.)) 

corrplot(cor(new_commencement), method = 'square', addCoef.col = 'black', tl.pos = 'd', cl.pos = 'n', col = COL2('PiYG'))

## Reliability Analysis

# with actual data
psych::alpha(commencement_item)

# with reduced data
commencement_reduced <- commencement_item[c(-14, -16)]
psych::alpha(commencement_reduced)

### Suitability for Factor Analysis
# Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy (MSA)
KMO(r = cor_matrix)

# Bartlett’s Test of Sphericity
bart_spher(commencement_item, use = "everything")

### Identify the Dimension of the Survey
## Parallel Analysis

# with actual data
paran(commencement_item, cfa = T, graph = TRUE,
      color = TRUE, col = c("black", "red", "blue"))

# with reduced data
paran(commencement_reduced, cfa = T, graph = TRUE,
      color = TRUE, col = c("black", "red", "blue"))

## Principal Component Analysis (PCA)

# with actual data
pca_ori <- prcomp(commencement_item)
summary(pca_ori)

var_explained_ori <- pca_ori$sdev^2 / sum(pca_ori$sdev^2)

qplot(c(1:17), var_explained_ori) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# with reduced data
pca_red <- prcomp(commencement_reduced)
summary(pca_red)

var_explained_red <- pca_red$sdev^2 / sum(pca_red$sdev^2)

qplot(c(1:15), var_explained_red) +
  geom_line() +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

### Statistical Method
## Exploratory Factor Analysis

commencement_fa11 <- fa(commencement_item, nfactors = 11, rotate = "oblimin")
print(commencement_fa11)

fa.diagram(commencement_fa11)

## Confirmatory Factor Analysis

# with full model
commencement.model <- '
  Fac1 =~ Item1
  Fac2 =~ Item2 + Item3
  Fac3 =~ Item4 + Item5
  Fac4 =~ Item6 + Item7 + Item8 + Item9 + Item10 + Item11
  Fac5 =~ Item12
  Fac6 =~ Item13 + Item14
  Fac7 =~ Item15
  Fac8 =~ Item16 + Item17
'

commencement.fit <- lavaan::cfa(commencement.model, data = commencement_item)

summary(commencement.fit, fit.measures = TRUE)

# with reduced model
commencement_re.model <- '
  Fac1 =~ Item1
  Fac2 =~ Item2 + Item3
  Fac3 =~ Item4 + Item5
  Fac4 =~ Item6 + Item7 + Item8 + Item9 + Item10 + Item11
  Fac5 =~ Item12
  Fac6 =~ Item13
  Fac7 =~ Item15
  Fac8 =~ Item17
'

commencement_re.fit <- lavaan::cfa(commencement_re.model, data = commencement_reduced)

summary(commencement_re.fit, fit.measures = TRUE)

semPlot::semPaths(commencement_re.fit, "std")
