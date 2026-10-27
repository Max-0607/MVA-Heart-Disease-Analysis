# Import dataset
data <- read.csv2("euroleague_25_26.csv")
# Remove row number
data$No <- NULL
# Inspect data
head(data)
str(data)
head(data)

# Split minutes
aux <- strsplit(data$Min, ":")

# Extract minutes and convert to numeric
data$`Min 2` <- as.numeric(sapply(aux, `[`, 1))

# Inspect result
head(data[, c("Min", "Min 2")])


# Assign player names as row labels
rownames(data) <- make.unique(as.character(data$PLAYER))

# Remove PLAYER and Min columns
data$PLAYER <- NULL
data$Min <- NULL

# Inspect result
head(data)

# Number of players before filtering
nrow(data)

# Keep players with at least 5 minutes and 5 games
data <- data[data$`Min 2` >= 5 & data$GP >= 5, ]

# Number of players after filtering
nrow(data)

# Inspect result
head(data)

#install.packages("FactoMineR")

library(FactoMineR)

# Convert categorical variables to factors
data$TEAM <- as.factor(data$TEAM)
data$POSITION <- as.factor(data$POSITION)

# Identify supplementary variables
quali_vars <- which(names(data) %in% c("TEAM", "POSITION"))
quanti_vars <- which(names(data) == "PIR")

# Apply PCA
pca <- PCA(
  data,
  scale.unit = TRUE,
  quali.sup = quali_vars,
  quanti.sup = quanti_vars,
  graph = FALSE
)

# Inspect PCA results
summary(pca)

# Display eigenvalues and explained variance
pca$eig

# Number of components with eigenvalue > 1
sum(pca$eig[, 1] > 1)

# Scree plot
barplot(
  pca$eig[, 1],
  names.arg = paste0("PC", 1:nrow(pca$eig)),
  xlab = "Principal Components",
  ylab = "Eigenvalue",
  main = "Scree Plot",
  col = "steelblue"
)

abline(h = 1, col = "red", lty = 2)

# 2c. PCA Variable Plots

# Variable coordinates and correlations
pca$var$coord
pca$var$cor

# Contributions of variables
pca$var$contrib

# PC1 vs. PC2
plot(pca, choix = "var", axes = c(1, 2))

# PC2 vs. PC3
plot(pca, choix = "var", axes = c(2, 3))

# 2e. PCA Individual Plots

# PC1 vs. PC2
plot(pca, choix = "ind", axes = c(1, 2))

# PC2 vs. PC3
plot(pca, choix = "ind", axes = c(2, 3))


# Select active numerical variables (exclude PIR)
numerical_data <- data[, sapply(data, is.numeric)]
numerical_data$PIR <- NULL

# Calculate Euclidean distances on scaled data
d_eu <- dist(scale(numerical_data))

# Apply metric MDS
mds_eu <- cmdscale(d_eu, eig = TRUE)

# Plot the first two coordinates
x <- mds_eu$points[, 1]
y <- mds_eu$points[, 2]

plot(x, y,
     xlab = "Coordinate 1",
     ylab = "Coordinate 2",
     main = "Metric MDS - Euclidean Distance",
     type = "n",
     xlim = range(x) + c(-1, 1),
     ylim = range(y) + c(-1, 1))

text(x, y, labels = rownames(data), cex = 0.5)


library(cluster)

# Select numerical variables and POSITION
gower_data <- data[, c(names(numerical_data), "POSITION")]

# Ensure POSITION is categorical
gower_data$POSITION <- as.factor(gower_data$POSITION)

# Calculate Gower distances
d_gower <- daisy(gower_data, metric = "gower")

# Apply metric MDS
mds_gower <- cmdscale(d_gower, eig = TRUE)

# Plot the first two coordinates
x <- mds_gower$points[, 1]
y <- mds_gower$points[, 2]

plot(x, y,
     xlab = "Coordinate 1",
     ylab = "Coordinate 2",
     main = "Metric MDS - Gower Distance",
     type = "n")

text(x, y, labels = rownames(data), cex = 0.5)

# Eigenvalues and goodness of fit
mds_gower$eig
mds_gower$GOF


# MDS coordinates
x <- mds_gower$points[, 1]
y <- mds_gower$points[, 2]

# Plot by POSITION
plot(x, y, type = "n",
     xlab = "Coordinate 1", ylab = "Coordinate 2",
     main = "Gower MDS - Player Positions")

text(x, y, labels = data$POSITION,
     col = as.numeric(as.factor(data$POSITION)),
     cex = 0.7)

# Plot by TEAM
plot(x, y, type = "n",
     xlab = "Coordinate 1", ylab = "Coordinate 2",
     main = "Gower MDS - Teams")

text(x, y, labels = data$TEAM,
     col = as.numeric(as.factor(data$TEAM)),
     cex = 0.7)

# Plot by PTS
plot(x, y, type = "n",
     xlab = "Coordinate 1", ylab = "Coordinate 2",
     main = "Gower MDS - Points")

text(x, y, labels = round(data$PTS, 1),
     cex = 0.7)

# Plot by AST
plot(x, y, type = "n",
     xlab = "Coordinate 1", ylab = "Coordinate 2",
     main = "Gower MDS - Assists")

text(x, y, labels = round(data$AST, 1),
     cex = 0.7)


# Create MCA dataset
mca_data <- data

# Identify numerical variables
num_vars <- sapply(mca_data, is.numeric)

# Convert numerical variables to binary factors
mca_data[num_vars] <- lapply(mca_data[num_vars], function(x) {
  factor(
    ifelse(x > mean(x, na.rm = TRUE),
           "Over average", "Below average")
  )
})

# Ensure categorical variables are factors
mca_data$TEAM <- as.factor(mca_data$TEAM)
mca_data$POSITION <- as.factor(mca_data$POSITION)

# Inspect results
str(mca_data)
summary(mca_data)


library(FactoMineR)

# Apply MCA
mca <- MCA(mca_data, graph = FALSE)

# Inspect MCA results
summary(mca)
mca$eig

# Describe dimensions
dimdesc(mca)

# Scree plot
barplot(mca$eig[, 2],
        main = "MCA - Explained Inertia",
        xlab = "Dimensions",
        ylab = "Percentage of Inertia")

# Plot categories
plot(mca, choix = "var", axes = c(1, 2))

# MCA Category Map
plot(mca, invisible = "ind",
     title = "MCA - Categories")

# Plot individuals
plot(mca, choix = "ind", axes = c(1, 2),
     label = "none")
