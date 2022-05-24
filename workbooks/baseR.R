# download datafile and load libraries
source("scripts/setup.R")

data <- read.csv(file = "data/elife-data.csv", header=T, stringsAsFactors=FALSE)

head(data)

# index first row
data[1,]

# index third col
data[,3]

# get value at first row, third column
data[1,3]

# index column by name
data$SAMEA2737770

# index a column by name using a concatenated vector (preferred)
data[,c('SAMEA2737770')]

# some basic data manipulation
data[,c('X','...1')]

# drop X column
data <- subset(data, select = -c(X))

# rename ...1 col
colnames(data)[colnames(data) == '...1'] <- 'species'

# transpose data 

# 1. set rownames to species column values
row.names(data) <- data$species

# 2. use subset to remove the species column
data <- subset(data, select = -c(species))

# 3. transpose the dataframe
data <- as.data.frame(t(data))

# add rownames as samples column
data$samples <- row.names(data)

# reset rownames to numbers
row.names(data) <- NULL

# indexing out conditional data
data[data[,c("[Clostridium] scindens ATCC 35704")] > 0.001,][,c("samples","[Clostridium] scindens ATCC 35704")]

# example of plotting two columns
png(filename = "figures/figure1.png", res = 300,
    height = 3, width = 6, units = 'in')

plot(
  subset(data, select = c("Butyrivibrio proteoclasticus B316", 
                          "Dorea formicigenerans ATCC 27755"))
)

dev.off()

png(filename = "figures/figure1.png", res = 300,
    height = 3, width = 6, units = 'in')

plot(x = data$`Butyrivibrio proteoclasticus B316`, y = data$`Dorea formicigenerans ATCC 27755`)


dev.off()



two.col.subset <- subset(data, select = c("Butyrivibrio proteoclasticus B316", 
                                          "Dorea formicigenerans ATCC 27755"))
plot(two.col.subset)

# create subset dataframe variable
subset.data <- data[data[,c("[Clostridium] scindens ATCC 35704")] > 0.001,]

# sort column and select top 10 samples

head(
  data[order(data$`[Clostridium] scindens ATCC 35704`, 
             decreasing = TRUE),c("samples","[Clostridium] scindens ATCC 35704")]
  ,10)

# calculate column means

data.means <- as.data.frame(colMeans(subset(data, select = -c(samples))))

colnames(data.means) <- 'colMeans'

# plot bar plot minus x axis
png(filename = "figures/figure1.png", res = 300,
    height = 3, width = 6, units = 'in')

barplot(t(data.means), xaxt = 'no', ylab = "mean relative abundance")

dev.off()

# get top abundance species 
data.means$species <- rownames(data.means)

rownames(data.means) <- NULL

# get top 10 mean abundances
head(
data.means[order(-data.means$colMeans)
           ,c('species','colMeans')]
,10)

# select specific bacteria species into subset
spec.cols <- subset(data, select = c("samples","Clostridium sp. D5", 
                                     "Butyrivibrio proteoclasticus B316", 
                                     "Dorea formicigenerans ATCC 27755")
                    )


# calculate rowMeans and return samples with highest mean abundance

spec.means <- spec.cols

rownames(spec.means) <- spec.cols$samples

spec.means <- as.data.frame(rowSums(subset(spec.means, select = -c(samples))))

colnames(spec.means) <- 'rowSums'

spec.means$samples <- rownames(spec.means)

rownames(spec.means) <- NULL

head(
  spec.means[order(-spec.means$rowSums),c('samples','rowSums')],
  10
)



