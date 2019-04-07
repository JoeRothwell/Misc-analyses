#a simple loop to generate the mean of an increasing sample size from 
#a normal distribution

#make an empty vector
#i is the iterative variable that 1 is added to on each cycle of the loop
output <- numeric(200)
for (i in 1:200) { output[i] <- mean(rnorm(i, 0, 1)) }
#plot the data on a scatter graph
plot(output, pch=16, cex=0.6, xlab="Sample size", ylab="mean")
abline(0, 0, lwd=0.5)

#another loop that adds elements of a vector cumulatively
output <- numeric(10)
X <- c(1:10)
for (i in 1:10) { output[i] <- sum(X[1:i]) }
output

#loop to demonstrate central limit theorem
#create empty vector for results
output <- numeric(1000)
#draw 1000 samples of 10 numbers from a uniform distribution and sum them
for (i in 1:1000) { output[i] <- sum(runif(10)) }
#normally distributed graph output
hist(output)

#a loop that generates many QQ plots 
obs <- read.csv("urinary PPs in EPIC.csv")
logobs <- log(obs)

qqnorm(obs$uPPc_1, main="My QQ plot")
qqline(obs$uPPc_1)

#loop to get a QQ for the distribution of each polyphenol
pdf("qqplots.pdf")
for (i in 1:ncol(obs)) { 
  qqnorm(obs[, i], main=colnames(obs)[i])
  qqline(obs[, i])
}
dev.off()

#instead of a loop, reshape and plot with lattice
library(reshape2)
library(lattice)
meltedobs <- melt(obs)
qqmath(~log(value)|variable, data=meltedobs, strip.background=list(col="grey"))

source("http://bioconductor.org/biocLite.R")
biocLite("devtools")    # only if devtools not yet installed
biocLite("jtleek/genstats",ref="gh-pages")

#------------------------------------------------------------------------------------------------
#subset a data frame and loop over different subsets to perform hierarchical clustering

df2 <- read.table(text="
Name, Group, X, Y
Bob,  2, -300, 20
Karl, 4,  200, 10
Sara, 2, -298, 19
Hans, 4,  210, 12
Anna, 3, -700, 100
Maria,3, -690, 110
Lars, 2,  100, 50
Person1, 3,  -680, 120
Person2, 4,  220, 14  ",
                  header=T, sep=",", row.names = 1)

#preallocate a list of the correct length to store each hclust object
grpcol <- df2$Group
ngroup <- unique(grpcol)
output <- vector("list", length(unique(grpcol)))

#run loop, subsetting each group with %in%
for (grp in ngroup) {
  mat <- df2[df2$Group %in% grp, -2]
  output[[grp]] <- hclust(dist(mat))
}

#get output
output[2]

#writing many plots to separate files. eg boxplot
grps <- factor(c(rep("group1", 5), rep("group2", 5), rep("group3", 5)))
data <- matrix(rnorm(150), nrow=15)

for (i in 1:ncol(data)) {
  png(file = paste("var_", i, ".png", sep=""))
  boxplot(data[, i] ~ grps)
  dev.off()
}



