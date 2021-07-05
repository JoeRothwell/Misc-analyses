library(MendelianRandomization)
library(ivreg)

data.genotype=read.csv("data4practical1.csv") 
dim(data.genotype)
head(data.genotype)
# Risk factor x and primary outcome y. Secondary outcomes y2 y3

x=data.genotype$x 
y=data.genotype$y 
y2=data.genotype$y2 
y3=data.genotype$y3
g1=data.genotype$g1 
g2=data.genotype$g2 
g3=data.genotype$g3 
g4=data.genotype$g4 
g5=data.genotype$g5 
g6=data.genotype$g6
g = cbind(g1,g2,g3,g4,g5,g6) 
dim(g)

# Check correlation of genetic variants
round(cor(g), digits =2)

# Question A: Is the risk factor x correlated with the outcome y? Test the association using a linear model.
mod1 <- lm(y ~ x, data = data.genotype)
summary(mod1)
# Yes associated p < 2.2e-16

mod2 <- lm(y2 ~ x, data = data.genotype)
summary(mod2)
# Yes associated p < 2.2e-16

mod3 <- glm(y3 ~ x, data = data.genotype, family = "binomial")
summary(mod3)
# Yes associated p < 2.2e-16

#A Instrument selection and ratio estimate. lm of each instrument on exposure
summary(lm(x ~ g1, data = data.genotype))$f #417
summary(lm(x ~ g2, data = data.genotype))$f #886
summary(lm(x ~ g3, data = data.genotype))$f #2476
summary(lm(x ~ g4, data = data.genotype))$f #138
summary(lm(x ~ g5, data = data.genotype))$f #12
summary(lm(x ~ g6, data = data.genotype))$f #1.18

#B minor allele freq (instrument strength often correlaties with the MAP)
sum(g1[g1 == 1]) + 2*sum(g1[g1 == 2]) / (2*length(g1)) #0.24
sum(g2[g2 == 1])/(2*length(g1)) #0.199
sum(g3[g3 == 1])/(2*length(g1)) #0.21
sum(g4[g4 == 1])/(2*length(g1)) #0.01
sum(g5[g5 == 1])/(2*length(g1)) #0.15
sum(g6[g6 == 1])/(2*length(g1)) #0.19

#C All apart from g6
#D Ratio estimate
g1y <- lm(y ~ g1, data = data.genotype)$coef
g1x <- lm(x ~ g1, data = data.genotype)$coef
g1y/g1x # 0.301

g2y <- lm(y ~ g2, data = data.genotype)$coef
g2x <- lm(x ~ g2, data = data.genotype)$coef
g2y/g2x # 0.292

g3y <- lm(y ~ g3, data = data.genotype)$coef
g3x <- lm(x ~ g3, data = data.genotype)$coef
g3y/g3x # 0.286

g4y <- lm(y ~ g4, data = data.genotype)$coef
g4x <- lm(x ~ g4, data = data.genotype)$coef
g4y/g4x # 0.263

g5y <- lm(y ~ g5, data = data.genotype)$coef
g5x <- lm(x ~ g5, data = data.genotype)$coef
g5y/g5x # 0.306

g6y <- lm(y ~ g6, data = data.genotype)$coef
g6x <- lm(x ~ g6, data = data.genotype)$coef
g6y/g6x # -0.773

# First and second order precision
summary(lm(y ~ g1, data = data.genotype))
# Looks like g1 has best precision

se_ratio_1st = 1
bx = summary(lm(x-g1))$coef[2,1]
bxse = summary(lm(x-g1))$coef[2,2]
se_ratio_1st

# Two stage least squares
# First stage get genetically predicted x
pred <- lm(x ~ g1 + g2 + g3 + g4 + g5, data = data.genotype)
summary(pred)
df <- data.frame(x)
xhat <- predict(pred, df)

#easier way
fitted.x = pred$fitted.values

# Second stage regress on y
twostagemod <- lm(y ~ xhat)
summary(twostagemod)
# Interpretaton: model highly significant for xhat, st. error much less than individual IVs
# Also get combined F-statistic and we see that IV account for 30% variation

library(AER)
imod <- ivreg(y ~ x | g1 + g2 + g3 + g4 + g5, data = data.genotype)
summary(imod)
# Estimate for x is the same but standard error is slightly different
# Still highly significant

summary(imod, diagnostics = TRUE)
# Weak instruments: not weak
# Wu-Hausman: comparison with observational
# Sargan: is there overidentification? No

imod2 <- ivreg(y2 ~ x | g1 + g2 + g3 + g4 + g5, data = data.genotype)
summary(imod2)

#F what happens if condition on confounder
u = data.genotype$u
summary(lm(y2 ~ x + u))
# We have confounding from u

# Part 4 2LSL for binomial


# Practical 2

library(ieugwasr) 
library(data.table) 
library(MendelianRandomization)
#ldl.path = "/Users/verena/work/mr/applications/summary-data/GLGC/"
#ldl.data = fread(paste(ldl.path,"jointGwasMc_LDL.txt.gz", sep=""), stringsAsFactors=FALSE)

# Genetic associations with LDL
ldl.data <- fread("jointGwasMc_LDL.txt", stringsAsFactors = FALSE)
nrow(ldl.data) #2.4 million
ldl.data$`P-value` <- as.numeric(ldl.data$`P-value`)
hist(ldl.data$`P-value`)
sum(ldl.data$`P-value` < 0.00000005)
# 3078 are significant

# Make dataset of significant SNPs
ldl.iv <- ldl.data[ldl.data$`P-value` < 0.00000005, ]

# Genetic associations with CAD
cad.data <- fread("UKBB.GWAS1KG.EXOME.CAD.SOFT.META.PublicRelease.300517.txt", 
                  stringsAsFactors=FALSE)

# Rename SNPs
colnames(cad.data)[2]="rsid"

# Merge datasets
data.merge <- merge(ldl.iv, cad.data, by = "rsid")
# 2943 SNPs

# Rename columns
colnames(data.merge)[4:5] = c("ldl_effectallele","ldl_noneffectallele") 
colnames(data.merge)[c(6,7,9)] = c("ldl_beta","ldl_se","ldl_pval") 
colnames(data.merge)[10] = c("ldl_eaf")

colnames(data.merge)[c(14,15,16)] = c("cad_effectallele","cad_noneffectallele","cad_eaf_not_aligned") 
colnames(data.merge)[c(17,18,19)] = c("cad_beta_not_aligned","cad_se","cad_pval")

# Check alleles align (need to convert case)
alleles.equal <- data.merge$ldl_effectallele == tolower(data.merge$cad_effectallele)
sum(alleles.equal)
length(alleles.equal) - sum(alleles.equal)
# 2504 are aligned, 439 not

data.merge$cad_beta <- ifelse(alleles.equal == T)
data.merge$cad_eaf <- ifelse(alleles.equal == T)
