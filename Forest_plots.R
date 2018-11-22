# Forest plots 

# 1. Neil, CRC paper
# 2. Nathalie, manuscript and group meeting slides
# 3. Marc, section day slides
# 4. Isabel, manuscript on amino acids and cancer

# Email me for data files!!!

# package example ----

library(metafor)
library(tidyverse)
data(dat.bcg)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)
forest(dat$yi, #vi=NA, #sei=NA,
       dat$vi, slab=paste(dat$author, dat$year, sep=", "), transf=exp,
       alim=c(0,2), steps=5, xlim=c(-2.5,4), refline=1, cex=.9)

# Neil Table 2 -----------------------------------------------------------------------------------------------------

t2 <- read.csv("Forest plots/Forest table 2.csv")
rowvec <- rev(c(1:3, 6:7, 10:13, 16:17, 20)) + 1
pointvec <- 18
#can use efac=0 to suppress vertical lines on CIs

dev.off()
par(mfrow=c(1,6))

#Plot for row names
par(mar=c(5,6,0,1))
plot(1, type="n", axes = F, ann = F, ylim=c(1,25), xlim = c(1,1.25))
text(1.03, c(2:4,7:8,11:14,17:18,21), c("Current", "Former", "Never", "Yes", "No", "Active", "Moderately active",
                                     "Moderately inactive", "Inactive", "Yes", "No", "Per 15 g/day"), pos=4)
text(1, c(5,9,15,19,22), c("Smoking status", "Prevalent diabetes", "Physical activity index", "Ever NSAID use",
                               "Alcohol consumption"), cex = 1, pos=4)
abline(h = 23)

#Colorectal
par(mar=c(5,0,0,1))
forest(x = t2$HR, ci.ub = t2$high, ci.lb = t2$low, rows = rowvec, ylim=c(1,25), refline=1, 
       xlab="", psize= rep(1.5, nrow(t2)), xlim=c(0,3), cex=1, subset=1:12, slab=t2$N, pch = 18)
text(3, c(1, 10), c("P-trend < 0.0001", "P-trend = 0.01"), pos = 2)
text(1, 24, "Colorectal", cex = 1.2)
text(3, 24, "HR [95% CI]", cex = 1, pos = 2)

#Colon proximal
par(mar=c(5,4,0,1))
forest(x = t2$HR, ci.ub = t2$high, ci.lb = t2$low, rows = rowvec, ylim=c(1,25), refline=1,
       xlab="", psize= rep(1.5, nrow(t2)), xlim=c(0,3), slab= t2$N, cex=1, subset=13:24, pch = 18)
text(3,c(1, 10), c("P-trend = 0.004", "P-trend = 0.0004"), pos = 2)
text(1, 24, "Colon proximal", cex = 1.2)
text(3, 24, "HR [95% CI]", cex = 1, pos = 2)

#Colon distal
par(mar=c(5,4,0,1))
forest(x = t2$HR, ci.ub = t2$high, ci.lb = t2$low, rows = rowvec, ylim=c(1,25), refline=1, 
       xlab="", psize= rep(1.5, nrow(t2)), xlim=c(0,3), slab= t2$N, cex=1, subset=25:36, pch = 18)
text(3,c(1, 10), c("P-trend = 0.09", "P-trend = 0.06"), pos = 2)
text(1, 24, "Colon distal", cex = 1.2)
text(3, 24, "HR [95% CI]", cex = 1, pos = 2)

#Rectal
par(mar=c(5,4,0,0))
forest(x = t2$HR, ci.ub = t2$high, ci.lb = t2$low, rows = rowvec, ylim=c(1,25), refline=1, 
       xlab="", psize= rep(1.5, nrow(t2)), xlim=c(0,3),  slab= t2$N, cex=1, subset=37:48, pch = 18)
text(3, c(1, 10), c("P-trend < 0.0001", "P-trend = 0.43"), pos = 2)
text(1, 24, "Rectal", cex = 1.2)
text(3, 24, "HR [95% CI]", cex = 1, pos = 2)

#Hetergeneity
par(mar=c(5,1,0,6))
plot(1, type="n", axes = F, xlab="", ylim=c(1,25), xlim = c(1,1.2))
text(1, c(5,9,15,19,22,24), c("0.05","0.73","0.03","0.62", "0.15", "Prox-dist-\nrect"), cex = 1, pos=4)
text(1.2, c(5,9,15,19,22,24), c("0.04","0.83","0.15","0.34", "0.12", "Prox-dist"), cex = 1, pos=2)
text(1.1, 25, expression(paste(italic(P),"-Heterogeneity" )))
abline(h = 23)

# Neil Table 3 ----------------------------------------------------------------------------------------------

t3 <- read.csv("Forest table 3new.csv")
#margins bottom, left, top, right
rowvec <- rev(c(1,2,5,6,9,10,13,14))

dev.off()
par(mfrow=c(1,6))

#Plot for row names
par(mar=c(5,4,0,1))
plot(1, type="n", axes = F, ann = F, ylim=c(1, 19), xlim = c(1, 1.25))
text(1, c(4,8,12,16), c("Waist-to-hip ratio", "Waist circumference", "Height", "Body mass index"), pos=4)
text(1.1, c(1,2,5,6,9,10,13,14), rep(c("Women", "Men"), 4), cex = 1, pos=4)
text(1.05, c(3,7,11,15), c("Per 0.05", "Per 5 cm", "Per 10 cm", expression("Per 5 kg/m"^2)), cex = 1, pos=4)
abline(h = 17)

#Colorectal
par(mar=c(5,0,0,1))
forest(slab = t3$n, x = t3$HR, ci.ub = t3$high, ci.lb = t3$low, # Read data frame
       rows = rowvec, xlim = c(0.8, 1.65), ylim = c(1, 19),         # Plot row spacing and x limits
       refline = 1, xlab = "", psize = rep(1.5, 32), cex=1, pch = 18,    # Point and text attributes
       subset = 1:8)
text(1, 17.5, "Colorectal", cex = 1.2)
text(1.65, 17.5, "HR [95% CI]", cex = 1.2, pos = 2)

#Colon proximal
par(mar=c(5,4,0,1))
forest(x=t3$HR, ci.lb=t3$low, ci.ub=t3$high, slab = t3$n, refline = 1, xlab="", psize=rep(1.5,32), 
       subset=9:16, rows = rowvec, ylim=c(1,19), xlim=c(0.7, 2.1), cex=1, pch=18)
text(1, 17.5, "Colon proximal", cex = 1.2)
text(2.1, 17.5, "HR [95% CI]", cex = 1, pos = 2)

#Colon distal
par(mar=c(5,4,0,1))
forest(x=t3$HR, ci.lb=t3$low, ci.ub=t3$high, rows = rowvec, ylim=c(1,19), slab = t3$n, subset=17:24,
       xlab="", refline = 1, psize=rep(1.5,32), xlim=c(0.8, 2), cex=1, pch=18)
text(1, 17.5, "Colon distal", cex = 1.2)
text(2, 17.5, "HR [95% CI]", cex = 1, pos = 2)

#Rectal
par(mar=c(5,4,0,0))
forest(x=t3$HR, ci.lb=t3$low, rows = rowvec, ylim=c(1,19), ci.ub=t3$high, slab = t3$n, refline = 1, 
       subset=25:32, xlab="", psize=rep(1.5,32), xlim=c(0.6, 1.8), 
       cex=1, pch=18)
text(1, 17.5, "Rectal", cex = 1.2)
text(1.8, 17.5, "HR [95% CI]", cex = 1, pos = 2)

#Hetergeneity
par(mar=c(5,1,0,5))
plot(1, type="n", axes = F, xlab="", ylim=c(1,19), xlim = c(1,1.2))

text(1, c(1,2,5,6,9,10,13,14, 17.5), 
     c("0.89","0.85","0.71","0.03","< 0.0001", "0.0001","0.43","0.008", "Prox-dist-\nrect "), cex = 1, pos=4)

text(1.2, c(1,2,5,6,9,10,13,14, 17.5), 
     c("0.64","0.69","0.79","0.78", "0.05", "0.24","0.19","0.94", " Prox-dist"), cex = 1, pos=2)
text(1.1, 18.5, expression(paste(italic(P),"-Heterogeneity" )))
abline(h = 17)


# Neil Table 4 --------------------------------------------------------------------------------------------------------
t4 <- read.csv("Forest plots/Forest table 4.csv")
rowvec = rev(c(2:6, 9:10, 13:14, 17:20, 23:26))

dev.off()
par(mfrow=c(1,6))

#Plot for row names
par(mar=c(5,2,0,1))
plot(1, type="n", axes = F, ann = F, ylim=c(1, 31), xlim = c(1, 1.25))
text(1, c(7,11,15,21,27), c("Duration of MHT use (years)", "Ever MHT use", "Ever oral contraceptive use", 
                        "Age at menopause (years)", "Age at menarche (years)"), pos=4)
text(1.05, rowvec, t4$category[1:17], cex = 1, pos=4)
abline(h = 29)

#Colorectal
par(mar=c(5,0,0,1))
forest(x = t4$HR, ci.ub = t4$high, ci.lb = t4$low, refline=1, rows = rowvec, ylim=c(1,31), 
       xlab="", psize= rep(1.5, nrow(t4)), xlim=c(0,2.5), slab= t4$n, pch = 18, cex=1, subset=1:17)
text(2.5, c(1,16,22), c("P-trend = 0.01", "P-trend = 0.18" , "P-trend = 0.4"), pos = 2)
text(1, 30, "Colorectal", cex = 1.2)
text(2.5, 30, "HR [95% CI]", cex = 1, pos = 2)

#Colon proximal
par(mar=c(5,4,0,1))
forest(x = t4$HR, ci.ub = t4$high, ci.lb = t4$low, refline=1, xlab="", psize= rep(1.5, nrow(t4)), 
       rows = rowvec, ylim=c(1,31), xlim=c(0, 3), cex=1, subset=18:34, slab= t4$n, pch = 18)
text(3 ,c(1,16,22), c("P-trend = 0.30", "P-trend = 0.45" , "P-trend = 0.09"), pos = 2)
text(1, 30, "Colon proximal", cex = 1.2)
text(3, 30, "HR [95% CI]", cex = 1, pos = 2)

#Colon distal
par(mar=c(5,4,0,1))
forest(x = t4$HR, ci.ub = t4$high, ci.lb = t4$low, refline=1, xlab="", psize = rep(1.5, nrow(t4)), 
       xlim=c(0, 3), rows = rowvec, ylim=c(1,31), cex=1, subset=35:51, slab= t4$n, pch = 18)
text(3, c(1,16,22), c("P-trend = 0.21", "P-trend = 0.83" , "P-trend = 0.51"), pos = 2)
text(1, 30, "Colon distal", cex = 1.2)
text(3, 30, "HR [95% CI]", cex = 1, pos = 2)

#Rectal
par(mar=c(5,4,0,0))
forest(x = t4$HR, ci.ub = t4$high, ci.lb = t4$low, refline=1, xlab="", psize= rep(1.5, nrow(t4)), 
       xlim=c(0, 3.5), cex=1, subset=52:68, rows = rowvec, ylim=c(1,31),slab= t4$n, pch = 18)
text(3.5 , c(1,16,22), c("P-trend = 0.03", "P-trend = 0.08" , "P-trend = 0.65"), pos = 2)
text(1, 30, "Rectal", cex = 1.2)
text(3.5, 30, "HR [95% CI]", cex = 1, pos = 2)

#Hetergeneity
par(mar=c(5,1,0,5))
plot(1, type="n", axes = F, xlab="", ylim=c(1,31), xlim = c(1,1.2))
text(1, c(7,11,15,21,27,30), c("0.22","0.37","0.93","0.54", "0.57", "Prox-dist-\nrect"), cex = 1, pos=4)
text(1.2, c(7,11,15,21,27,30), c("0.14","0.16","0.91","0.4", "0.76", "Prox-dist"), cex = 1, pos=2)
text(1.1, 31.5, expression(paste(italic(P),"-Heterogeneity" )))
abline(h = 29)

# Nathalie 1st draft --------------------------------------------------------------------------------------------

library(tidyverse)
vec <- 1:46 %in% c(1,3,5,7,9,11,13,15,17,19,20,21,22,23)
t1 <- read_csv("Forest plots/Forest data Nathalie.csv") %>% mutate(site.filt = vec, Cat2 = ifelse(site.filt == T, site, ""))

rowvec = rev(c(1,3,5,7,9,11:12,14:15,17:18,20:21,23:24,26:27,29:30,32:33,35:36))

dev.off()
par(mfrow=c(1,2))
par(mar=c(4,4,0,0))
forest(slab = t1$Cat2, ilab = t1$category, ilab.xpos = 0.55, ilab.pos = 4,
       x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, refline = 1, rows = rowvec,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, 40), xlim = c(0, 1.8), pch = 18, psize= rep(1.5, nrow(t1)), cex = 0.8, subset = 1:23)

text(c(0.07, 1, 1.6), 39, c("Site", "With BMI", "HR [95% CI]"), cex = 0.8)
#bottom, left, top, right

par(mar=c(4,0,0,7))
forest(x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, refline = 1, rows = rowvec,
       xlab="Mulitivariable-adjusted HR", psize = rep(1.5, nrow(t1)), 
       ylim = c(1, 40), xlim = c(0.1, 2.1), ilab = t1$category, ilab.xpos = 0.2, ilab.pos = 4,
       slab = NA, pch = 18, cex = 0.8, subset = 24:46)

text(c(1, 1.8), 39, c("Without BMI", "HR [95% CI]"), cex = 0.8)

# Nathalie group meeting ------------------------------------------------------------------------------------------------------

t1 <- read_csv("Forest data Nathalie2.csv")

#Male cancers
rowvec1 <- rev(seq(1,19,2))

dev.off()
#Put 2 graphs side by side
par(mfrow=c(1,2))

#Sets margins
par(mar=c(4,4,0,0))

forest(slab = t1$site, x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, refline = 1, rows = rowvec1, 
       xlab = "Mulitivariable-adjusted HR", ylim = c(1, max(rowvec1) + 3), xlim = c(0.2, 1.8), 
       pch = 23, bg="dodgerblue", psize= rep(2, 46), fill="blue", cex = 0.8, subset = 1:10)

#text(c(0.38, 1), max(rowvec) + 2, c("Site (without BMI)", "HR [95% CI]"), cex = 0.8)
text(c(1), max(rowvec1) + 2, c("HR [95% CI]"), cex = 0.8)

#bottom, left, top, right
par(mar=c(4,0,0,2))

forest(slab = t1$site, x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, refline = 1, rows = rowvec1,
       xlab = "Mulitivariable-adjusted HR", ylim = c(1, max(rowvec1) + 3), xlim = c(0.1, 1.8), 
       pch = 23, bg="dodgerblue", psize= rep(2, 46), cex = 0.8, subset = 11:20)

#text(c(0.2, 1), max(rowvec) + 2, c("Site (with BMI)", "HR [95% CI]"), cex = 0.8)
text(c(1), max(rowvec1) + 2, c("HR [95% CI]"), cex = 0.8)

#Female cancers
dev.off()
#Put 2 graphs side by side
par(mfrow=c(1,2))
rowvec2 <- t1[t1$model == "With BMI" & t1$category == "Women" , ]$spacing

#Sets margins
par(mar=c(4,4,0,0))

forest(slab = t1$site,
       x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, 
       refline = 1, rows = rowvec2,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, max(rowvec2) + 3), 
       xlim = c(0.2, 2), 
       pch = 23, bg="orange", psize= rep(2, 46), #fill="blue",
       cex = 0.8, subset = 21:33)

#text(c(0.38, 1), max(rowvec) + 2, c("Site (without BMI)", "HR [95% CI]"), cex = 0.8)
text(c(1), max(rowvec2) + 2, c("HR [95% CI]"), cex = 0.8)
#bottom, left, top, right

par(mar=c(4,0,0,2))

forest(slab = t1$site,
       x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, 
       refline = 1, rows = rowvec2,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, max(rowvec2) + 3), 
       xlim = c(-0.4, 2.4), 
       pch = 23, bg="orange", psize= rep(2, 46),
       cex = 0.8, subset = 34:46)

#text(c(0.2, 1), max(rowvec) + 2, c("Site (with BMI)", "HR [95% CI]"), cex = 0.8)
text(c(1), max(rowvec2) + 2, c("HR [95% CI]"), cex = 0.8)

# Nathalie paper --------------------------------------------------------------------------------------------

library(tidyverse)
library(metafor)
t1 <- read_csv("Forest data Nathalie3.csv")

#BMR/BMI comparison
#Make vectors for row spacing and colours
rowvec <- t1[t1$category == "Men" , ]$spacing
#pointvec <- c(rep(18, 18), rep(NA, 8), rep(18, 30))
pointvec <- c(rep(c(18, 1), 9), rep(NA, 8), rep(c(18, 1), 15))
colvec <- c(rep("black", 18), rep("white", 8), rep("black", 28), rep("white", 2))

dev.off()
par(mfrow=c(1,2))
#Sets margins (bottom, left, top, right)
par(mar=c(4,3.5,0,0))

forest(slab = t1$site,
       #ilab = t1$model, ilab.xpos = 0.5, ilab.pos = 4,
       x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, 
       refline = 1, rows = rowvec,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, max(rowvec) + 3), 
       xlim = c(-0.6, 3), 
       pch = pointvec,
       col = colvec,
       psize= rep(1.5, nrow(t1)), #fill = "blue",
       cex = 0.8, subset = 1:28)

#Use pull() to convert a tibble to a character vector, pos to left justify
text(rep(-0.6, 4), c(5, 8, 11, 14), pull(t1[c(25, 23, 21, 19), 1]), cex = 0.8, pos=4)
text(2.15, max(rowvec) + 2, "HR [95% CI]", cex = 0.8, pos = 4)
text(1, max(rowvec) + 2, "Men", cex = 0.8)

#bottom, left, top, right
par(mar=c(4,2,0,6))

forest(slab = NA,
       x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, 
       refline = 1, rows = rowvec,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, max(rowvec) + 3), 
       xlim = c(0, 3), 
       col = colvec,
       pch = pointvec,
       psize= rep(1.5, nrow(t1)),
       cex = 0.8, subset = 29:nrow(t1))

text(2.1, max(rowvec) + 2, c("HR [95% CI]"), cex = 0.8, pos = 4)
text(1, max(rowvec) + 2, "Women", cex = 0.8)

#BMI/BMI and height comparison

t2 <- read_csv("Forest data Nathalie4.csv")
dev.off()
#Put 2 graphs side by side
par(mfrow=c(1,2))
par(mar=c(4,3.5,0,0))

forest(slab = t2$site,
       x = t2$HR, ci.ub = t2$high, ci.lb = t2$low, 
       refline = 1, rows = rowvec,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, max(rowvec) + 3), 
       xlim = c(-3.5, 6), 
       pch = pointvec,
       col = colvec,
       psize= rep(1.5, nrow(t2)), #fill="blue",
       cex = 0.8, subset = 1:28)

text(1, max(rowvec) + 2, "Men", cex = 0.8)
text(3.75, max(rowvec) + 2, "HR [95% CI]", cex = 0.8, pos = 4)
text(rep(-3.5, 4), c(5, 8, 11, 14), pull(t1[c(25, 23, 21, 19), 1]), cex = 0.8, pos=4)

#bottom, left, top, right
par(mar=c(4,2,0,6))

forest(slab = NA,
       x = t2$HR, ci.ub = t2$high, ci.lb = t2$low, 
       refline = 1, rows = rowvec,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, max(rowvec) + 3), 
       xlim = c(-0.5, 4), 
       #alim = c(0.5, 1.8),
       pch = pointvec,
       #lty="blank",
       col = colvec,
       psize= rep(1.5, nrow(t2)),
       cex = 0.8, subset = 29:nrow(t2))

text(1, max(rowvec) + 2, "Women", cex = 0.8)
text(2.6, max(rowvec) + 2, c("HR [95% CI]"), cex = 0.8, pos = 4)

# BMR/BMI new 16 July 2018

#Use base R to read in to avoid inserting NAs
t1 <- read.csv("Forest data Nathalie5.csv")

#Generate sequence for row spacing
malerows <- rev((1:51)[-c(seq(3, 51, by=4), seq(4, 51, by=4))])
femalerows <- rev((1:64)[-c(seq(3, 64, by=4), seq(4, 64, by=4))])
#Alternating 18 and 1 point styles
pointvec <- c(rep(c(18, 1), nrow(t1)/2))

dev.off()
#Sets margins (bottom, left, top, right)
par(mar=c(4,4,1,2))

forest(slab = t1$site,
       ilab = t1[, 2], ilab.xpos = c(0.2), ilab.pos = 4,
       x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, 
       refline = 1, rows = malerows,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, max(malerows) + 3), 
       xlim = c(-1.2, 3.7), 
       alim = c(0.5, 2.5),
       pch = pointvec,
       #efac = 0,
       psize= rep(1.5, nrow(t1)),
       cex = 0.8, subset = 1:26)

#Add p-heterogeneity
text(3.7, seq(0,51, by=4), paste("P-heterogeneity =", 
       rev(na.omit(t1$phet[1:26]))), cex=0.8, pos=2)

#Add title text
text(3.7, max(malerows) + 2, "HR [95% CI]", cex = 0.8, pos = 2)
text(c(-1.2, 0.2), max(malerows) + 2, c("Cancer", "Cases"), cex = 0.8, pos=4)

dev.off()
#Sets margins (bottom, left, top, right)
par(mar=c(4,4,1,2))

#Females
forest(slab = t1$site,
       ilab = t1[, 2], ilab.xpos = c(0.2), ilab.pos = 4,
       x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, 
       refline = 1, rows = femalerows,
       xlab = "Mulitivariable-adjusted HR",
       ylim = c(1, max(femalerows) + 3), 
       xlim = c(-1.2, 3.7), 
       alim = c(0.5, 2.5), 
       pch = pointvec,
       psize= rep(1.5, nrow(t1)),
       cex = 0.8, subset = 27:58)

#Add p-heterogeneity
text(3.7, seq(0,63, by=4), paste("P-heterogeneity =", 
      rev(na.omit(t1$phet[27:58]))), cex=0.8, pos=2)

#Add title text
text(3.7, max(femalerows) + 2, "HR [95% CI]", cex = 0.8, pos = 2)
text(c(-1.2, 0.2), max(femalerows) + 2, c("Cancer", "Cases"), cex = 0.8, pos=4)


# Marc section day ----------------------------------------------------------------------------------------
library(tidyverse)
library(metafor)
#Table 1
t1 <- read.csv("Forest Marc1.csv")
rowvec <- t1$spacing

par(mar=c(4,4,1,2))
forest(slab = t1$param,
       x = t1$HR, ci.ub = t1$high, ci.lb = t1$low, 
       ilab = t1[, 2:4], ilab.xpos = c(-5.5, -3.5, -1.5), ilab.pos = 4,
       refline = 1, rows = rowvec,
       xlab = "Mulitivariable-adjusted HR",
       pch = 18, psize = 1.5,
       xlim = c(-8, 11),
       ylim = c(1, max(rowvec) + 3))

#Table 2
t2 <- read_csv("Forest Marc2.csv")
rowvec <- t2$spacing

par(mar=c(4,4,1,2))
forest(slab = t2$type,
       x = t2$HR, ci.ub = t2$high, ci.lb = t2$low, 
       ilab = t1[, 6:7], ilab.xpos = c(-1,-0.5),
       refline = 1, rows = rowvec,
       xlab = "Mulitivariable-adjusted HR",
       pch = 18, psize = 1.5,
       xlim = c(-3,3),
       ylim = c(1, max(rowvec) + 3))

#Table 3
t3 <- read.csv("Forest Marc3a.csv")
rowvec <- t3$spacing
labeldf <- t3[, c(2,7,8)]
colvec <- rev(c("white", rep("black", 3), "white", rep("black", 3), "white", rep("black", 11)))
colvec <- rev(c(NA, rep("black", 3), NA, rep("black", 3), NA, rep("black", 11)))

#Sets margins (bottom, left, top, right)
par(mar=c(4,4,1,2))
forest(slab = t3$type,
       x = t3$HR, ci.ub = t3$high, ci.lb = t3$low, 
       ilab = labeldf, ilab.xpos = c(-1.7, -0.9, -0.3),
       refline = 1, rows = rowvec,
       xlab = "Mulitivariable-adjusted HR",
       pch = 18, psize = 1.5,
       col = colvec,
       xlim =c(-3, 3.75),
       ylim = c(1, max(rowvec) + 3))

text(3.75, c(2, 8, 14, 19, 25), 
     c("p-trend = 0.5", "p-trend = 0.69", "p-trend = 0.01", "p-trend = 0.01", "p-trend = 0.09"), 
     cex = 1, pos = 2)

# Isa paper amino acids cancer ----

# p-value adjustment
library(readxl)
t1 <- read_xlsx("Forest Isa.xlsx", sheet = 3)

class(t1$trendtest)

p.raw.chisq   <- t1$probchisq #%>% na.omit
p.fdr.chisq   <- p.adjust(t1$probchisq, method = "fdr") #%>% na.omit
p.bonf.chisq  <- p.adjust(t1$probchisq, method = "bonferroni") #%>% na.omit

p.raw.trend   <- t1$trendtest #%>% na.omit
p.fdr.trend   <- p.adjust(t1$trendtest, method = "fdr") #%>% na.omit
p.bonf.trend  <- p.adjust(t1$trendtest, method = "bonferroni") #%>% na.omit

df <- cbind(Cancer = t1$Cancer, Parameter = t1$Parameter, 
            p.raw.chisq, p.fdr.chisq, p.bonf.chisq, p.raw.trend, p.fdr.trend, p.bonf.trend)

#write.csv(df, "FDR adjustment.csv")

# extract FDR adjusted p-values
# colorectal
p.adjust <- round(as.numeric(na.omit(df[1:144, 7])), 3)

# Plots

t4 <- read_xlsx("Forest Isa.xlsx", n_max = 144)

# three groups are rows 1-48, 49-96, 97-144
# lines needed: 48 obs + 13 spaces = 61
vec <- 1:63
# spaces: 6 groups of 2 plus one = 13
spaces <- c(1, 8:9, 16:17, 24:25, 32:33, 40:41, 48:49, 56:57) # spaces
p.pos  <- c(1, 9, 17, 25, 33, 41, 49, 57)

# "All quintiles" data points only
points <- rep(18, 144)
points[seq(1, 144, 6)] <- 5

# the row vector should be length 48
rowvec <- rev(vec[-spaces])
y.lim <- c(1, max(rowvec) + 3)
top <- max(rowvec) + 2

# function to add p-trends
plabs <- function(x) as.expression(bquote(italic(P)~ "-trend =" ~ .(x) ))

dev.off()
par(mfrow=c(1,3))

# Alanine - hypoxanthine
par(mar=c(5,4,0,1))
forest(x = t4$HR, ci.ub = t4$high, ci.lb = t4$low, refline=1, rows = rowvec,
       ilab = t4$Cases, ilab.xpos = 0.45, ilab.pos = 4, ylim = y.lim, cex = 1, efac = 0.3,
       xlab="", psize = 1.5, xlim=c(0.1, 1.6), slab= t4$cmpd, pch = points, subset = 1:48)
# add P-trends
text(1.6, p.pos, sapply(rev(p.adjust[1:8]), plabs), pos = 2)
# add titles
text(0.1, top, "Amino acid", pos=4)
text(0.45, top, "Cases, quintiles 1-5 (total n = 6921)", pos = 4)
text(1.6, top, "HR [95% CI]", pos = 2)

# Isoleucine - threonine
par(mar=c(5,4,0,1))
forest(x = t4$HR, ci.ub = t4$high, ci.lb = t4$low, refline = 1, 
       xlab="Multivariable-adjusted hazard ratio", psize = 1.5,
       ilab = t4$Cases, ilab.xpos = 0.45, ilab.pos = 4, pch = points,
       cex = 1, rows = rowvec, efac = 0.3, ylim= y.lim, xlim=c(0.1, 1.6), subset=49:96, slab= t4$cmpd)
# p-trends
text(1.6, p.pos, sapply(rev(p.adjust[9:16]), plabs), pos = 2)
# titles
text(0.1, top, "Amino acid", pos=4)
text(0.45, top, "Cases, quintiles 1-5 (total n = 6921)", pos = 4)
text(1.6, top, "HR [95% CI]", pos = 2)

# Tryptophan - total proteins
par(mar=c(5,4,0,2))
forest(x = t4$HR, ci.ub = t4$high, ci.lb = t4$low, refline = 1, xlab="", psize = 1.5, pch = points,
       xlim=c(0.1, 1.6), subset=97:144, cex = 1, ilab = t4$Cases, efac = 0.3,
       ilab.xpos = 0.45, ilab.pos = 4, rows = rowvec, ylim= y.lim, slab= t4$cmpd)
# p-trends
text(1.6, p.pos, sapply(rev(p.adjust[17:24]), plabs), pos = 2)
# titles
text(0.1, top, "Amino acid", pos=4)
text(0.1, 32, "Groups", pos=4, font=3)
text(0.45, top, "Cases, quintiles 1-5 (total n = 6921)", pos = 4)
text(1.6, top, "HR [95% CI]", pos = 2)



