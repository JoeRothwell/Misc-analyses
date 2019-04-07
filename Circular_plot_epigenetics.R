library(tidyverse)
# old data
dat <- read.csv("MCF7LIB11.csv") %>% 
            rename(region = factors, y = ratio) %>% 
            #group_by(region) %>% 
            #mutate(pos.y = ifelse(y > 0, y, NA), neg.y = ifelse(y < 0, y, NA))
            mutate(pos.y = ifelse(y > 0, y, 0), neg.y = ifelse(y < 0, -y, 0))
dat$region <- as.factor(dat$region) %>% fct_reorder(dat$pos)

# Prepare new data, putting the two sections of Dloop together -------------------------------------------------


dat.all <- read.csv("MT3.csv")
dat.old <- dat.all %>% slice(1:16023)
dat.new <- dat.all %>% slice(16024:n())

# arrow vectors to join to data
outside <- rep(NA, 16569)
outside[c(57, 545)] <- 1

inside <- rep(NA, 16569)
inside[c(325, 5721)] <- 1

#Create new variables for plot, giving non-methylated Cs a value of 1 so lines fill the whole track
dat     <- bind_rows(dat.new, dat.old) %>% rename(region = factors) %>% select(-hypo) %>%
            mutate(   pos  = 1:n(), 
                        y  = ifelse(is.na(ratio), 0, ratio),
                    pos.y  = ifelse(y > 0, y, 0), 
                    neg.y  = ifelse(y < 0, -y, 0),
                   nometh  = ifelse(seq == "c" & y == 0, 1, NA),
                   outside = outside,
                   inside  = inside)

# gene regions
gen.region <- read.csv("Vibha data/genome regions annotated1.csv")
genreg.long <- gen.region %>% gather(end, position, -Region.ID, -Symbol, -Region.Name, -Include ) %>%
  filter(Include == T) %>%
  arrange(position) %>%
  select(end, position)

dat <- dat %>% left_join(genreg.long, by="position")

dat$region <- dat$region %>% fct_reorder(dat$pos)

# subset df to not plot gap regions
#dat1 <- dat
dat1 <- filter(dat, !(region %in% c("G2", "G4"))) %>% droplevels()
#dat1 <- filter(dat, !(region %in% c("G1", "G2", "G3", "G4"))) %>% droplevels()
#dat1 <- dat
# gaps are regions 15, 20, 23, 39
#gaps <-  c(rep(0, 41), 5) # One gap only for the key, all others zero
#gaps <-  c(rep(0, 19), 2, rep(0, 18), 2, rep(0, 2), 5) # gaps for gap regions
gaps <-  c(rep(1, 19), 2, rep(1, 18), 2, rep(1, 2), 5) # gaps for gap regions

# Plot circular graph --------------------------------------------------------------------------------------------

library(circlize)

circos.clear()

# General parameters and initialise. There are 42 regions
par(mar = c(1, 1, 1, 1), lwd = 0.1, cex = 0.7)
circos.par("track.height" = 0.2, cell.padding = c(0, 0, 0, 0), start.degree = 100,
           gap.degree = gaps) #gap.degree for gaps between sectors

#Allocate sectors on the circle with region and X data
circos.initialize(factors = dat$region, x = dat$pos)

#Create plotting regions for the first track. Subset data frame is used
circos.track(factors = dat1$region, y = dat1$neg.y, ylim = c(0, 0.65), bg.border = "grey60", 
            bg.lwd = 1, track.height = 0.13,
                panel.fun = function(x, y) {
                    circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(5, "mm"), 
                                CELL_META$sector.index, facing="clockwise", niceFacing = T)
                    circos.axis(labels = F, col = "grey60")
                    } )

        #Plot track data. Draw y-axis at beginning of D-loop
        circos.yaxis(sector.index = "Dloop")
        circos.trackLines(dat1$region, dat1$pos, dat1$neg.y, lwd = 0.3, type="h", col = "hotpink", baseline = "bottom")

#Second track. Subset data frame is used
circos.track(factors = dat1$region, y = dat1$pos.y, bg.border = "grey60", bg.lwd = 1,
                panel.fun = function(x, y) {
                    circos.axis(labels = F, col = "grey60", h = "bottom", major.tick = F)
                    } )

        #Draw y-axis at beginning of D-loop
        circos.yaxis(sector.index = "Dloop")
        circos.trackLines(dat1$region, dat1$pos, dat1$pos.y, baseline = "bottom", lwd = 0.3, type="h", col = "dodgerblue")

#Third track to indicate non-methylation of C. Subset data frame is used
circos.track(factors = dat1$region, y = dat1$pos.y, bg.border = "grey60", track.height = 0.02, bg.lwd=1,
                panel.fun = function(x, y) {
                    circos.axis(labels = F, col = "grey60", h = "bottom", major.tick = F)
                    } )

        #circos.yaxis(sector.index = "Dloop")
        #circos.trackPoints(dat1$region, dat1$pos, dat1$nometh, pch = 1, cex = 0.5, col="darkgreen")
        circos.trackLines(dat1$region, dat1$pos, dat1$nometh, lwd = 0.3, type="h", col="darkgreen")
        
# Other tracks-------------------------------------------------------------------------------------------------
        
#Fourth track to show genomic regions with lines        
circos.track(factors = dat1$region, y = dat1$pos.y, bg.border = "grey60", track.height = 0.02, bg.lwd=1,
                     panel.fun = function(x, y) {
                       circos.axis(labels = F, col = "grey60", h = "bottom", major.tick = F)
                     } )
        
        #circos.yaxis(sector.index = "Dloop")
        circos.trackLines(dat1$region, dat1$pos, dat1$gen.region, type="s", lwd = 0.3, type="h", col="black")        

#Arrows (correct parameters not yet found)
circos.track(factors = dat1$region, y = dat1$pos.y, bg.border = NA, track.height = 0.02, bg.lwd=1,
                panel.fun = function(x, y) {
                circos.arrow(x1 = 325, x2 = 400, width = 0.2, arrow.head.width = 0.5, arrow.head.length = 0.1)
               #circos.arrow(x1 = 5721, x2 = 5796, width = 0.3, arrow.head.width = 0.25)
                    circos.axis(labels = F, col = "white", h = "bottom", major.tick = F)
                    } )

#Dots for arrows
circos.track(factors = dat1$region, y = dat1$pos.y, bg.border = "white", track.height = 0.005, bg.lwd=1,
             panel.fun = function(x, y) {
               circos.axis(labels = F, col = "white", h = "bottom", major.tick = F)
                    } )

        #circos.yaxis(sector.index = "Dloop")
        circos.trackPoints(dat1$region, dat1$pos, dat1$inside, pch = 1, cex = 0.3, col="black")



circos.clear()

