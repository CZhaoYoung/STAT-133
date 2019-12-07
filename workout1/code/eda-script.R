# ===================================================================
# Title: exploratory data analysis 
# Description:
#   This script computes descriptive statistics, as well as 
#   various exploratory data visualizations.
# Input(s): data file 'ibtraces-2010-2015.csv'
# Output(s): summary data files, and plots
# Author: Zhaoyang Chen
# Date: 10-16-2019    
# ===================================================================

# ===============================
# 4 Data Importing in R
# ===============================
dat <- read.csv(
file = "~/Desktop/workout1/data/ibtracs-2010-2015.csv", 
na.strings = "-999.", 
sep = ",", 
fill = TRUE, 
header = TRUE, 
colClasses = c("character", "integer", "character", "factor", "character"))

sink(file = '~/Desktop/workout1/output/data-summary.txt')
summary(dat)
sink()

# ===============================
# 5 Data Visualization
# ===============================
# 5.1
map()
points(dat$Longitude, dat$Latitude, col ="red", cex = .3)
png(filename = "~/Desktop/workout1/images/map-all-storms.png")
dev.off()
pdf(file = "~/Desktop/workout1/images/map-all-storms.pdf")
dev.off()


# 5.2
ggplot(data = dat, aes(x = height, y = weight)) 
+ geom_point() 
+ facet_grid(~ season) 
+ ggsave("~/Desktop/workout1/images/map-ep-na-storms-by-month.pdf", width = 10, height = 10)
