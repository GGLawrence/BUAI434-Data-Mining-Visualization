setwd("~/Desktop/DATA/R/R_Datasets")

#Question 7.4
# part 1
library(readr)
LS.df <- data.frame(read_csv("LaptopSales.csv")) #fixed with read*_*csv

# part 2
for (i in names(LS.df)) {
  missing <- sum(is.na.data.frame(LS.df[, i]))
  print(c(i, missing))
}

# part 3
data.frame(mean = sapply(LS.df[, 6:9], mean),
                       sd = sapply(LS.df[, 6:9], sd), 
                       missing = sapply(LS.df[, 6:9], function(x)
                         sum(length(which(is.na(x))))))

# part 4
corr_sub <- subset(LS.df, LS.df$Bundled.Applications == "Yes")
round(cor(corr_sub[c(5, 8, 9)], use = "complete.obs"), 1)

# part 5
LS.df$Binned.Retail.Price <- .bincode(LS.df$Retail.Price, seq(150, 900, 50))


# part 6

# library(dplyr)
# LaptopSales.df %>% group_by(Store.Postcode) %>%
#   summarize(Retail.Price = n()) %>%
#   arrange(desc(Retail.Price))

#assumed that "highest volume of sale" means the largest number of sales transaction occurrences

library(plyr)
sale.volume <- count(LS.df,"Store.Postcode") 
t5.code <- sale.volume[order(-sale.volume$freq),][1:5,] 
t5.code
topfivedata <- LS.df[LS.df$Store.Postcode %in% t5.code$Store.Postcode, ]
topfive.clean <- na.omit(topfivedata)
table1 <- aggregate(topfive.clean$Retail.Price, by = list(Store = topfive.clean$Store.Postcode, BundledApp = topfive.clean$Bundled.Applications.), FUN = mean, data = topfive.clean)
table1


# part 7
library(reshape) 
mlt <- melt(topfive.clean, id = c("Store.Postcode", "Integrated.Wireless."), measure=c("Retail.Price"))
cast(mlt, Integrated.Wireless.~Store.Postcode, margins = c("grand_row", "grand_col"), sum)


# part 8
library(ggmap)
tbl <- table(mlt$Integrated.Wireless.,mlt$Store.Postcode)
tbl.topfive <- tbl[,t5.code$Store.Postcode]
tbl.topfive
prop.tbl <- prop.table(tbl.topfive, margin = 2)
prop.tbl

barplot(prop.tbl, xlab = "Store Postcode", ylab = "Percentage of Integrated Wireless", yaxt = "n", main = "Distribution of Top Selling 5 Store by Integrated Wireless",)
axis(2, at = (seq(0,1, 0.5)), paste(seq(0,100,50), "%"))