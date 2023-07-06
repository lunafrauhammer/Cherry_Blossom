#### Prediction ####
setwd("/Users/lunafrauhammer/Library/CloudStorage/OneDrive-UniversitaetDuisburg-Essen/Sonstiges/CherryBlossom/peak-bloom-prediction-main/data")
doy_to_date <- function (year, doy) {
  strptime(paste(year, doy, sep = '-'), '%Y-%j') %>% # create date object
    strftime('%Y-%m-%d') # translate back to date string in ISO 8601 format
}

# read in data 
cherry <- read.csv("washingtondc.csv") %>% 
  bind_rows(read.csv("liestal.csv")) %>% 
  bind_rows(read.csv("kyoto.csv"))
head(cherry)
summary(cherry$location)
cherry$locationF <- as.factor(cherry$location)
summary(cherry$locationF)
cherry <- subset(cherry, subset = year >=1880)
cherry$city <- NA
cherry[cherry$location == "kyoto", ]$city <- "kyoto1" 
cherry[cherry$location == "liestal", ]$city <- "liestal1" 
cherry[cherry$location == "washingtondc", ]$city <- "washingtondc1" 



city_Colors<- c(kyoto = "lightpink1", liestal = "lightskyblue", washingtondc = "palegreen3") 

ggplot(cherry, aes(x = year, y = bloom_doy)) + 
  geom_text(label = "\u273F", aes(color = location),  
            size=4, family = "Arial Unicode MS", alpha = 1) +
 #  scale_color_discrete(name = "location") + 
  theme_classic() + 
  labs(x = "Year", y = "Full Blossom Day of the Year") + 
  geom_smooth(method = "lm", fullrange = TRUE, se = FALSE, aes(color = location), show.legend = FALSE) +
 # geom_smooth(method = "lm", fullrange = TRUE, aes(fill = location), alpha = 0.2,) 
  scale_color_manual(values=city_Colors)


### Liestal ###

cherry_Lies <- subset(cherry, location == "liestal")

# now use data from temperature data set: temp_Lies

nrow(temp_Lies)
dat_Lies <- merge(cherry_Lies, temp_Lies, by = "year")
head(dat_Lies)

mYear <- lm(bloom_doy ~ year, dat_Lies)
summary(mYear) # R^2 = 0.13

mDRD <- lm(bloom_doy ~ DRD, dat_Lies)
summary(mDRD) # R^2 = 0.35

mtpred <- lm(bloom_doy ~ t.pred, dat_Lies)
summary(mtpred) # R^2 = 0.48

mjan <- lm(bloom_doy ~ jan, dat_Lies)
summary(mjan) # R^2 = 0.06

mAll <- lm(bloom_doy ~ year + DRD + t.pred + jan, dat_Lies)
summary(mAll) # R^2 = 0.68

mAll.2 <- lm(bloom_doy ~ year + DRD * t.pred + jan, dat_Lies)
summary(mAll.2) # R^2 = 0.71

plot()
# the colder the DRD modeled temperature is the later is the bloom DOY

plot(bloom_doy ~ t.pred, dat_Lies, cex = 0.5, pch = 19,
     ylim = c(75, 122), xlim = c(-20, 85), 
     ylab = "Full Bloom Day of the Year", 
     xlab = "Modeled Temperature at DRD", bty = "n")
box(bty = "L")
with(dat_Lies, text((bloom_doy -1) ~ t.pred, labels = year, cex = 0.5))
abline(mtpred)

# I am considering to exclude the 1963 data point, however, the cooks-distance 
# is still okay so I will leave it. 
cooks.distance(mtpred)

mwithout63 <- lm(bloom_doy ~ t.pred, subset(dat_Lies, year != 1963))
summary(mwithout63)
abline(mwithout63, col = "grey")
mwithout63.2 <- lm(bloom_doy ~ t.pred + I(t.pred^2), subset(dat_Lies, year != 1963))
summary(mwithout63.2) # not really better


#### Predicition ####
# before I calculate the prediction for 2023, I check the RMSE. 
# calculate RMSE
install.packages("Metrics")
library(Metrics)
pbloomY <- predict(mYear, newdata = data.frame(year = dat_Lies$year))
rmse(dat_Lies$bloom_doy, pbloomY) # 10.85
mae(dat_Lies$bloom_doy, pbloomY) # 9.22
cor(dat_Lies$bloom_doy, pbloomY) # 0.46
pbloomD <- predict(mDRD, newdata = data.frame(DRD = dat_Lies$DRD))
rmse(dat_Lies$bloom_doy, pbloomD) # 9.34
mae(dat_Lies$bloom_doy, pbloomD) # 7.30
cor(dat_Lies$bloom_doy, pbloomD) # 0.60
pbloomT <- predict(mtpred, newdata = data.frame(t.pred = dat_Lies$t.pred))
rmse(dat_Lies$bloom_doy, pbloomT) # 8.32
mae(dat_Lies$bloom_doy, pbloomT) # 6.60
cor(dat_Lies$bloom_doy, pbloomT) # 0.70

pbloomA <- predict(mAll, newdata = data.frame(year = dat_Lies$year, DRD = dat_Lies$DRD, 
                                              t.pred = dat_Lies$t.pred, jan = dat_Lies$t.pred) )
rmse(dat_Lies$bloom_doy, pbloomA) # 7.10 not really super 
mae(dat_Lies$bloom_doy, pbloomA) # 5.06
cor(dat_Lies$bloom_doy, pbloomA) # 0.79


pbloomA.2 <- predict(mAll.2, newdata = data.frame(year = dat_Lies$year, DRD = dat_Lies$DRD, 
                                                t.pred = dat_Lies$t.pred, jan = dat_Lies$t.pred) )
rmse(dat_Lies$bloom_doy, pbloomA.2) # 6.61 not really super 
mae(dat_Lies$bloom_doy, pbloomA.2) # 4.94
cor(dat_Lies$bloom_doy, pbloomA.2) # 0.83

# I will now predict the 2023 bloom date for Liestal. To do this, I need all 
# the information from my model for 2023. 

data23 <- data.frame(DRD = DRD23, t.pred = tpred23 * 10, year = 2023, jan = jan23)

predict(mDRD, newdata = data.frame(DRD = DRD23))
predict(mAll, newdata = data23) 
# my prediction for 2023 is 93.12!



with(dat_Lies, plot(bloom_doy ~ year, pch = 19))
points(2023, 93.12, col = "red", pch = 19)

plot(NULL, ylim = c(73, 123), xlim = c(1950, 2025), 
     ylab = "Full Bloom Day of the Year", 
     xlab = "Year", bty = "L", main = "Liestal")
text(bloom_doy ~ year, dat_Lies, family = "Arial Unicode MS", label = "\u273F")
text(2023, 93.12, family = "Arial Unicode MS", label = "\u273F", 
     col = "hotpink", cex = 1.5)
abline(lm(bloom_doy ~ year, dat_Lies))

doy_to_date(2023, 93)




install.packages("scatterplot3d") # Install
library(scatterplot3d)

s3d <- with(dat_Lies, scatterplot3d(x = DRD, y = year, z = bloom_doy, 
                                    angle = 60, pch = 19,
                                    grid = FALSE))
s3d$plane3d(mYDRD)

mYDRD <- lm(bloom_doy ~ DRD + year, dat_Lies)







