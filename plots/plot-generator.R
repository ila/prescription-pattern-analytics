# libraries
require("ggplot2")
require("scales")
require("gridExtra")
require("lubridate")
require("grid")


# setting file path
csv_path = "/Users/ila/Desktop/codes/cmr-internship/plots/csv/"
image_path = "/Users/ila/Desktop/codes/cmr-internship/plots/"


# plotting ATC trends for years
atcyear <- read.csv(paste(csv_path, "top_atc-year.csv", sep=""))

# removing 2018 values [incomplete year]
atcyear <- atcyear[which(atcyear$anno <= 2017),]

# plotting and saving as .png
png(filename=paste(image_path, "top_atc-year.png", sep=""), width=2200, height=1100, res=200)

  atcyearplot <- ggplot(atcyear, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + scale_y_continuous(breaks = seq(0, 200000, by=10000), labels=comma) + labs(x="Year", y="Total prescriptions", color="ATC code") + scale_color_discrete(name="ATC code", labels=c("Rifaximin", "Amoxicillin", "Amoxicillin and \nbeta-lactamase inhibitor", "Clarithromycin", "Ciprofloxacin", "Levofloxacin"))

  print(atcyearplot)

dev.off()


# plotting ATC trends for months
atcmonth <- read.csv(paste(csv_path, "top_atc-month.csv", sep=""))

# parsing to date and removing 2018 values [incomplete year]
atcmonth$mese <- as.Date(atcmonth$mese)
atcmonth <- atcmonth[which(atcmonth$mese <= as.Date("2018-06-01")),]

# plotting and saving as .png
png(filename=paste(image_path, "top_atc-month.png", sep=""), width=2200, height=1100, res=200)

  atcmonthplot <- ggplot(atcmonth, aes(x=mese, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months")) + scale_y_continuous(breaks=seq(0, 30000, by=1000), labels=comma) + labs(x="Month", y="Total prescriptions") + scale_color_discrete(name="ATC code", labels=c("Rifaximin", "Amoxicillin", "Amoxicillin and \nbeta-lactamase inhibitor", "Ceftriaxone", "Clarithromycin", "Ciprofloxacin", "Levofloxacin"))

  print(atcmonthplot)

dev.off()


# A class antibiotics trend by month
atcamonth <- read.csv(paste(csv_path, "top_atc_a-month.csv", sep=""))

# parsing to date and removing 2018 values [incomplete year]
atcamonth$mese <- as.Date(atcamonth$mese)
atcamonth <- atcamonth[which(atcamonth$mese <= as.Date("2018-06-01")),]

# plotting and saving as .png
png(filename=paste(image_path, "top_atc_a-month.png", sep=""), width=2200, height=1100, res=200)

  atcamonthplot <- ggplot(atcamonth, aes(x=mese, y=totale, color=co_atc)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months")) + scale_y_continuous(breaks=seq(0, 30000, by=1000), labels=comma) + labs(x="Month", y="Total prescriptions") + scale_color_discrete(name="ATC code", labels=c("Nystatin", "Paromomycin", "Rifaximin"))

  print(atcamonthplot)

dev.off()


# plotting AIC trends
aicyear <- read.csv(paste(csv_path, "top_aic-year.csv", sep=""))

# turning values to factors and removing 2018
aicyear$co_codifa <- factor(aicyear$co_codifa)
aicyear <- aicyear[which(aicyear$anno <= 2017),]

png(filename=paste(image_path, "top_aic-year.png", sep=""), width=2200, height=1100, res=200)

  aicyearplot <- ggplot(aicyear, aes(x=anno, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 100000, by=5000), labels=comma) + labs(x="Year", y="Total prescriptions") + scale_color_discrete(name="AIC code", labels=c("Velamox", "Normix", "Monuril", "Augmentin", "Ciproxin", "Levoxacin"))

  print(aicyearplot)

dev.off()


# AIC months
aicmonth <- read.csv(paste(csv_path, "top_aic-month.csv", sep=""))

aicmonth$co_codifa <- factor(aicmonth$co_codifa)
aicmonth$mese <- as.Date(aicmonth$mese)
aicmonth <- aicmonth[which(aicmonth$mese <= as.Date("2018-06-01")),]

png(filename=paste(image_path, "top_aic-month.png", sep=""), width=2200, height=1100, res=200)

  aicmonthplot <- ggplot(aicmonth, aes(x=mese, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months")) + scale_y_continuous(breaks=seq(0, 13000, by=1000), labels=comma) + labs(x="Month", y="Total prescriptions") + scale_color_discrete(name="AIC code", labels=c("Velamox", "Rocefin", "Normix", "Monuril", "Augmentin", "Ciproxin", "Zitromax", "Levoxacin"))
  
  print(aicmonthplot)
  
dev.off()


# zooming on 4 AIC codes
aic4 <- read.csv(paste(csv_path, "aic_4-year.csv", sep=""))

# turning values to factors
aic4$co_codifa <- factor(aic4$co_codifa)

png(filename=paste(image_path, "aic_4-year.png", sep=""), width=2200, height=1100, res=200)

  aic4plot <- ggplot(aic4, aes(x=date_part, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 100000, by=5000), labels=comma) + labs(x="Year", y="Total prescriptions") + scale_color_discrete(name="AIC code", labels=c("Velamox", "Normix", "Augmentin", "Levoxacin"))

  print(aic4plot)

dev.off()


# 4 AIC codes for a subset of patients
aic4subset <- read.csv(paste(csv_path, "aic_4_subset-year.csv", sep=""))

# turning values to factors and removing 2018
aic4subset$co_codifa <- factor(aic4subset$co_codifa)

png(filename=paste(image_path, "aic_4_subset-year.png", sep=""), width=2200, height=1100, res=200)

  aic4subsetplot <- ggplot(aic4subset, aes(x=date_part, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 100000, by=5000), labels=comma) + labs(x="Year", y="Total prescriptions") + scale_color_discrete(name="AIC code", labels=c("Velamox", "Normix", "Augmentin", "Levoxacin"))

  print(aic4subsetplot)

dev.off()


# zooming on top AIC of a subset of patients
topaicsubset <- read.csv(paste(csv_path, "top_aic_subset-year.csv", sep=""))

# turning values to factors and removing 2018
topaicsubset$co_codifa <- factor(topaicsubset$co_codifa)

png(filename=paste(image_path, "top_aic_subset-year.png", sep=""), width=2200, height=1100, res=200)

  topaicsubsetplot <- ggplot(topaicsubset, aes(x=date_part, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 100000, by=5000), labels=comma) + labs(x="Year", y="Total prescriptions") + scale_color_discrete(name="AIC code", labels=c("Velamox", "Normix", "Monuril", "Augmentin", "Ciproxin", "Levoxacin"))

  print(topaicsubsetplot)

dev.off()

# ATC compared to AIC
# extracting subsets
aic1 <- subset(aicmonth, co_codifa == "26089019")
atc1 <- subset(atcmonth, co_atc == "J01CR02")

colnames(aic1) <- c("codice", "mese", "count")
colnames(atc1) <- c("codice", "mese", "count")
aicatc <- rbind(aic1, atc1)

png(filename=paste(image_path, "atc_aic-month.png", sep=""), width=2200, height=1100, res=200)

  atcaic <- ggplot(aicatc, aes(x=mese, y=count, colour=codice)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months")) + scale_y_continuous(breaks=seq(0, 50000, by=2000), labels=comma) + labs(x="Month", y="Total prescriptions") + scale_color_discrete(name="Code", label=c("Augmentin", "Amoxicillin"))

  print(atcaic)

dev.off()


# plotting Velamox trends
velamox <- read.csv(paste(csv_path, "velamox-year.csv", sep=""))

# plotting and saving as .png
png(filename=paste(image_path, "velamox-year.png", sep=""), width=2200, height=1100, res=200)

  velamoxplot <- ggplot(velamox, aes(x=anno, y=count)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + scale_y_continuous(breaks = seq(0, 200000, by=5000), labels=comma) + labs(x="Year", y="Total prescriptions") 

  print(velamoxplot)

dev.off()


# trending by age range
# to finish: eventually plot by AIC, ATC instead of age, fix labels
augmentinageyear <- data.frame(year=numeric(0), range=numeric(0), count=numeric(0))

plotsatcyear <- list()
plotsatcmonth <- list()
plotsaicyear <- list()
plotsaicmonth <- list()

atcyearlabels <- list(
  c("Amoxicillin", "Amoxicillin and \nbeta-lactamase inhibitor", "Ceftriaxone", "Clarithromycin", "Azithromycin", "Ciprofloxacin"),
  c("Amoxicillin", "Amoxicillin and \nbeta-lactamase inhibitor", "Clarithromycin", "Azithromycin", "Ciprofloxacin"),
  c("Rifaximin", "Amoxicillin", "Amoxicillin and \nbeta-lactamase inhibitor", "Clarithromycin", "Ciprofloxacin", "Levofloxacin"),
  c("Rifaximin", "Amoxicillin", "Amoxicillin and \nbeta-lactamase inhibitor", "Ceftriaxone", "Ciprofloxacin", "Levofloxacin")
  )
atcmonthlabels <- c()
aicyearlabels <- c()
aicmonthlabels <- c()

for (range in 2:5) {
  
  atc_year <- read.csv(paste(csv_path, "age/range", range, "/top_atc_", range, "-year.csv", sep=""))
  atc_month <- read.csv(paste(csv_path, "age/range", range, "/top_atc_", range, "-month.csv", sep=""))
  aic_year <- read.csv(paste(csv_path, "age/range", range, "/top_aic_", range, "-year.csv", sep=""))
  aic_month <- read.csv(paste(csv_path, "age/range", range, "/top_aic_", range, "-month.csv", sep=""))
  
  atc_month$mese <- as.Date(atc_month$mese)
  aic_month$mese <- as.Date(aic_month$mese)
  
  atc_month <- atc_month[which(atc_month$mese <= as.Date("2018-06-01")),]
  atc_year <- atc_year[which(atc_year$anno <= 2017),]
  aic_month <- aic_month[which(aic_month$mese <= as.Date("2018-06-01")),]
  aic_year <- aic_year[which(aic_year$anno <= 2017),]
  
  aic_year$co_codifa <- factor(aic_year$co_codifa)
  aic_month$co_codifa <- factor(aic_month$co_codifa)
  
  for (year in 2008:2017) {
    
    augmentinageyear <- rbind(augmentinageyear, data.frame(year=year, range=range, count=aic_year[which(aic_year$anno == year & aic_year$co_codifa == '26089019'),]$count))
  
  }
  
  plotsatcyear[[length(plotsatcyear) + 1]] <- ggplot(atc_year, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + scale_y_continuous(breaks = seq(0, 200000, by=5000), labels=comma, limits=c(0, 60000)) + labs(x="Year", y="Total ATC prescriptions") + ggtitle(paste("Range ", range, sep="")) + scale_color_discrete(name="ATC code", label=atcyearlabels[range + 1])
    
  plotsatcmonth[[length(plotsatcmonth) + 1]] <- ggplot(atc_month, aes(x=mese, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("1 year")) + scale_y_continuous(breaks=seq(0, 30000, by=1000), labels=comma, limits=c(0, 6000)) + labs(x="Month", y="Total ATC prescriptions") + ggtitle(paste("Range ", range, sep="")) #+ scale_color_discrete(name="ATC code", label=atcmonthlabels[range])
  
  plotsaicyear[[length(plotsaicyear) + 1]] <- ggplot(aic_year, aes(x=anno, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + scale_y_continuous(breaks = seq(0, 200000, by=5000), limits=c(0, 45000), labels=comma) + labs(x="Year", y="Total AIC prescriptions") + ggtitle(paste("Range ", range, sep="")) #+ scale_color_discrete(name="AIC code", label=aicyearlabels[range])
    
  plotsaicmonth[[length(plotsaicmonth) + 1]] <- ggplot(aic_month, aes(x=mese, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("1 year")) + scale_y_continuous(breaks=seq(0, 30000, by=1000), labels=comma, limits=c(0, 5000)) + labs(x="Month", y="Total AIC prescriptions") + ggtitle(paste("Range ", range, sep="")) #+ scale_color_discrete(name="AIC code", label=aicmonthlabels[range])
  
}

png(filename=paste(image_path, "top_atc_age-year.png", sep=""), width=4200, height=2700, res=300)
  grid.arrange(grobs=plotsatcyear, ncol=2, top=(textGrob(paste("ATC codes for year", sep=""), gp=gpar(fontsize=20))))
dev.off()

png(filename=paste(image_path, "top_atc_age-month.png", sep=""), width=4200, height=2700, res=300)
  grid.arrange(grobs=plotsatcmonth, ncol=2, top=(textGrob(paste("ATC codes for month", sep=""), gp=gpar(fontsize=20))))
dev.off()

  png(filename=paste(image_path, "top_aic_age-year.png", sep=""), width=4200, height=2700, res=300)
grid.arrange(grobs=plotsaicyear, ncol=2, top=(textGrob(paste("AIC codes for year", sep=""), gp=gpar(fontsize=20))))
dev.off()

png(filename=paste(image_path, "top_aic_age-month.png", sep=""), width=4200, height=2700, res=300)
grid.arrange(grobs=plotsaicmonth, ncol=2, top=(textGrob(paste("AIC codes for month", sep=""), gp=gpar(fontsize=20))))
dev.off()


# plotting Augmentin trends
augmentin <- read.csv(paste(csv_path, "augmentin-year.csv", sep=""))

# plotting and saving as .png
png(filename=paste(image_path, "augmentin-year.png", sep=""), width=2200, height=1100, res=200)

  augmentinplot <- ggplot(augmentin, aes(x=anno, y=count)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + scale_y_continuous(breaks = seq(0, 200000, by=5000), labels=comma) + labs(x="Year", y="Total prescriptions") 

  print(augmentinplot)

dev.off()


# trends for age
augmentinageyear$range = as.factor(augmentinageyear$range)

png(filename=paste(image_path, "augmentin_age-year.png", sep=""), width=2200, height=1100, res=200)

  augmentinageplot <- ggplot(augmentinageyear, aes(x=year, y=count, colour=range)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017)) + scale_y_continuous(breaks = seq(0, 200000, by=5000), labels=comma) + labs(x="Year", y="Total prescriptions") + scale_color_discrete(name="Age range", label=c("15-24", "25-44", "45-64", "65+"))

  print(augmentinageplot)

dev.off()


# women and men by year using ATC code
atcwomenyear <- read.csv(paste(csv_path, "top_atc_women-year.csv", sep=""))
atcmenyear <- read.csv(paste(csv_path, "top_atc_men-year.csv", sep=""))

# removing 2018 values [incomplete year]
atcwomenyear <- atcwomenyear[which(atcwomenyear$anno <= 2017),]
atcmenyear <- atcmenyear[which(atcmenyear$anno <= 2017),]

# plotting and saving as .png
png(filename=paste(image_path, "top_atc_sex-year.png", sep=""), width=2500, height=1200, res=200)

  women <- ggplot(atcwomenyear, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 110000, by=5000), limits=c(0, 110000), labels=comma) + labs(x="Year", y="Total prescriptions") + ggtitle("Women") + scale_color_discrete(name="ATC code", labels=c("Rifaximin", "Amoxicillin", "Amoxicillin and \nbeta-lactamase \ninhibitor", "Ciprofloxacin"))
  
  men <- ggplot(atcmenyear, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 110000, by=5000), limits=c(0, 110000), labels=comma) + labs(x="Year", y="Total prescriptions") + ggtitle("Men") + scale_color_discrete(name="ATC code", labels=c("Rifaximin", "Amoxicillin", "Amoxicillin and \nbeta-lactamase \ninhibitor", "Ciprofloxacin", "Levofloxacin"))

  print(grid.arrange(women, men, ncol=2))

dev.off()

# 
# # mf
# mf$mese <- as.Date(mf$mese)
# a <- mf[which(mf$co_atc == 'A07AA11'),]
# j <- mf[which(mf$co_atc == 'J01MA12'),]
# plot1 <- ggplot(a, aes(x=mese, y=count, color=sesso)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("1 year")) + scale_y_continuous(breaks=seq(0, 30000, by=500), limits=c(0, 4500)) + labs(x="Mese", y="Totale prescrizioni", color="Codice ATC") + ggtitle("A07AA11")
# plot2 <- ggplot(j, aes(x=mese, y=count, color=sesso)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("1 year")) + scale_y_continuous(breaks=seq(0, 30000, by=500), limits=c(0, 4500)) + labs(x="Mese", y="Totale prescrizioni", color="Codice ATC") + ggtitle("J01MA12")
# grid.arrange(plot1, plot2, ncol=2)
# 
# # anno
# a$mese <- format(a$mese, format="%Y")
# j$mese <- format(j$mese, format="%Y")
# aanno <- aggregate(a$count, by=list(anno=a$mese, sesso=a$sesso), FUN=sum)
# janno <- aggregate(j$count, by=list(anno=a$mese, sesso=a$sesso), FUN=sum)
# plot1 <- ggplot(aanno, aes(x=anno, y=x, group=sesso, color=sesso)) + geom_point() + geom_line() + scale_y_continuous(breaks=seq(0, 45000, by=5000), limits=c(0, 45000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC") + ggtitle("A07AA11")
# plot2 <- ggplot(janno, aes(x=anno, y=x, group=sesso, color=sesso)) + geom_point() + geom_line() + scale_y_continuous(breaks=seq(0, 45000, by=5000), limits=c(0, 45000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC") + ggtitle("J01MA12")


# trending by province
provinces <- read.csv(paste(csv_path, "provinces.csv", sep=""))
provinces <- provinces[which(provinces$anno <= 2017),]
provinces$provincia = factor(provinces$provincia)

png(filename=paste(image_path, "provinces.png", sep=""), width=2200, height=1100, res=200)

  provincesplot <- ggplot(provinces, aes(x=anno, y=count, color=provincia)) + scale_color_discrete(name="Province", breaks=c(61, 62, 63, 64, 65), labels=c("Caserta", "Benevento", "Napoli", "Avellino", "Salerno")) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 1000000, by=50000), labels=comma) + labs(x="Year", y="Total prescriptions") 
  
  print(provincesplot)

dev.off()


# distribution of number of prescriptions during the years
pnyear <- read.csv(paste(csv_path, "prescriptions_number-year.csv", sep=""))

plots_year <- list()
valuesyear <- c()

for (year in 2008:2017) {
  plots_year[[length(plots_year) + 1]] <- ggplot(pnyear[which(pnyear$anno == year),], aes(x=factor(np), y=count)) + geom_bar(stat="identity") + labs(x="Number of prescriptions", y="Number of patients") + ggtitle(year) + scale_y_log10(limits=c(1,1e5), labels=comma)
  
  valuesyear <- c(valuesyear, pnyear[which(pnyear$anno == year & pnyear$np == 1),]$count)
}

statsyear <- c(mean(valuesyear), var(valuesyear), sd(valuesyear))

png(filename=paste(image_path, "prescriptions_number-year.png", sep=""), width=8000, height=8000, res=300)

  print(do.call("grid.arrange", c(plots_year, ncol=2)))

dev.off()


# prescriptions by months in 2017
pnmonth <- read.csv(paste(csv_path, "prescriptions_number-month.csv", sep=""))

pnmonth$data <- as.Date(pnmonth$data)
pnmonth2017 <- pnmonth[which(pnmonth$data > as.Date("2016-12-01") & pnmonth$data < as.Date("2018-01-01")),]

plots_month <- list()
valuesmonth <- c()

for (month in 1:12) {
  plot <- pnmonth2017[which(month(pnmonth2017$data) == month),]
  
  plots_month[[length(plots_month) + 1]] <- ggplot(plot, aes(x=factor(np), y=count)) + geom_bar(stat="identity") + geom_vline(aes(xintercept=mean(count))) + labs(x="Number of prescriptions", y="Number of patients") + ggtitle(month) + scale_y_log10(limits=c(1,1e5), labels=comma)
  
  valuesmonth <- c(valuesmonth, plot[which(plot$np == 1),]$count)
  
}

statsmonth <- c(mean(valuesmonth), var(valuesmonth), sd(valuesmonth))

png(filename=paste(image_path, "prescriptions_number-month.png", sep=""), width=5000, height=8000, res=300)

  print(do.call("grid.arrange", c(plots_month, ncol=2)))

dev.off()

