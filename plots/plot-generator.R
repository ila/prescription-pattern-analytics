# libraries
require("ggplot2")
require("scales")
require("gridExtra")
require("lubridate")


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

# turning values to factors and removing 2018
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

# ATC
# # trasformare a factor e date
# codifa1 <- subset(codifa, co_codifa == "26089019")
# topatc1 <- subset(topatc, co_atc == "J01CR02")
# ggplot(topatc1, aes(x=mese, y=count)) + geom_line(color="cyan3") + geom_point(color="cyan3") + scale_y_continuous(breaks = seq(0, 25000, by=1000)) + geom_line(data=codifa1, aes(x=mese, y=count), color="blue2") + geom_point(data=codifa1, aes(x=mese, y=count), color="blue2") + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months"))
# 

# # donne
# plot1 <- ggplot(donne, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 110000, by=5000), limits=c(0, 110000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC") + ggtitle("Donne")
# plot2 <- ggplot(uomini, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 110000, by=5000), limits=c(0, 110000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC") + ggtitle("Uomini")
# grid.arrange(plot1, plot2, ncol=2)
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
# 
# # province
# ggplot(provincie, aes(x=anno, y=count, color=provincia)) + scale_color_discrete(name="Provincia", breaks=c(61, 62, 63, 64, 65), labels=c("Caserta", "Benevento", "Napoli", "Avellino", "Salerno")) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 1000000, by=50000)) + labs(x="Anno", y="Totale prescrizioni") 
# 

# distribution of number of prescriptions during the years
pnyear <- read.csv(paste(csv_path, "prescriptions_number-year.csv", sep=""))

plots <- list()
values <- c()

for (year in 2008:2017) {
  plots[[length(plots) + 1]] <- ggplot(pnyear[which(pnyear$anno == year),], aes(x=factor(np), y=count)) + geom_bar(stat="identity") + geom_vline(aes(xintercept=mean(count))) + labs(x="Number of prescriptions", y="Number of patients") + ggtitle(year) + scale_y_log10(limits = c(1,1e5), labels=comma)
  
  values <- c(values, pnyear[which(pnyear$anno == year & pnyear$np == 1),]$count)
}

statsyear <- c(mean(values), var(values), sd(values))

do.call("grid.arrange", c(plots, ncol=2))

png(filename=paste(image_path, "prescriptions_number-year.png", sep=""), width=8000, height=8000, res=300)

  print(do.call("grid.arrange", c(plots, ncol=2)))

dev.off()

# months in 2017
pnmonth <- read.csv(paste(csv_path, "prescriptions_number-month.csv", sep=""))

pnmonth$data <- as.Date(pnmonth$data)
pnmonth2017 <- pnmonth[which(pnmonth$data > as.Date("2016-12-01") & pnmonth$data < as.Date("2018-01-01")),]

plots <- list()

for (month in 1:12) {
  plot <- pnmonth2017[which(month(pnmonth2017$data) == month),]
  
  plots[[length(plots) + 1]] <- ggplot(plot, aes(x=factor(np), y=count)) + geom_bar(stat="identity") + geom_vline(aes(xintercept=mean(count))) + labs(x="Number of prescriptions", y="Number of patients") + ggtitle(month) + scale_y_log10(limits = c(1,1e5), labels=comma)
  
}

png(filename=paste(image_path, "prescriptions_number-month.png", sep=""), width=5000, height=8000, res=300)

  print(do.call("grid.arrange", c(plots, ncol=2)))

dev.off()

