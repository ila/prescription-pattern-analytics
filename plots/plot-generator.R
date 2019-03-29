library("ggplot2")

f <- file.choose()
topatc <- read.csv(f)

# atc anno
ggplot(topatc, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 200000, by=10000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC")
 
# atc mese
library("scales")
topatc$mese <- as.Date(topatc$mese)
ggplot(topatc, aes(x=mese, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months")) + scale_y_continuous(breaks=seq(0, 30000, by=1000)) + labs(x="Mese", y="Totale prescrizioni", color="Codice ATC")

# rifaximina
r$mese <- as.Date(r$mese)
ggplot(r, aes(x=mese, y=totale)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months")) + scale_y_continuous(breaks=seq(0, 7500, by=500)) + labs(x="Mese", y="Totale prescrizioni")

# antibiotici classe A
topatc$mese <- as.Date(topatc$mese)
ggplot(topatc, aes(x=mese, y=totale, color=co_atc)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months")) + scale_y_continuous(breaks=seq(0, 30000, by=1000)) + labs(x="Mese", y="Totale prescrizioni", color="Codice ATC")

# codifa anno
codifa$co_codifa <- factor(codifa$co_codifa)
ggplot(codifa, aes(x=anno, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 100000, by=5000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice")

# codifa mese
codifa$co_codifa <- factor(codifa$co_codifa)
codifa$mese <- as.Date(codifa$mese)
ggplot(codifa, aes(x=mese, y=count, color=co_codifa)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months")) + scale_y_continuous(breaks=seq(0, 13000, by=1000)) + labs(x="Mese", y="Totale prescrizioni", color="Codice")

# codifa + atc
# trasformare a factor e date
codifa1 <- subset(codifa, co_codifa == "26089019")
topatc1 <- subset(topatc, co_atc == "J01CR02")
ggplot(topatc1, aes(x=mese, y=count)) + geom_line(color="cyan3") + geom_point(color="cyan3") + scale_y_continuous(breaks = seq(0, 25000, by=1000)) + geom_line(data=codifa1, aes(x=mese, y=count), color="blue2") + geom_point(data=codifa1, aes(x=mese, y=count), color="blue2") + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("6 months"))

# test di correlazione codifa + atc
cor(topatc1$count, codifa1$count, method="pearson")
cor(topatc1$count, codifa1$count, method="kendall")
cor(topatc1$count, codifa1$count, method="spearman")

# donne
plot1 <- ggplot(donne, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 110000, by=5000), limits=c(0, 110000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC") + ggtitle("Donne")
plot2 <- ggplot(uomini, aes(x=anno, y=count, color=co_atc)) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 110000, by=5000), limits=c(0, 110000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC") + ggtitle("Uomini")
require(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

# mf
mf$mese <- as.Date(mf$mese)
a <- mf[which(mf$co_atc == 'A07AA11'),]
j <- mf[which(mf$co_atc == 'J01MA12'),]
plot1 <- ggplot(a, aes(x=mese, y=count, color=sesso)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("1 year")) + scale_y_continuous(breaks=seq(0, 30000, by=500), limits=c(0, 4500)) + labs(x="Mese", y="Totale prescrizioni", color="Codice ATC") + ggtitle("A07AA11")
plot2 <- ggplot(j, aes(x=mese, y=count, color=sesso)) + geom_point() + geom_line() + scale_x_date(labels=date_format("%y/%m"), breaks=date_breaks("1 year")) + scale_y_continuous(breaks=seq(0, 30000, by=500), limits=c(0, 4500)) + labs(x="Mese", y="Totale prescrizioni", color="Codice ATC") + ggtitle("J01MA12")
grid.arrange(plot1, plot2, ncol=2)

# anno
a$mese <- format(a$mese, format="%Y")
j$mese <- format(j$mese, format="%Y")
aanno <- aggregate(a$count, by=list(anno=a$mese, sesso=a$sesso), FUN=sum)
janno <- aggregate(j$count, by=list(anno=a$mese, sesso=a$sesso), FUN=sum)
plot1 <- ggplot(aanno, aes(x=anno, y=x, group=sesso, color=sesso)) + geom_point() + geom_line() + scale_y_continuous(breaks=seq(0, 45000, by=5000), limits=c(0, 45000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC") + ggtitle("A07AA11")
plot2 <- ggplot(janno, aes(x=anno, y=x, group=sesso, color=sesso)) + geom_point() + geom_line() + scale_y_continuous(breaks=seq(0, 45000, by=5000), limits=c(0, 45000)) + labs(x="Anno", y="Totale prescrizioni", color="Codice ATC") + ggtitle("J01MA12")

# province
ggplot(provincie, aes(x=anno, y=count, color=provincia)) + scale_color_discrete(name="Provincia", breaks=c(61, 62, 63, 64, 65), labels=c("Caserta", "Benevento", "Napoli", "Avellino", "Salerno")) + geom_point() + geom_line() + scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018)) + scale_y_continuous(breaks = seq(0, 1000000, by=50000)) + labs(x="Anno", y="Totale prescrizioni") 

