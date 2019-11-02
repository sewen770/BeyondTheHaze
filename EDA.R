#### INSTALL PACKAGES ####
install.packages("corrplot")


#### LOADING LIBRARIES ####
library(corrplot)


##### EDA #####
nrtM<- read.csv("Data/Borneo_fire_data/MODIS_nrt.csv")
modis<-read.csv("Data/Borneo_fire_data/MODIS_archive.csv")

#### BARPLOT PER MONTH ####
col_<-color.function <- c(colorRampPalette( c( "#104E8B","#ED4D56" ) )(9),colorRampPalette( c("#ED4D56", "#104E8B" ) )(3))

bp<-barplot(table(modis$MONTH[modis$CONFIDENCE>50]),
        main="Forest Fire Frequency and Mean Palm Oil Price(USD/Ton) By Month",
        las=2,
        col=col_,
        names.arg = c("January","February","March","April","May","June","July","August","September","October","November","Decemebr"),
        ) 
###SLASH AND BURN
par(new=T)
li<-aggregate(palm_oil$PPOILUSDM~palm_oil$Month,FUN = mean)
plot(li,type="l",ylab="",xlab="",axes=F,lwd=2.5)
axis(side = 4,ylim=c(500,600))
mtext(side = 4, line = 1, 'Palm Oil Price(USD)')
#legend("topleft",
#       c("Frequency of Forest Fire","Palm Oil Price"))
plot(sgpsi$~joined_oil_over50$MONTH)

#HIGHLY CENTERED AROUND 8,9,10

sgpsi<-read.csv("Data/singapore_psi.csv")
sgpsi$date<-paste0(as.character(sgpsi$Year),"-",as.character(sgpsi$Month),"-",as.character(sgpsi$Day))

modis_eq<-modis[modis$YEAR>=2009,]
modis_eq$date<-paste0(as.character(modis_eq$YEAR),"-",as.character(modis_eq$MONTH),"-",as.character(modis_eq$DAY))

joined<-merge(modis_eq,sgpsi,by.x="date",by.y="date")
plot(joined$PSI_24_hourly~joined$SCAN)

corrplot(cor(only_num), method = "number")



over50<-modis[modis$CONFIDENCE>50,]
over50<-data.frame(LATITUDE=over50$LATITUDE,
                   LONGITUDE=over50$LONGITUDE,
                   BRIGHTNESS=over50$BRIGHTNESS,
                   SCAN=over50$SCAN)

only_num<-data.frame(LATITUDE=joined$LATITUDE,
                     LONGITUDE=joined$LONGITUDE,
                     BRIGHTNESS=joined$BRIGHTNESS,
                     SCAN=joined$SCAN,
                     MONTH=joined$MONTH,
                     CONFIDENCE=joined$CONFIDENCE,
                     PSI_24_hourly=joined$PSI_24_hourly)


month_psi<-data.frame(MONTH=joined$MONTH,
                     CONFIDENCE=joined$CONFIDENCE,
                     PSI_24_hourly=joined$PSI_24_hourly,
                     date=joined$date)
plot(month_psi$PSI_24_hourly[month_psi$CONFIDENCE>50]~month_psi$MONTH[month_psi$CONFIDENCE>50])
unique(month_psi$date[month_psi$MONTH==6&month_psi$PSI_24_hourly>150&month_psi$CONFIDENCE>50])
#OUTLIERS FOUND IN MAY AT 2013-6-20 2013-6-21 2013-6-22
boxplot(sgpsi$PSI_24_hourly~sgpsi$Year)

palm_oil<-read.csv("Data/global_price_palm_oil_usd.csv")
palm_oil$date<-paste0(as.character(palm_oil$Year),"-",as.character(palm_oil$Month),"-",as.character(palm_oil$Day))

joined_oil<-merge(joined,palm_oil,by.x="date",by.y="date")
joined_oil_over50<-joined_oil[joined_oil$CONFIDENCE>50,]
plot(joined_oil_over50$PSI_24_hourly~joined_oil_over50$PPOILUSDM)
cor(joined_oil_over50$PSI_24_hourly,joined_oil_over50$PPOILUSDM)
cor(joined_oil_over50$CONFIDENCE,joined_oil_over50$PPOILUSDM)

plot(joined_oil_over50$PSI_24_hourly[joined_oil_over50$PSI_24_hourly<150]~joined_oil_over50$PPOILUSDM[joined_oil_over50$PSI_24_hourly<150],
     main="Singapore's 24hr-PSI Against Worldwide Price of Palm Oil",
     ylab="24hr PSI",
     xlab="Palm Oil Price",
     pch=16,
     col="green")
lm_psi_oil<-lm(joined_oil_over50$PSI_24_hourly[joined_oil_over50$PSI_24_hourly<150]~joined_oil_over50$PPOILUSDM[joined_oil_over50$PSI_24_hourly<150])
abline(lm_psi_oil,lwd=3,col="red")
plot(lm_psi_oil)
cor(joined_oil_over50$PSI_24_hourly[joined_oil_over50$PSI_24_hourly<150],joined_oil_over50$PPOILUSDM[joined_oil_over50$PSI_24_hourly<150])**2
summary(lm_psi_oil)
lm_psi_oil

plot(table(joined_oil_over50$MONTH))
plot(joined_oil_over50$PSI_24_hourly)
cor(log10(joined_oil_over50$PSI_24_hourly[joined_oil_over50$PSI_24_hourly<150]),joined_oil_over50$PPOILUSDM[joined_oil_over50$PSI_24_hourly<150])


outlier<-joined_oil_over50$date[joined_oil_over50$PSI_24_hourly>150&joined_oil_over50$PPOILUSDM<600&joined_oil_over50$PPOILUSDM>500]
unique(outlier) #2015-10-1
