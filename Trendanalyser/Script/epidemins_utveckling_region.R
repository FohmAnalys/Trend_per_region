rm(list = ls())

#heading <- "
#------------------------------------------------------------------------------------
#
#
# CONTACT:      Lisa Brouwers
# EMAIL:        analysenheten@folkhalsomyndigheten.se
# PROJECT:      COVID-19 modelling
# STUDY:        Trendanalys
#
#
#
# R version:   	4.0.2
# What:   	   Trendanalys senaste veckorna, per region.
#				
#------------------------------------------------------------------------------------
#\n"



#-----------------------------------------------------------------------------------
# Values (Parametrar för att styra programmet)
#-----------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------
# Directory and paths
#-----------------------------------------------------------------------------------


# Input your own path where folder (end with "/") is located in project.path.
# E.g. project.path 	     <- "C:/Users/Modelling/"


project.path 	<- ""

data.path 		<- paste(project.path,"Data",sep="")
output.path 	<- paste(project.path,"Output",sep="")



#---------------------------------------------------------------------------------------------------
# Read data
#---------------------------------------------------------------------------------------------------


load(file.path(data.path, "incidence_daily_regions.Rdata"))




#---------------------------------------------------------------------------------------------------
# LIBRARIES
#---------------------------------------------------------------------------------------------------

library(xtable)
library(tidyr)
library(incidence)
library(ggplot2)
library(distcrete)
library(epitrix)
library(EpiEstim)
library(lubridate)
library(multcomp)
library(MASS)
library(fastDummies)


## ----'parameters'-------------------------------------------------------------
today <- ymd("2021-05-25") #Sys.Date()
now   <- Sys.time()
analysis_date <- today

# start 7 days before so that first estimation is on 2020-08-01 due to 7 day window.
start.date  <- ymd("2020-08-01") - ddays(7)
start.curve <- ymd("2020-08-01")

period_trend <- 21 - 1


# Analysed Regions
# Dalarna
# Gävleborg
# Gotland
# Halland
# Jämtland_Härjedalen
# Jönköping,
# Kalmar,
# Kronoberg,
# Norrbotten,
# Örebro,
# Östergötland,
# Skåne,
# Sörmland,
# Stockholm,
# Uppsala,
# Värmland,
# Västerbotten,
# Västernorrland,
# Västmanland,
# Västra_Götaland,

All_regions <- c("Dalarna", "Gävleborg", "Gotland", "Halland", "Jämtland_Härjedalen", "Jönköping", "Kalmar", "Kronoberg", 
                 "Norrbotten", "Örebro", "Östergötland", "Skåne", "Sörmland", "Stockholm", "Uppsala", "Värmland", "Västerbotten", 
                 "Västernorrland", "Västmanland", "Västra_Götaland")


for(i in 1:length(All_regions)){
  region <- All_regions[i]
  
  title.plot <- paste(region, today, sep=" ")
  
  
  #-----------------------------------------------------------------------------------
  # Read data
  #-----------------------------------------------------------------------------------
  
  
  incidence_daily <- get(paste("incidence_daily_statd_", region, sep=""))
  
  start_date <- "2020-07-27"
  
  ## Reporting delay: 2 days. But does not work for all regions. Remove 3 days.
  yesterday <- today - 3
  
  incidence_daily <- incidence_daily[which((incidence_daily$dates <= yesterday) & (incidence_daily$dates >= as.Date(start_date))),]
  
  
  
  
  #-----------------------------------------------------------------------------------
  # Edit data/Create variables
  #-----------------------------------------------------------------------------------
  
  
  long_dates <- incidence_daily$dates
  # format dates: abbreviated month - day
  
  incidence_daily$dates_long <- incidence_daily$dates
  
  incidence_daily$dates <- format(as.Date(incidence_daily$dates), "%b %d")
  
  
  inc_date   <- as.Date(long_dates)
  times_each <- incidence_daily$Total
  inc        <- NULL
  inc$Date   <- rep(inc_date, times_each)
  
  
  
  
  #-----------------------------------------------------------------------------------
  # Find peak
  #-----------------------------------------------------------------------------------
  
  inc_obj  <- incidence(inc$Date)
  
  # find peak
  inc_peak <- find_peak(inc_obj)
  
  
  ## ----'plot_incidence_peak'----------------------------------------------------
  
  daily_tick  <- seq(1, nrow(incidence_daily), 8)
  daily_label <- incidence_daily$dates[daily_tick]
  
  plot_inc_peak <- plot(inc_obj) +
    geom_vline(xintercept = inc_peak,
               col = "red", lty = 2) +
    labs(title = "Covid-19: Dagligt bekräftade inhemska fall",
         subtitle = title.plot,
         x="Datum",
         y="Antal fall") +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x = element_text(face="bold", size=10, vjust=0.25),
          axis.title.y = element_text(face="bold", size=10),
          axis.text.x  = element_text(size=5),
          axis.text.y  = element_text(size=9),
          legend.position="none")
  
  
  
  
  ## ----'plot_loess'-------------------------------------------------------------
  ## loess
  
  day_breaks <- seq(min(as.Date(long_dates)),
                    max(as.Date(long_dates)), 8)
  
  total_incidence <- incidence_daily$Total
  
  size.line <- 0.8
  plot_loess <- ggplot(data=incidence_daily, aes(x=long_dates, y=total_incidence))  +
    geom_bar(stat="identity", color="black", fill="white")+
    geom_smooth(colour="red", size=size.line)  +
    #ylim(0, max(inläggning)*1.1) +
    labs(title="Covid-19: fall per dag (staplar) och utjämningskurva (röd linje).",
         x="Datum",
         y="Antal fall") +
    #scale_x_continuous(breaks=as.Date(day_breaks)) +
    theme(panel.background = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          plot.title = element_text(size=9),
          axis.title.x = element_text(face="bold", size=10, vjust=0.25),
          axis.title.y = element_text(face="bold", size=10),
          axis.text.x  = element_text(size=8),
          axis.text.y  = element_text(size=14),
          
          #legend.position="none",
          panel.border = element_rect(colour = "black", fill=NA, size=0.5))
  
  
  
  ## ----save plot for Figure 1-----------------------------------------------------
  
  
  pdf_file_name <- paste(output.path,"/incidence_peak_",region,".pdf", sep ="")
  
  pdf(pdf_file_name,width=9, height=6 )
  print(plot_inc_peak)
  dev.off() 
  
  pdf_file_name <- paste(output.path,"/incidence_smoothed_",region,".pdf", sep ="")
  
  pdf(pdf_file_name,width=9, height=6 )
  print(plot_loess)
  dev.off() 
  
  
  
  
  
  ## ----'nb_trend'---------------------------------------------------------------
  
  date_limit <- as.Date(yesterday - period_trend)
  
  dat        <- incidence_daily[which(incidence_daily$dates_long >= date_limit), ]
  dat$x      <- as.numeric(1:nrow(dat))
  dat$day_nr <- as.factor(wday(dat$dates_long))
  
  ##############################
  
  
  dat_dummy <- dummy_cols(dat, select_columns="day_nr")
  dat_dummy <- dat_dummy[,-which(colnames(dat_dummy) == "day_nr_7")]
  dat_dummy[which(dat_dummy$day_nr == 7), which(colnames(dat_dummy) %in% paste0("day_nr_", 1:6))] <- -1
  dat_dummy <- dat_dummy[, which(colnames(dat_dummy) %in% c("Total", "x", paste0("day_nr_", 1:6)))]
  
  nb_total_dummy  <- glm.nb(Total ~ x + day_nr_1 + day_nr_2 + day_nr_3 + day_nr_4 + day_nr_5 + day_nr_6, data=dat_dummy)
  summary.total.dummy <- summary(nb_total_dummy)
  pvalue.nb.dummy     <- summary.total.dummy$coeff['x',4]
  pvalue.nb.dummy.txt <- ifelse(pvalue.nb.dummy < 0.01, "<0.01", round(pvalue.nb.dummy,2))
  
  
  
  irr_total_dummy     <- data.frame(IRR=exp(summary.total.dummy$coeff[,'Estimate']), exp(confint(nb_total_dummy)))
  colnames(irr_total_dummy) <- c("IRR", "95% CI lower", "95% CI upper")
  
  
  slope.nb.dummy    <- (irr_total_dummy["x", "IRR"]-1) * 100
  ci.slope.nb.dummy <- (irr_total_dummy["x", c( "95% CI lower", "95% CI upper")] - 1) * 100
  
  ## Dubbleringstid
  doubtime <- round(log(2) / (log(1 + slope.nb.dummy/100)),0) # approx same as round(70/round(slope.nb, 0),1)
  doubtime.ci <- round(log(2) / (log(1 + ci.slope.nb.dummy/100)),0)
  
  
  ################ Contrasts to focus on intercept + trend, sum of effect of days = 0 ###########
  # contrast of coef so that sum(day_nr) = 0
  K <- cbind(rep(1, nrow(dat_dummy)), dat_dummy$x, matrix(0, nrow=nrow(dat_dummy), ncol=6))
  
  # sum((coef of day_nr) = 0, thus intercept + slope
  int_slope_estimate <- glht(nb_total_dummy, linfct = K)
  #summary(int_slope)
  
  dat_day_ci  <- confint(int_slope_estimate, calpha = univariate_calpha())$confint
  
  # fitted with CI
  dat_day_fit_ci <- data.frame(dat,
                               fit_resp=exp(as.data.frame(dat_day_ci)$Estimate),
                               ci_lwr = exp(as.data.frame(dat_day_ci)$lwr),
                               ci_upr = exp(as.data.frame(dat_day_ci)$upr))
  
  
  
  
  ##############################
  ## Dubbleringstid
  #doubtime <- round(log(2) / (log(1 + slope.nb/100)),0) #round(70/round(slope.nb, 0),1)
  
  
  
  if (pvalue.nb.dummy > 0.05){
    show_text_nonsignif_nb <- TRUE
    doubtime <- ""
  } else {
    if (pvalue.nb.dummy <= 0.05 & slope.nb.dummy > 0){
      show_text_signif_increase_nb <- TRUE
    } else {
      show_text_signif_decrease_nb    <- TRUE
    }
  }
  
  # save in external file per region
  if (region == "Blekinge"){
    write.table(data.frame(Region=region, Dubblering_Halveringsstid=doubtime),
                col.names=TRUE, row.names=FALSE,
                file=file.path(output.path, paste0("doubling_time_(", analysis_date, ").csv")),
                append=FALSE, sep=";")
  } else {
    write.table(data.frame(Region=region, Dubblering_Halveringsstid=doubtime),
                col.names=FALSE, row.names=FALSE,
                file=file.path(output.path, paste0("doubling_time_(", analysis_date, ").csv")),
                append=TRUE, sep=";")
  }
  
  
  
  
  ## ----label='print_nb_trend', out.width='0.4\\linewidth', fig.show='hold'------
  
  total_fit <- ggplot(dat_day_fit_ci,
                      aes(x = dates_long, y = Total, ymin=ci_lwr, ymax=ci_upr)) +
    geom_ribbon(fill = "grey76") +
    geom_line(aes(y=Total), colour="black", linetype = "dashed", size=0.5) +
    geom_line(aes(y=fit_resp), colour="red", linetype = "solid", size=0.5) +
    coord_cartesian(ylim = c(0, max(dat_day_fit_ci$ci_upr, dat$Total)*1.1), expand = FALSE, clip = "off") +
    theme_bw() +
    #annotation_custom(years_text)+
    scale_x_date(date_breaks = '2 day', date_labels = '%b%d') +
    labs(x = "Dag", y="Antal fall") +
    theme(plot.margin = unit(c(2, 2, 2, 1), "lines"),
          #axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size=10), #angle = 90,
          legend.position = "none",
          strip.placement = 'outside',
          strip.background.x = element_blank())
  
  
  
  ## ----save plot for Figure 2-----------------------------------------------------
  
  pdf_file_name <- paste(output.path,"/estimated_trend_",region,".pdf", sep ="")
  
  pdf(pdf_file_name,width=9, height=6 )
  print(total_fit)
  dev.off() 
  
  
  
  
  
  
  
}
