
####----
#projekt
library(data.table)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(readr)
library(plotly)
library(RColorBrewer)
library(grDevices)


####READING DATA!-----


#reservations
res_view_old <- fread("/Volumes/SD disk/Data_mining/Reservations_view.csv")[,c(2,3,4,6,7,8,9,14,16,28,31,34,35,36,41)]
res_view <- fread("/Volumes/SD disk/Data_mining/Res_new.csv")
res_view <- res_view[,c(2,3,4,6,7,8,9,18,20,32,35,38,39,40,45)]

reservations <- merge(res_view_old,res_view,all=TRUE)
reservations <- reservations[!duplicated(reservations$rate_quote_uuid),]
write.csv(reservations,"reservations.csv")


#rate quotes filtered & dup
#dup - no duplicates by request uuid
#filtered - no duplicates and removed very similar requests (gone through phase 2)

rateq1_filtered <- fread("/Volumes/SD disk/Data_mining/filtered_rate_quote_1.csv")
rateq2_filtered <- fread("/Volumes/SD disk/Data_mining/filtered_rate_quote_2.csv")
rateq3_filtered <- fread("/Volumes/SD disk/Data_mining/filtered_rate_quote_3.csv")
rateq4_filtered <- fread("/Volumes/SD disk/Data_mining/filtered_rate_quote_4.csv")
rateq_filtered <- merge(merge(merge(rateq1_filtered,rateq2_filtered,all=TRUE),rateq3_filtered,all=TRUE),rateq4_filtered,all=TRUE)
remove(rateq1_filtered,rateq2_filtered,rateq3_filtered,rateq4_filtered)

rateq1_dup <- fread("/Volumes/SD disk/Data_mining/rate_quote_1_dup1.csv")
rateq2_dup <- fread("/Volumes/SD disk/Data_mining/rate_quote_2_dup1.csv")
rateq3_dup <- fread("/Volumes/SD disk/Data_mining/rate_quote_3_dup1.csv")
rateq4_dup <- fread("/Volumes/SD disk/Data_mining/rate_quote_4_dup1.csv")
rateq_dup <- merge(merge(merge(rateq1_dup,rateq2_dup,all=TRUE),rateq3_dup,all=TRUE),rateq4_dup,all=TRUE)
remove(rateq1_dup,rateq2_dup,rateq3_dup,rateq4_dup)

write.csv(rateq_dup,"rateq_dup.csv")
write.csv(rateq_filtered,"rateq_filtered.csv")

#overview of how many days we have in the data
tabel_days <- rateq_dup %>% group_by(as.Date(timestamp)) %>% summarise(min=min(timestamp),max=max(timestamp))
tabel_days


#removing date 2017-12-07
rateq_dup <- rateq_dup[!(as.Date(timestamp)=="2017-12-07"),]
rateq_filtered <- rateq_filtered[!(as.Date(timestamp)=="2017-12-07"),]

one <- rateq_dup[,2]
one$datatype <- "Duplicated by uuid"
two <- rateq_filtered[,3]
two$datatype <- "Filtered"
together<- merge(one,two,all=TRUE)


#REQUESTS-----------------

#if log scale needed
#together <-together %>% group_by(datatype,as.factor(as.Date(timestamp))) %>% summarise(count=n())
#ggplot(together, aes(x=`as.factor(as.Date(timestamp))`,y=count,group=datatype,colour=datatype))+geom_line()

p_distr_notchanging_plot <-ggplot(together,aes(x=as.factor(as.Date(timestamp)),group=datatype,colour=datatype))+labs(xlab="",title="Requests per day")+theme_bw()+geom_line(size=1.25,stat="count")+theme(legend.text = element_text(size=12),legend.title = element_blank(),axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "bottom",plot.title=element_text(hjust=0.5),axis.title.x=element_blank(),panel.grid.minor = element_blank())
p_distr_notchanging_plot


#----days in advance----
modif_rate <- rateq_dup
modif_rate$timestamp <- as.Date(modif_rate$timestamp)
modif_rate$pickup_timestamp <- as.Date(modif_rate$pickup_timestamp)
modif_rate$return_timestamp <- as.Date(modif_rate$return_timestamp)
modif_rate$days_in_advance <- modif_rate$pickup_timestamp - modif_rate$timestamp

p_days_in_advance_dist <-ggplot(modif_rate,aes(x=days_in_advance,..density..))+geom_histogram(binwidth = 10,colour="black",fill=rgb(0.1,0.67,0.674))+theme_bw()+
  labs(x="Days in advance",title="Price checking")+scale_x_continuous(breaks=c(seq(0,1000,50)))+
  theme(panel.grid.minor=element_blank(),legend.position = "none",plot.title=element_text(hjust=0.5))
p_days_in_advance_dist

#getting season in case needed
yq <- as.yearqtr(as.yearmon(modif_rate$timestamp, "%m/%d/%Y") + 1/12)
modif_rate$Season <- factor(format(yq, "%q"), levels = 1:4, labels = c("winter", "spring", "summer", "fall"))

p_by_season_facets <-ggplot(modif_rate,aes(x=days_in_advance,..density..))+geom_histogram(binwidth = 10,colour="black",fill=rgb(0.1,0.67,0.674))+facet_wrap(~Season,nrow=2)+theme_bw()+labs(x="Days in advance",title="Price checking by season")+scale_x_continuous(breaks=c(seq(0,1000,50)))+theme(panel.grid.minor=element_blank(),legend.position = "none",plot.title=element_text(hjust=0.5))
p_by_season_facets


#_----
#click on legend to remove a broker
#dobule click on legend to add a broker
#compare data !
bybroker_data_req <-rateq_filtered %>% filter(Broker_name != "RateChain test")%>%group_by(Broker_name,as.factor(as.Date(timestamp))) %>% summarise(count=n())
bybroker_data_req$`as.factor(as.Date(timestamp))` <- format(as.Date(bybroker_data_req$`as.factor(as.Date(timestamp))`,"%Y-%m-%d"),format="%d. %b")
picked_colours <- brewer.pal(length(unique(bybroker_data_req$Broker_name)),"Spectral")
p_bybroker <- bybroker_data_req %>% plot_ly(x=~`as.factor(as.Date(timestamp))`,y=~count,type="bar",color=~Broker_name,colors=picked_colours) %>%
  layout(barmode="stack",title="Rate quotes by brokers",xaxis=list(title="",tickangle=0),legend=list(y=0.5))
p_bybroker


#hover shows count not log10(count) --- not goodie
p_bybroker_logscale <- bybroker_data_req %>% plot_ly(x=~`as.factor(as.Date(timestamp))`,y=~log10(count),type="bar",color=~Broker_name,colors=picked_colours,hoverinfo="text",text=~paste0(Broker_name,":\t",count)) %>%
  layout(barmode="stack",title="Requests by brokers",xaxis=list(title="",tickangle=0),legend=list(y=0.5))
p_bybroker_logscale

bybroker_data_res <- res_view %>% group_by(`Broker name`,as.factor(as.Date(rate_request_timestamp))) %>% summarise(count=n())
colnames(bybroker_data_res) <- c("Broker name","date","count")
bybroker_data_res$date <- format(as.Date(bybroker_data_res$date,"%Y-%m-%d"),format="%d. %b")
picked_colours <- brewer.pal(length(unique(bybroker_data_res$`Broker name`)),"Spectral")
p_bybroker_res <- bybroker_data_res %>% plot_ly(x=~date,y=~count,type="bar",color=~`Broker name`,colors=picked_colours) %>%
  layout(barmode="stack",title="Reservations by brokers",xaxis=list(title="",tickangle=0,type="date"),legend=list(y=0.5))
p_bybroker_res


#HOTDAYS-----------------
#Rate quotes
#by day
hotdays_rq_nodupl_brokers_byday <- fread("/Volumes/SD disk/Data_mining/hotdays_rq_nodupl.csv")
hotdays_rq_nodupl_brokers_byday <- hotdays_rq_nodupl_brokers_byday[,-6]#removing ratechain test
hotdays_rq_nodupl_brokers_byday$TravelJigsaw <- as.numeric(hotdays_rq_nodupl_brokers_byday$TravelJigsaw)
hotdays_rq_nodupl_brokers_byday <- melt(hotdays_rq_nodupl_brokers_byday)
hotdays_rq_nodupl_brokers_byday[is.na(hotdays_rq_nodupl_brokers_byday)] <- 0

picked_colours <- brewer.pal(length(unique(hotdays_rq_nodupl_brokers_byday$variable)),"Spectral")
p_hotdays_rq_brokers_byday <- hotdays_rq_nodupl_brokers_byday %>% plot_ly(x=~timestamp,y=~value,color=~variable,colors=picked_colours) %>% 
  add_lines() %>% layout(title="Hot days by brokers",xaxis=list(title="",type="date"),legend=list(y=0.5),yaxis=list(title="count of requests"))
p_hotdays_rq_brokers_byday

#by week
hotdays_rq_nodupl_brokers_byweek <- fread("/Volumes/SD disk/Data_mining/hotdays_rq_nodupl_week.csv")
hotdays_rq_nodupl_brokers_byweek <- hotdays_rq_nodupl_brokers_byweek[,-6]#removing ratechain test
hotdays_rq_nodupl_brokers_byweek$TravelJigsaw <- as.numeric(hotdays_rq_nodupl_brokers_byweek$TravelJigsaw)
hotdays_rq_nodupl_brokers_byweek <- melt(hotdays_rq_nodupl_brokers_byweek)
hotdays_rq_nodupl_brokers_byweek[is.na(hotdays_rq_nodupl_brokers_byweek)] <- 0

picked_colours <- brewer.pal(length(unique(hotdays_rq_nodupl_brokers_byweek$variable)),"Spectral")
p_hotdays_rq_brokers_byweek <- hotdays_rq_nodupl_brokers_byweek %>% plot_ly(x=~timestamp,y=~value,color=~variable,colors=picked_colours) %>% 
  add_lines() %>% layout(title="Hot weeks by brokers",xaxis=list(title="",type="date"),legend=list(y=0.5),yaxis=list(title="count of requests"))
p_hotdays_rq_brokers_byweek

#RESERVATIONS ----------
hotdays_reserv_brokers_byday <- fread("/Volumes/SD disk/Data_mining/hotdays_reserv_broker.csv")
hotdays_reserv_brokers_byday <- hotdays_reserv_brokers_byday[,-6]#removing ratechain test
hotdays_reserv_brokers_byday <- melt(hotdays_reserv_brokers_byday)
hotdays_reserv_brokers_byday[is.na(hotdays_reserv_brokers_byday)] <- 0

picked_colours <- brewer.pal(length(unique(hotdays_reserv_brokers_byday$variable)),"Spectral")
p_hotdays_reserv_brokers_byday <- hotdays_reserv_brokers_byday %>% plot_ly(x=~timestamp,y=~value,color=~variable,colors=picked_colours) %>% 
  add_lines() %>% layout(title="Hot days by brokers",xaxis=list(title="",type="date"),legend=list(y=0.5),yaxis=list(title="count of reservations"))
p_hotdays_reserv_brokers_byday


hotdays_reserv_acriss_byday <-fread("/Volumes/SD disk/Data_mining/hotdays_reserv_acriss.csv")
hotdays_reserv_acriss_byday <- melt(hotdays_reserv_acriss_byday)
hotdays_reserv_acriss_byday[is.na(hotdays_reserv_acriss_byday)] <- 0

picked_colours <- brewer.pal(length(unique(hotdays_reserv_acriss_byday$variable)),"Spectral")
p_hotdays_reserv_acriss_byday <- hotdays_reserv_acriss_byday %>% plot_ly(x=~timestamp,y=~value,color=~variable,colors=picked_colours,type="bar") %>% 
  layout(barmode="stack",title="Hot days by car type",xaxis=list(title="",type="date"),yaxis=list(title="count of reservations"))
p_hotdays_reserv_acriss_byday
#htmlwidgets::saveWidget(p_hotdays_reserv_acriss_byday,"hotdaysbycartype.html")

hotdays_reserv_statustype_byday <- fread("/Volumes/SD disk/Data_mining/hotdays_reserv_statustype.csv")
hotdays_reserv_statustype_byday <- melt(hotdays_reserv_statustype_byday)
hotdays_reserv_statustype_byday[is.na(hotdays_reserv_statustype_byday)] <- 0

picked_colours <- brewer.pal(length(unique(hotdays_reserv_statustype_byday$variable)),"Spectral")
p_hotdays_reserv_statustype_byday <- hotdays_reserv_statustype_byday %>% plot_ly(x=~timestamp,y=~value,color=~variable,colors=picked_colours,type="bar") %>% 
  layout(barmode="stack",title="Hot days by status type",xaxis=list(title="",type="date"),yaxis=list(title="count of reservations"))
p_hotdays_reserv_statustype_byday
#seems pointless


##days in advance
modif_res <- reservations
modif_res$rate_request_timestamp <- as.Date(modif_res$rate_request_timestamp)
modif_res$pickup_timestamp <- as.Date(modif_res$pickup_timestamp)
modif_res$return_timestamp <- as.Date(modif_res$return_timestamp)
modif_res$days_in_advance <- modif_res$pickup_timestamp - modif_res$rate_request_timestamp

p_days_in_advance_dist_reserv <-ggplot(modif_res,aes(x=days_in_advance,..density..))+geom_histogram(binwidth = 10,colour="black",fill=rgb(0.1,0.67,0.674))+theme_bw()+
  labs(x="Days in advance",title="Reservations made")+scale_x_continuous(breaks=c(seq(0,1000,50)))+
  theme(panel.grid.minor=element_blank(),legend.position = "none",plot.title=element_text(hjust=0.5))
p_days_in_advance_dist_reserv

#getting season in case needed
yq <- as.yearqtr(as.yearmon(modif_res$rate_request_timestamp, "%m/%d/%Y") + 1/12)
modif_res$Season <- factor(format(yq, "%q"), levels = 1:4, labels = c("WINTER", "SPRING", "SUMMER", "FALL"))

p_by_season_facets_reserv <-ggplot(modif_res,aes(x=days_in_advance,..density..))+geom_histogram(binwidth = 10,colour="black",fill=rgb(0.1,0.67,0.674))+facet_wrap(~Season,nrow=2)+theme_bw()+labs(x="Days in advance",title="Reservations made by season")+scale_x_continuous(breaks=c(seq(0,1000,50)))+theme(panel.grid.minor=element_blank(),legend.position = "none",plot.title=element_text(hjust=0.5),panel.grid.major.x=element_blank())
p_by_season_facets_reserv




#season and status type

p_by_season_status_type_facets_reserv <-ggplot(modif_res,aes(x=days_in_advance,..density..,group=reservation_status_type,fill=reservation_status_type))+geom_histogram(binwidth = 10,position = "stack",colour="black")+scale_fill_manual(values=brewer.pal(5,"Spectral"))+
  facet_wrap(~Season,nrow=2)+theme_bw()+labs(x="Days in advance",title="Reservations made by season",fill="")+scale_x_continuous(breaks=c(seq(0,1000,50)))+
  theme(panel.grid.minor=element_blank(),legend.position = "bottom",plot.title=element_text(hjust=0.5),panel.grid.major.x=element_blank())
p_by_season_status_type_facets_reserv









