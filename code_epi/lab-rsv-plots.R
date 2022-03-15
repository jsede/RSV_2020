#' Plots of RSV RSV % positive for WA, NSW, Vic
#' see plotmaster.R for file paths and libraries
#' 
# ###WA DATA ===================
# PathWest data -----------------------------------------------
# prior years' data
wa.lab.2017 <- read_excel("Resp viruses 2017-2020 PW plus COVID_.xlsx", skip=5) %>%
  filter(!`Row Labels...1`=="Grand Total") %>% 
  mutate(weekend=as.Date(as.numeric(`Row Labels...1`), origin="1899-12-30"),
         rsv=`Sum of RSV`,
         ppos=`Sum of RSV`/`Count of RSV`*100,
         total=`Count of RSV`,
         week=epiweek(weekend),
         year=epiyear(weekend)) %>% 
  select(week, year, ppos,weekend, total) %>% 
  filter(!(weekend>"2020-01-05" ))
wa.lab.2020 <- read_excel("Copy of PathWest_resp viruses 2020 weekly.xlsx", sheet = "Sheet4") %>% 
  mutate(week=epiweek(`Week ending`),
         year=epiyear(`Week ending`),
         rsv=`RSV positive`,
         total=`Number tested by resp screen`,
         ppos=rsv/total*100) %>% 
  select(weekend=`Week ending`, week, year, ppos, total)

wa.lab <- bind_rows(wa.lab.2017,wa.lab.2020) %>% 
  as.data.frame() %>% 
  arrange(year, week) %>% 
  mutate(ppos=ifelse(is.na(ppos),0,ppos),
         mav.ppos=mav(ppos))
wa.lab.mean <- wa.lab %>% 
  filter(year %in% c(2018,2019) | (year==2020 & week<startweek) | (year==2017 & week>=startweek)) %>% 
  group_by(week) %>% 
  summarise(mav.mean= mean(mav.ppos),
            mav.min = min(mav.ppos, na.rm=T),
            mav.max = max(mav.ppos, na.rm=T)) %>% 
  mutate(type="Mean 2017-2019",
         mav.ppos=mav.mean)
# 2020-2021
wa.lab.new <- wa.lab %>% 
  filter((year==2020 & week>=startweek) | year==2021) %>% 
  mutate(type="2020/21")
wa.lab.data <- bind_rows(wa.lab.mean, wa.lab.new) %>% 
  filter(!week==53) %>% 
  mutate(xorder=ifelse(week>=startweek, week-startweek, week+52-startweek),
         total=ifelse(type=="Mean 2017-2019",NA,total))

# labels -----
weeklbls <- format(wa.lab.data$weekend, "%d %b \n%Y")
weeklbls <- weeklbls[seq(1, length(weeklbls), 4)]
weeklbls <- weeklbls[!is.na(weeklbls)]
weeklbls
weeklblsnoyr <- format(wa.lab.data$weekend, "%d %b")
weeklblsnoyr <- weeklblsnoyr[seq(1, length(weeklblsnoyr), 4)]
weeklblsnoyr <- weeklblsnoyr[!is.na(weeklblsnoyr)]
weeklblsnoyr

wa.lab.plot <-
  ggplot(wa.lab.data)+
  geom_bar(aes(x=xorder,y=total/25, fill="Number of tests"), alpha=0.2, stat="identity")+
  geom_line(aes(x=xorder, y=mav.ppos, lty=type), col="darkred", lwd=0.5)+
  geom_ribbon(data=subset(wa.lab.data,!is.na(mav.min)),
              aes(x=xorder, ymin=mav.min, ymax=mav.max, fill="2017-2019 range"), alpha=0.2)+
  # scale_color_viridis_d("", option="A", begin = 0.8, end = 0.2)+
  scale_fill_manual("", values=c("pink","grey"))+
  theme(panel.background=element_blank()) +
  theme(legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        # legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", fill="")+
  scale_x_continuous(limits=c(1,52),
                     minor_breaks = seq(1,52,1),
                     breaks=seq(1,52,4),
                     # expand=c(0,0),
                     labels=weeklbls
                     # labels=c(seq(17,52,4),seq(1,14,4))
  )+
  scale_y_continuous(
    # Left axis
    name = "RSV % positive",
    limits=c(0,50),breaks=seq(0,50,10),
    # Right axis
    sec.axis = sec_axis( trans=~.*25, name="Total tests")
  ) 
wa.lab.plot

wa.lab.plot.long <-
  ggplot(wa.lab)+
  geom_bar(aes(x=as.Date(weekend),y=total/45, fill="Weekly tests"), alpha=0.2, stat="identity")+
  geom_line(aes(x=as.Date(weekend), y=mav.ppos, col="Weekly percent positive"),  lwd=0.5)+
  scale_color_hue(l=20)+
  theme(panel.background=element_blank(),
        legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", fill="", col="")+
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2021-04-01")))+
  scale_y_continuous(
    # Left axis
    name = "RSV % positive",
    limits=c(0,40),breaks=seq(0,50,10),
    # Right axis
    sec.axis = sec_axis( trans=~.*45, name="Total tests")
  ) 
wa.lab.plot.long


# LAB ========================================================
flunetfiles <- list.files(pattern="FluNet_", full.names = T)
nsw.lab.2017 <- lapply(flunetfiles, read_excel, col_types="numeric") %>% 
  bind_rows() %>% 
  select(rsv=RSV, total=SPECIMENPROCESSED, weekend=START_DATE) %>% 
  mutate(ppos=rsv/total*100,
         weekend=as.Date(weekend, origin="1899-12-30"),
         week=epiweek(weekend),
         year=epiyear(weekend)
         ) %>% 
  distinct(weekend, .keep_all=T)
max(nsw.lab.2017$weekend, na.rm=TRUE)
table(nsw.lab.2017$year)
nsw.lab.2020 <- read_excel("Australia _ICPMR week beginning 05 Apr 2021_.xlsx")  %>% 
  select(total=SPECIMENPROCESSED,rsv=RSV, weekend) %>% 
  mutate(ppos=rsv/total*100,
         week=epiweek(weekend),
         year=epiyear(weekend)) %>% 
  filter(!(year==2021 & week>=startweek))
table(nsw.lab.2020$year)
nsw.lab <- bind_rows(nsw.lab.2017,nsw.lab.2020) %>% 
  mutate(ppos=ifelse(is.na(ppos),0,ppos)) %>% 
  as.data.frame() %>% 
  # filter(!(week==1 & year==2020) & !(week==1 & year==2019)) %>% 
  arrange(year, week) %>% 
  mutate(mav.ppos=mav(ppos)) %>% 
  arrange(weekend) %>% 
  mutate(diff=weekend-lag(weekend),
         otherweek=epiweek(weekend))
nsw.lab.mean <- nsw.lab %>% 
  filter(year %in% c(2018,2019) | (year==2020 & week<startweek) | (year==2017 & week>=startweek)) %>% 
  group_by(week) %>% 
  summarise(mav.mean= mean(mav.ppos),
            mav.min = min(mav.ppos, na.rm=T),
            mav.max = max(mav.ppos, na.rm=T)) %>% 
  # summarise(mean.rsv=mean(rsv, na.rm=T),
  #           min.rsv = min(rsv, na.rm=T),
  #           max.rsv = max(rsv, na.rm=T),
  #           ) %>% 
  # mutate(mav.rsv=mav(mean.rsv),
  #        mav.ppos=mav(mean.rsv),
  #        mav.min.rsv=mav(min.rsv),
  #        mav.max.rsv=mav(max.rsv)) %>% 
  mutate(type="Mean 2017-2019",
         mav.ppos=mav.mean)
# 2020-2021
nsw.lab.2021 <- nsw.lab %>% 
  filter((year==2020 & week>=startweek) | year==2021) %>% 
  mutate(type="2020/21")
nsw.lab.data <- bind_rows(nsw.lab.mean, nsw.lab.2021) %>% 
  filter(!week==53) %>% 
  mutate(xorder=ifelse(week>=startweek, week-startweek, week+52-startweek))
  
# ICPMR plots --------------------------------------------------
max(nsw.lab.data$mav.ppos, na.rm=T)

# RSV % positive ---------------------------------------------
nsw.lab.plot <-
ggplot(nsw.lab.data)+
  geom_bar(aes(x=xorder,y=total/150, fill="Number of tests"), alpha=0.2, stat="identity")+
  geom_line(aes(x=xorder, y=mav.ppos, lty=type), col="darkred", lwd=0.5)+
  geom_ribbon(data=subset(nsw.lab.data,!is.na(mav.min)),
              aes(x=xorder, ymin=mav.min, ymax=mav.max, fill="2017-2019 range"), alpha=0.2)+
  # scale_color_viridis_d("", option="A", begin = 0.8, end = 0.2)+
  scale_fill_manual("", values=c("pink","grey"))+
  theme(panel.background=element_blank(),
        legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        # legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", fill="")+
  scale_x_continuous(limits=c(1,52),
                     minor_breaks = seq(1,52,1),
                     breaks=seq(1,52,4),
                     # expand=c(0,0),
                     labels=weeklblsnoyr
                     # labels=c(seq(17,52,4),seq(1,14,4))
  )+
  scale_y_continuous(
    # Left axis
    name = "RSV % positive",
    limits=c(0,30),breaks=seq(0,30,10),
    # Right axis
    sec.axis = sec_axis( trans=~.*150, name="Total tests")
  ) 
nsw.lab.plot

nsw.lab.plot.long <-
  ggplot(nsw.lab)+
  geom_bar(aes(x=as.Date(weekend),y=total/150, fill="Weekly tests"), alpha=0.2, stat="identity")+
  geom_line(aes(x=as.Date(weekend), y=mav.ppos, col="Weekly percent positive"),  lwd=0.5)+
  scale_color_hue(l=20)+
  theme(panel.background=element_blank()) +
  theme(legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        # legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", fill="", col="")+
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2021-04-01")))+
  scale_y_continuous(
    # Left axis
    name = "RSV % positive",
    limits=c(0,30),breaks=seq(0,50,10),
    # Right axis
    sec.axis = sec_axis( trans=~.*150, name="Total tests")
  ) 
nsw.lab.plot.long

# ###VIC DATA ===================
# RCH data -----------------------------------------------
# prior years' data
vic.path <- "rv 2010-2021.xlsx"
dat <- vic.path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, col_types = c("text","text","date","text","text","date","text","text","text"), path = vic.path)
vic.lab <- do.call("bind_rows",dat) %>% 
  filter(!RSVNAD %in% c("Unassessable","Not performed.","Not tested",
                        "Not tested.","NOT TESTED","not tested","Result pending","n/a")) %>% 
  mutate(week=epiweek(CollDate),
         year=epiyear(CollDate),
         result=grepl("detected rsv|detected - a|detected - b|rsva detected",RSVNAD,ignore.case = T),
         result=ifelse(RSVNAD %in% c("DETECTED","Detected","DETECTED by polymerase chain reaction."),T,result)) %>% 
  group_by(week,year) %>% 
  summarise(rsv=sum(result==T),
            total=n(),
            weekend=min(CollDate)) %>% 
  mutate(ppos=rsv/total*100,
         ppos=ifelse(is.na(ppos),0,ppos)) %>% 
  as.data.frame() %>% 
  arrange(year,week) %>% 
  mutate(mav.ppos=mav(ppos))
vic.lab
vic.lab.mean <- vic.lab %>% 
  filter(year %in% c(2018,2019) | (year==2020 & week<startweek) | (year==2017 & week>=startweek)) %>% 
  group_by(week) %>% 
  summarise(mav.mean= mean(mav.ppos),
            mav.min = min(mav.ppos, na.rm=T),
            mav.max = max(mav.ppos, na.rm=T)) %>% 
  mutate(type="Mean 2017-2019",
         mav.ppos=mav.mean)
# 2020-2021
vic.lab.2021 <- vic.lab %>% 
  filter((year==2020 & week>=startweek) | (year==2021 & week<startweek)) %>% 
  mutate(type="2020/21")
vic.lab.data <- bind_rows(vic.lab.mean, vic.lab.2021) %>% 
  filter(!week==53) %>% 
  mutate(xorder=ifelse(week>=startweek, week-startweek, week+52-startweek))

vic.lab.plot <-
  ggplot(vic.lab.data)+
  geom_bar(aes(x=xorder,y=total/4, fill="Number of tests"), alpha=0.2, stat="identity")+
  geom_line(aes(x=xorder, y=mav.ppos, lty=type), col="darkred", lwd=0.5)+
  geom_ribbon(data=subset(vic.lab.data,!is.na(mav.min)),
              aes(x=xorder, ymin=mav.min, ymax=mav.max, fill="2017-2019 range"), alpha=0.2)+
  # scale_color_viridis_d("", option="A", begin = 0.8, end = 0.2)+
  scale_fill_manual("", values=c("pink","grey"))+
  theme(panel.background=element_blank()) +
  theme(legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 8),
        ) +
  labs(x = "", fill="", lty="", col="")+
  scale_x_continuous(limits=c(1,52),
                     minor_breaks = seq(1,52,1),
                     breaks=seq(1,52,4),
                     # expand=c(0,0),
                     labels=weeklblsnoyr
                     # labels=c(seq(17,52,4),seq(1,14,4))
                     )+
  scale_y_continuous(
    # Left axis
    name = "RSV % positive",
    limits=c(0,50),breaks=seq(0,50,10),
    # Right axis
    sec.axis = sec_axis( trans=~.*4, name="Total tests")
  ) 
vic.lab.plot

vic.lab.plot.long <-
ggplot(vic.lab)+
  geom_bar(aes(x=as.Date(weekend),y=total/5, fill="Weekly tests"), alpha=0.2, stat="identity")+
  geom_line(aes(x=as.Date(weekend), y=mav.ppos, col="Weekly percent positive"),  lwd=0.5)+
  scale_color_hue(l=20)+
  theme(panel.background=element_blank(),
        legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", fill="", col="")+
  scale_x_date(limits = c(as.Date("2017-01-01"), as.Date("2021-04-01")))+
  scale_y_continuous(
    # Left axis
    name = "RSV % positive",
    limits=c(0,50),breaks=seq(0,50,10),
    # Right axis
    sec.axis = sec_axis( trans=~.*5, name="Total tests")
  ) 
vic.lab.plot.long


ggarrange(nsw.lab.plot, vic.lab.plot, wa.lab.plot,
          align = "v", nrow = 3, 
          labels=c("NSW","Vic","WA"),
          common.legend = T,
          font.label = list(size = 10, color = "black", face = "bold", family = NULL, hjust=0))
# ggsave("rsv-nsw-vic.pdf", width=12, height=9)
# ggsave("rsv-nsw-vic.png", width=12, height=9)


ggarrange(nsw.lab.plot.long, vic.lab.plot.long, wa.lab.plot.long,
          align = "v", nrow = 3, 
          labels=c("NSW","Vic","WA"), vjust = 1,hjust=0,
          common.legend = T,
          font.label = list(size = 10, color = "black", face = "bold", family = NULL, hjust=0))
ggsave("rsv-nsw-vic-lab-long.pdf", width=12, height=9)
