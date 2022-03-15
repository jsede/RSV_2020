#' Plot for ICD-J21 presentations for WA, NSW, Vic
#' see plotmaster.R for file paths and libraries
#' 

nsw.icd <- read_excel("Westmead Kids 2015-2020 RSV_Bronchiolitis ICD10+PCR.xlsx", 
                      range="A23:G35") %>% 
  pivot_longer(cols=c(2:7),names_to = "year", values_to = "admissions") %>% 
  rename(month=`...1`) %>% 
  mutate(month = match(month,month.abb),
         yearmonth=as.Date(paste(year,month,"01", sep="-")))
nsw.icd.mean <- nsw.icd %>% 
  filter((year %in% c(2018,2019)) | (year==2020 & month <=3) | year==2017 & month>3) %>% 
  group_by(month) %>% 
  summarise(adm=mean(admissions),
            min=min(admissions),
            max=max(admissions)) %>% 
  mutate(type="Mean 2017-2019")
nsw.icd.2020 <- nsw.icd %>% 
  filter(year==2021 | (year==2020 & month >3)) %>% 
  select(month, adm=admissions) %>% 
  mutate(type="2020/21")
nsw.icd.data <- bind_rows(nsw.icd.mean,nsw.icd.2020) %>% 
  mutate(xmonth=ifelse(month>3, month-3, month+12-3))%>% 
  arrange(type) 

nsw.icd.plot <-
  nsw.icd.data %>% 
  ggplot()+
  geom_line(aes(x=xmonth, y=adm, lty=type), col="navy", lwd=0.5)+
  geom_ribbon(data=subset(nsw.icd.data, type=="Mean 2017-2019"), 
              aes(x=xmonth, ymin=min, ymax=max, fill="2017-2019 range"), alpha=0.1) +
  scale_fill_manual("", values=c("blue"))+
  theme(panel.background=element_blank()) +
  theme(legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        # legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", fill="",
       y="Bronchiolitis hospitalisations")+
  scale_x_continuous(limits=c(1,12),
                     breaks=seq(1,12,1),
                     labels=c(month.abb[4:12],month.abb[1:3]),
                     # expand=c(0,0)
                     )+
  scale_y_continuous(limits=c(0,150))
nsw.icd.plot
# ggsave("nsw-icd-plot.pdf")

nsw.icd.plot.long <-
ggplot(nsw.icd)+
  geom_line(aes(x=yearmonth, y=admissions, col="Monthly admissions"),  lwd=0.5)+
  scale_color_hue(h=c(0,360))+
  scale_x_date(breaks = "year",
               limits=c(as.Date("2017-01-01"),as.Date("2021-04-30")),
               date_labels = "%Y")+
  scale_y_continuous(limits=c(0,150))+
  scale_color_manual(values="blue")+
  theme(panel.background=element_blank(),
        legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", col="",
       y="Bronchiolitis hospitalisations")
nsw.icd.plot.long



# victoria - RCH =========
vic.icd <- read_excel("RCH-RSV_Bronchiolitis-ICD-Jan-March2021.xlsx", 
                      range="A20:H32") %>% 
  pivot_longer(cols=c(2:8),names_to = "year", values_to = "admissions") %>% 
  rename(month=`...1`) %>% 
  mutate(month = match(month,month.abb),
         yearmonth=as.Date(paste(year,month,"01", sep="-")))

vic.icd.mean <- vic.icd %>% 
  filter((year %in% c(2018,2019)) | (year==2020 & month <=3) | year==2017 & month>3) %>% 
  group_by(month) %>% 
  summarise(adm=mean(admissions),
            min=min(admissions),
            max=max(admissions)) %>% 
  mutate(type="Mean 2017-2019")
vic.icd.2020 <- vic.icd %>% 
  filter((year==2021 & month<=3)| (year==2020 & month >3)) %>% 
  select(year, month, adm=admissions) %>% 
  mutate(type="2020/21")
vic.icd.data <- bind_rows(vic.icd.mean,vic.icd.2020) %>% 
  mutate(xmonth=ifelse(month>3, month-3, month+12-3))%>% 
  arrange(type) 

vic.icd.plot <-
  vic.icd.data %>% 
  ggplot()+
  geom_line(aes(x=xmonth, y=adm, lty=type), col="navy" ,lwd=0.5)+
  geom_ribbon(data=subset(vic.icd.data, type=="Mean 2017-2019"), 
              aes(x=xmonth, ymin=min, ymax=max, fill="2017-2019 range"), alpha=0.1) +
  scale_fill_manual("", values=c("blue"))+
  theme(panel.background=element_blank()) +
  theme(legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        # legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", fill="",
       y="Bronchiolitis hospitalisations")+
  scale_x_continuous(limits=c(1,12),
                     breaks=seq(1,12,1),
                     labels=c(month.abb[4:12],month.abb[1:3]),
                     # expand=c(0,0)
                     )+
  scale_y_continuous(limits=c(0,150))
vic.icd.plot

vic.icd.plot.long <-
  ggplot(vic.icd)+
  geom_line(aes(x=yearmonth, y=admissions, col="Monthly admissions"),  lwd=0.5)+
  scale_color_hue(h=c(0,360))+
  scale_color_manual(values="blue")+
  scale_x_date(date_breaks = "year",
               date_minor_breaks = "month",
               limits=c(as.Date("2017-01-01"),as.Date("2021-04-30")),
               date_labels = "%Y")+
  scale_y_continuous(limits=c(0,150))+
  theme(panel.background=element_blank(),
        legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", col="",
       y="Bronchiolitis hospitalisations")
vic.icd.plot.long

# =======================================================
# WA hospital data
wa.icd <- read.csv("wa-icd.csv") %>% 
  pivot_longer(cols=c(2:6),names_to = "year", values_to = "admissions") %>% 
  rename(month=Month) %>% 
  mutate(month = match(month,month.abb),
         year = as.numeric(gsub("X","",year)),
         yearmonth=as.Date(paste(year,month,"01", sep="-")))

wa.icd.mean <- wa.icd %>% 
  filter((year %in% c(2018,2019)) | (year==2020 & month <=3) | year==2017 & month>3) %>% 
  group_by(month) %>% 
  summarise(adm=mean(admissions),
            min=min(admissions),
            max=max(admissions)) %>% 
  mutate(type="Mean 2017-2019")
wa.icd.2020 <- wa.icd %>% 
  filter((year==2021 & month<=3)| (year==2020 & month >3)) %>% 
  select(year, month, adm=admissions) %>% 
  mutate(type="2020/21")
wa.icd.data <- bind_rows(wa.icd.mean,wa.icd.2020) %>% 
  mutate(xmonth=ifelse(month>3, month-3, month+12-3))%>% 
  arrange(type) 

wa.icd.plot <-
  wa.icd.data %>% 
  ggplot()+
  geom_line(aes(x=xmonth, y=adm, lty=type), col="navy" ,lwd=0.5)+
  geom_ribbon(data=subset(wa.icd.data, type=="Mean 2017-2019"), 
              aes(x=xmonth, ymin=min, ymax=max, fill="2017-2019 range"), alpha=0.1) +
  scale_fill_manual("", values=c("blue"))+
  theme(panel.background=element_blank()) +
  theme(legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        # legend.title = element_blank(),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", fill="",
       y="Bronchiolitis hospitalisations")+
  scale_x_continuous(limits=c(1,12),
                     breaks=seq(1,12,1),
                     labels=c(paste(month.abb[4:12],"\n2020"),paste(month.abb[1:3], "\n2021")),
                     # expand=c(0,0)
  )+
  scale_y_continuous(limits=c(0,150))
wa.icd.plot

wa.icd.plot.long <-
  ggplot(wa.icd)+
  geom_line(aes(x=yearmonth, y=admissions, col="Monthly admissions"),  lwd=0.5)+
  scale_color_manual(values="blue")+
  scale_x_date(date_breaks = "year",
               date_minor_breaks = "month",
               limits=c(as.Date("2017-01-01"),as.Date("2021-04-30")),
               date_labels = "%Y")+
  scale_y_continuous(limits=c(0,150))+
  theme(panel.background=element_blank(),
        legend.position="right", 
        legend.background =element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.text = element_text(size = 8)) +
  labs(x = "", lty="", col="",
       y="Bronchiolitis hospitalisations")
wa.icd.plot.long


ggarrange(nsw.icd.plot.long, 
          wa.icd.plot.long, 
          vic.icd.plot.long,
          ncol=1, labels=c("NSW","WA","Vic"))
