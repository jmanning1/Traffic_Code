# create stat19_date

#### Stats19

prettyformat <- format_format(big.mark = ",", scientific = FALSE)

stat19$Date = dmy(stat19$Date)

MIDAS_Sites = unique(halo_spatial[c("X", "Y")])

MIDAS_Sites_Sp = st_as_sf(MIDAS_Sites, coords = c("X", "Y"), crs = 27700)

buff = st_buffer(MIDAS_Sites_Sp, 500)

severe = c(1,2)

MIDAS_Colls = stat19[stat19$Accident_Severity %in% severe & stat19$Junction_Detail == 0 & stat19$Road_Type == 3,]

MIDAS_Colls = MIDAS_Colls[buff, ]

nrow(MIDAS_Colls)


stat_sev1 = stat19[stat19$Accident_Severity == 1, ]
stat_sev2 = stat19[stat19$Accident_Severity == 2, ]
stat_sev3 = stat19[stat19$Accident_Severity == 3, ]

# Date

Date_Stat_all = ggplot(data=stat19, aes(date(Date))) +
  geom_histogram(binwidth = 7, fill = "#910591") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date") +
  labs(title = "All Collisions Within 2km")

Date_Stat_slight = ggplot(data=stat_sev3, aes(date(Date))) +
  geom_histogram(binwidth = 7, fill = "#FED976") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date") +
  labs(title = "Slight Collisions Within 2km")

Date_Stat_severe = ggplot(data=stat_sev2, aes(date(Date))) +
  geom_histogram(binwidth = 7, fill = "#FD8D3C") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date") +
  labs(title = "Severe Collisions Within 2km")

Date_Stat_fatal = ggplot(data=stat_sev1, aes(date(Date))) +
  geom_histogram(binwidth = 7, fill = "#E31A1C") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date") +
  labs(title = "Fatal Collisions Within 2km")

sev1 = MIDAS_Colls[MIDAS_Colls$Accident_Severity == 1, ]
sev2 = MIDAS_Colls[MIDAS_Colls$Accident_Severity == 2, ]
sev3 = MIDAS_Colls[MIDAS_Colls$Accident_Severity == 3, ]
str(sev2)

MIDAS_Stat_all = ggplot(data=MIDAS_Colls, aes(date(Date))) +
  geom_histogram(binwidth = 7, fill = "#910591") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date") +
  labs(title = "All Collisions Selected")

MIDAS_Stat_slight = ggplot(data=sev3, aes(date(Date))) +
  geom_histogram(binwidth = 7, fill = "#FED976") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date") +
  labs(title = "Slight Collisions Selected")

MIDAS_Stat_severe = ggplot(data=sev2, aes(date(Date))) +
  geom_histogram(binwidth = 7, fill = "#FD8D3C") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date") +
  labs(title = "Severe Collisions Selected")

MIDAS_Stat_fatal = ggplot(data=sev1, aes(date(Date))) +
  geom_histogram(binwidth = 7, fill = "#E31A1C") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date")+
  labs(title = "Fatal Collisions Selected")))

grid.arrange(Date_Stat_all, Date_Stat_slight, Date_Stat_severe, Date_Stat_fatal, 
             MIDAS_Stat_all, MIDAS_Stat_slight, MIDAS_Stat_severe, MIDAS_Stat_fatal,
             nrow = 2, ncol = 4, top = "Collisions by Severity: Per Week")

sev1 = stat19[stat19$Accident_Severity == 1, ]
sev2 = stat19[stat19$Accident_Severity == 2, ]
sev3 = stat19[stat19$Accident_Severity == 3, ]

Time_Stat_all = ggplot(data=stat19, aes(hour(Time))) +
  geom_histogram(binwidth = 1, fill = "#910591") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time") +
  labs(title = "All Collisions Within 2km")

Time_Stat_slight = ggplot(data=sev3, aes(hour(Time))) +
  geom_histogram(binwidth = 1, fill = "#FED976") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time") +
  labs(title = "Slight Collisions Within 2km")

Time_Stat_severe = ggplot(data=sev2, aes(hour(Time))) +
  geom_histogram(binwidth = 1, fill = "#FD8D3C") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time") +
  labs(title = "Severe Collisions Within 2km")

Time_Stat_fatal = ggplot(data=sev1, aes(hour(Time))) +
  geom_histogram(binwidth = 1, fill = "#E31A1C") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time") +
  labs(title = "Fatal Collisions Within 2km")

sev1 = MIDAS_Colls[MIDAS_Colls$Accident_Severity == 1, ]
sev2 = MIDAS_Colls[MIDAS_Colls$Accident_Severity == 2, ]
sev3 = MIDAS_Colls[MIDAS_Colls$Accident_Severity == 3, ]

Time_MIDAS_all = ggplot(data=MIDAS_Colls, aes(hour(Time))) +
  geom_histogram(binwidth = 1, fill = "#910591") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time") +
  labs(title = "All Collisions Selected")

Time_MIDAS_slight = ggplot(data=sev3, aes(hour(Time))) +
  geom_histogram(binwidth = 1, fill = "#FED976") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time") +
  labs(title = "Slight Collisions Selected")

Time_MIDAS_severe = ggplot(data=sev2, aes(hour(Time))) +
  geom_histogram(binwidth = 1, fill = "#FD8D3C") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time") +
  labs(title = "Severe Collisions Selected")

Time_MIDAS_fatal = ggplot(data=sev1, aes(hour(Time))) +
  geom_histogram(binwidth = 1, fill = "#E31A1C") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time") +
  labs(title = "Fatal Collisions Selected")

grid.arrange(Time_Stat_all, Time_Stat_slight, Time_Stat_severe, Time_Stat_fatal,
             Time_MIDAS_all, Time_MIDAS_slight, Time_MIDAS_severe, Time_MIDAS_fatal,
             nrow = 2, ncol = 4, top = "Collisions by Severity: Time of Day")



ggplot(data=stat19, aes(Date)) + geom_histogram(binwidth =7, fill = )

ggplot(data=stat19_date, aes(stat19_date$Time)) + geom_histogram()

Month = Month()

head(date(halo_spatial$Datetime))

#### MIDAS Gold

# Date and Time

Date_MIDAS = ggplot(data=halo_spatial, aes(date(Datetime))) +
  geom_histogram(binwidth = 7, fill = "#9fdda5") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Date")

Time_MIDAS = ggplot(data=halo_spatial, aes(hour(Datetime))) +
  geom_histogram(binwidth = 1, fill = "#62d16d") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Time")

tots = halo_spatial2[,c("Hour","TotalFlow")]

tots_hours = as.data.frame(xtabs(TotalFlow~Hour, tots))

head(tots_hours)

Total_Over_Time = ggplot(data=tots_hours, aes(Hour, Freq)) +
  geom_bar(stat = "identity", fill = "#30b23d") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Total Flow Per Hour") +
  xlab("Hours in Day")

grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow = 3, ncol = 2, heights = unit(c(0.5, 6, 6), "null"), widths = unit(c(4,4), "null"))))
grid.text("MIDAS Gold Dataset Over Time", vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
print(Date_MIDAS, vp=viewport(layout.pos.row = 2, layout.pos.col = 1))
print(Time_MIDAS, vp=viewport(layout.pos.row = 2, layout.pos.col = 2))
print(Total_Over_Time, vp=viewport(layout.pos.row = 3, layout.pos.col = 1:2))

# Average Attributes

Speed_all = ggplot(data=halo_spatial, aes(AveSpeed)) +
  geom_histogram(binwidth = 2, fill = "blue") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Average Speed")

Occupancy_all = ggplot(data=halo_spatial, aes(AveOccupancy)) +
  geom_histogram(binwidth = 2, fill = "green") + 
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Average Occupancy")

Headway_all = ggplot(data=halo_spatial, aes(AveHeadway)) +
  geom_histogram(binwidth = 2, fill = "purple") + 
  scale_y_continuous(labels = prettyformat)+
  ylab("Frequency") +
  xlab("Average Headway")

TotalFlow_all = ggplot(data=halo_spatial, aes(TotalFlow)) +
  geom_histogram(binwidth = 2, fill = "darkblue") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Total Flow")

grid.arrange(Speed_all, Occupancy_all, Headway_all, TotalFlow_all, nrow = 2, ncol = 2, top = "Attribute Profiles")

# Average Speeds By Lane

dummy1 = halo_spatial[halo_spatial$Average_Speed_Lane_1 != 0 & halo_spatial$Average_Speed_Lane_1 != 255, ]
dummy2 = halo_spatial[halo_spatial$Average_Speed_Lane_2 != 0 & halo_spatial$Average_Speed_Lane_2 != 255, ]
dummy3 = halo_spatial[halo_spatial$Average_Speed_Lane_3 != 0 & halo_spatial$Average_Speed_Lane_3 != 255, ]
dummy4 = halo_spatial[halo_spatial$Average_Speed_Lane_4 != 0 & halo_spatial$Average_Speed_Lane_4 != 255, ]
dummy5 = halo_spatial[halo_spatial$Average_Speed_Lane_5 != 0 & halo_spatial$Average_Speed_Lane_5 != 255, ]


Speed1 = ggplot(data=dummy1, aes(Average_Speed_Lane_1)) +
  geom_histogram(binwidth = 2, fill = "#C6DBEF") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Average Speed Lane 1") + 
  xlim(c(0,255))

Speed2 = ggplot(data=dummy2, aes(Average_Speed_Lane_2)) +
  geom_histogram(binwidth = 2, fill = "#9ECAE1") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Average Speed Lane 2") + 
  xlim(c(0,255))

Speed3 = ggplot(data=dummy3, aes(Average_Speed_Lane_3)) +
  geom_histogram(binwidth = 2, fill = "#6BAED6") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Average Speed Lane 3") + 
  xlim(c(0,255))

Speed4 = ggplot(data=dummy4, aes(Average_Speed_Lane_4)) +
  geom_histogram(binwidth = 2, fill = "#4292C6") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Average Speed Lane 4") + 
  xlim(c(0,255))

Speed5 = ggplot(data=dummy5, aes(Average_Speed_Lane_5)) +
  geom_histogram(binwidth = 2, fill = "#2171B5") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Average Speed Lane 5") + 
  xlim(c(0,255))

Speed_all = ggplot(data=halo_spatial, aes(AveSpeed)) +
  geom_histogram(binwidth = 2, fill = "#08306B") +
  scale_y_continuous(labels = prettyformat) +
  ylab("Frequency") +
  xlab("Average Speed Across All Lanes") + 
  xlim(c(0,255))

grid.arrange(Speed1, Speed2, Speed3, Speed4, Speed5, Speed_all, nrow = 2, ncol = 3, top = "Speed Profiles by Lane")

# number of lanes

table(halo_spatial$Number_of_Lanes)

