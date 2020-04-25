library('ggplot2');
library('forcats');
library('tidyr');


getDaytype <- function(dates) {
  result <- weekdays(dates) %in% c('Saturday','Sunday');
  result <- factor(result, levels = c(FALSE, TRUE), labels = c('Weekday','Weekend/Holiday'));
  result[dates == as.Date('2020/04/10')] <- 'Weekend/Holiday';   # Good Friday
  result
}
getRolling <- function(data) {
  result <- rep(NA, nrow(data));
  # TODO: must be some way to do this with a group by.
  for(daytype in levels(data$daytype))
    for(category in levels(data$category))
      for(region in levels(data$region)) {
        filter <- data$daytype == daytype & data$category == category & data$region == region;
        result[filter] <- rollmean(data[filter,]$value, ifelse(daytype=='Weekday',5,2), fill=NA, align='right');
      }
  result
}

# Actually: blank field = NA.
google <- read.csv('../input/google.csv', na.strings='ZZZZZZ');
colnames(google)[1:4] <- c('country_code', 'country', 'region', 'subregion');
google <- google[google$country_code=='CA',];
# TODO: deal with provinces with NA data.
google <- google[!google$region %in% c('Northwest Territories', 'Nunavut', 'Prince Edward Island', 'Yukon'),];
colnames(google)[6:11] <- c('retail', 'grocery', 'park', 'transit', 'work', 'res');
provinceOrder <-
  c('','British Columbia', 'Alberta', 'Saskatchewan', 'Manitoba',
    'Ontario', 'Quebec',
    #    'Prince Edward Island',
    'New Brunswick', 'Nova Scotia', 'Newfoundland and Labrador'
    #    'Yukon', 'Northwest Territories', 'Nunavut'
  );
google$region <- fct_relevel(google$region, provinceOrder);
levels(google$region)[1] = 'Canada';
google$date <- as.Date(google$date);
google$daytype <- getDaytype(google$date);
# Change from all categories on one row, to one row per category.
google <- do.call('rbind', lapply(6:11, function(col) {
  result <- google[,c(1:5,12:ncol(google))];
  result$category <- colnames(google)[col];
  result$value <- google[,col];
  result
}));
google$value[google$value=='NA'] <- NA;
categoryOrder <- c('work', 'transit', 'grocery', 'retail', 'res', 'park');
google$category <- fct_relevel(google$category, categoryOrder);

google <- droplevels(google);
region.labs = c('Canada', 'Brit Columbia', 'Alberta', 'Saskatchewan', 'Manitoba', 'Ontario', 'Quebec', 'New Brunsw', 'Nova Scotia', 'Newfoundland');
names(region.labs) = levels(google$region);
region.labs2 = c('Canada', 'Brit Colum', 'Alberta', 'Saskatch', 'Manitoba', 'Ontario', 'Quebec', 'New Bruns', 'Nova Scot', 'Newfound');
names(region.labs2) = levels(google$region);
category.labs = c('Workplace', 'Transit Stations', 'Grocery & Phar', 'Retail & Recreat', 'Residential', 'Park');
names(category.labs) = levels(google$category);
category.labs2 = c('Workplace', 'Transit Stations', 'Grocery & Pharmacy', 'Retail & Recreation', 'Residential', 'Park');
names(category.labs2) = levels(google$category);

#google$category_daytype <- with(google, interaction(google$daytype, google$category));
#google$region_date <- with(google, interaction(google$date, google$region));

#print(xtabs(formula = value ~ region_date + category_daytype, google)))
# TODO: must be some way to do this with a group by.
google$value7 <- getRolling(google);
ggplot(google, aes(y=value7, x=date)) +
  ggtitle(paste0("Mobility in Canada During Covid (as of ", format.Date(max(google$date), "%b %d"), ")")) +
  geom_line(aes(y=value7, color=daytype)) +
  facet_grid(rows=vars(category), cols=vars(region), scales = 'free_y', switch='y',
             labeller = labeller(region = region.labs, category=category.labs)) +
  coord_cartesian(xlim=c(as.Date("2020/03/01"), Sys.Date() - 1)) +
  theme_light() +
  scale_color_brewer(palette="Set1") +
  geom_hline(yintercept = 0, alpha=0.5) +
  theme(legend.position="bottom") +
  labs(y="Google Community Mobility Index", x="", color = "Day type", caption="Rolling 7 day average. drpritch.github.io/covid-mobility-canada") +
  theme(axis.text.x = element_text(angle = 90));
ggsave(filename = '../output/google_all.png',
  device = 'png',
  width=6.5, height=5.5, units='in', scale=1.5,
  dpi='print'
);

ggplot(google[google$date>=as.Date("2020-03-23") - 7,],
       aes(y=value, x=date)) +
  ggtitle(paste0("Mobility in Canada After Lockdown Low (as of ", format.Date(max(google$date), "%b %d"), ")")) +
  geom_point(aes(color=daytype), size=1, alpha=0.2) +
  geom_line(aes(y=value7, color=daytype)) +
  facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y',
    labeller = labeller(region = region.labs2, category=category.labs2)) +
  coord_cartesian(xlim=c(as.Date("2020/03/22"), Sys.Date() - 1)) +
  scale_color_brewer(palette="Set1") +
  geom_hline(yintercept = 0, alpha=0.5) +
  theme_light() +
  theme(legend.position="bottom") +
  labs(y="Google Community Mobility Index", x="", color = "Day type", caption="Rolling 7 day average. drpritch.github.io/covid-mobility-canada") +
  theme(axis.text.x = element_text(angle = 90));
# Deliberately narrower, to exaggerate slopes.
ggsave(filename = '../output/google_post.png',
       device = 'png',
       width=4, height=5.5, units='in', scale=2.0,
       dpi='print'
);





apple <- read.csv('../input/apple.csv');
apple <- as.data.frame(
  apple %>%
  tidyr::pivot_longer(cols=4:ncol(apple), names_to='date', names_prefix='X')
);
apple$date <- as.Date(apple$date, "%Y.%m.%d");
regionOrder <-
  c('Canada', 'Vancouver', 'Edmonton', 'Calgary', 'Toronto', 'Ottawa', 'Montreal', 'Halifax');
apple <- apple[apple$region%in% regionOrder,];
apple$region <- fct_relevel(apple$region, regionOrder);
apple$daytype <- getDaytype(apple$date);
colnames(apple)[3] <- 'category'; # mode, really

apple <- droplevels(apple);
weekdayList <- c('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
for (region in levels(apple$region)) {
  regionFilter <- apple$region == region;
  baselineDayOfWeek <- lapply(levels(apple$category), function(mode) {
    modeFilter <- regionFilter & apple$category == mode;
    baselineFilter <- modeFilter & apple$date < as.Date('2020/02/10');
    numerator <- apple$value[baselineFilter][1]; # always 100
    baselineDayOfWeek_mode <- lapply(weekdayList, function(day) {
      weekdayFilter <- weekdays(apple$date, abbreviate=TRUE) == day;
      # Factor to change baseline from row 1 (as shipped by Apple) to average of
      # equivalent weekday in period.
      numerator / median(apple$value[baselineFilter & weekdayFilter])
    });
    names(baselineDayOfWeek_mode) <- weekdayList;
    # Weekend-to-weekday ratio. About +15% for drive, +20% for transit, +10% for walk
    # Makes sense, for a trip planner app.
    #print(mean(unlist(baselineDayOfWeek_mode[c(1,7)]))/mean(unlist(baselineDayOfWeek_mode[2:6])));
    baselineDayOfWeek_mode
  });
  
  baselineDayOfWeek <- as.data.frame(do.call(rbind, baselineDayOfWeek));
  rownames(baselineDayOfWeek) <- levels(apple$category);
  print(region)
  print(baselineDayOfWeek);
  
  for (mode in levels(apple$category)) {
    modeFilter <- regionFilter & apple$category == mode;
    baselineDayOfWeek_mode <- baselineDayOfWeek[rownames(baselineDayOfWeek) == mode,];
    apple$value[modeFilter] <- apple$value[modeFilter] *
      unlist(baselineDayOfWeek_mode[weekdays(apple$date[modeFilter], abbreviate=T)]) - 100;
  }
}

apple$value7 <- getRolling(apple);

region.labs2 <- levels(apple$region);
region.labs2[2:3] <- c('Vancouv', 'Edmont');
names(region.labs2) <- levels(apple$region);

ggplot(apple, aes(y=value7, x=date)) +
  ggtitle(paste0("Mobility in Canada During Covid (as of ", format.Date(max(apple$date), "%b %d"), ")")) +
  geom_line(aes(y=value7, color=daytype)) +
  facet_grid(rows=vars(category), cols=vars(region), scales = 'free_y', switch='y') +
#               labeller = labeller(region = region.labs, category=category.labs)) +
  coord_cartesian(xlim=c(as.Date("2020/02/01"), Sys.Date() - 1)) +
  theme_light() +
  scale_color_brewer(palette="Set1") +
  geom_hline(yintercept = 0, alpha=0.5) +
  theme(legend.position="bottom") +
  labs(y="Apple Mobility Index", x="", color = "Day type", caption="Rebaselined similar to Google Mobility Index. Rolling 7 day average. drpritch.github.io/covid-mobility-canada") +
  theme(axis.text.x = element_text(angle = 90));
ggsave(filename = '../output/apple_all.png',
       device = 'png',
       width=6.5, height=4, units='in', scale=1.5,
       dpi='print'
);
ggplot(apple[apple$date>=as.Date("2020-03-23") - 7,],
       aes(y=value, x=date)) +
  ggtitle(paste0("Mobility in Canada After Lockdown Low (as of ", format.Date(max(apple$date), "%b %d"), ")")) +
  geom_point(aes(color=daytype), size=1, alpha=0.2) +
  geom_line(aes(y=value7, color=daytype)) +
  facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y',
               labeller = labeller(region = region.labs2)) +
  coord_cartesian(xlim=c(as.Date("2020/03/22"), Sys.Date() - 1)) +
  scale_color_brewer(palette="Set1") +
  geom_hline(yintercept = 0, alpha=0.5) +
  theme_light() +
  theme(legend.position="bottom") +
  labs(y="Apple Mobility Index", x="", color = "Day type", caption="Rebaselined similar to Google Mobility Index. Rolling 7 day average. drpritch.github.io/covid-mobility-canada") +
  theme(axis.text.x = element_text(angle = 90));  ggsave(filename = '../output/apple_post.png',
       device = 'png',
       width=3, height=4, units='in', scale=1.8,
       dpi='print'
);
