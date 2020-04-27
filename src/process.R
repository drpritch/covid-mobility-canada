library('ggplot2');
library('forcats');
library('tidyr');
library('zoo');

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
getMin <- function(data, minDate = as.Date('2020/03/23')) {
  result <- rep(NA, nrow(data));
  for(daytype in levels(data$daytype))
    for(category in levels(data$category))
      for(region in levels(data$region)) {
        filter <- data$daytype == daytype & data$category==category & 
          data$region==region & data$date>=minDate +4;
        # Using value7 with date+4 gives 5-day average from 3/23-3/27
        # Deliberately leave everything before date threshold as NA
        result[filter] <- data[filter,]$value7[1];
      }
  result
}


# Actually: blank field = NA.
google <- read.csv('../input/google.csv', na.strings='ZZZZZZ');
colnames(google)[1:4] <- c('country_code', 'country', 'region', 'subregion');
google <- google[google$country_code=='CA',];
google$region <- fct_recode(google$region,
    Canada='',
    BC='British Columbia',
    PEI='Prince Edward Island',
    Newfoundland='Newfoundland and Labrador',
    NWT='Northwest Territories'
);
provinceOrder <-
  c('Canada','BC', 'Alberta', 'Saskatchewan', 'Manitoba',
    'Ontario', 'Quebec',
    #    'PEI',
    'New Brunswick', 'Nova Scotia', 'Newfoundland'
    #    'Yukon', 'NWT', 'Nunavut'
);
google$region <- fct_relevel(google$region, provinceOrder);
# TODO: deal with provinces with NA data.
google <- google[!google$region %in% c('PEI','Yukon','NWT', 'Nunavut'),];

colnames(google)[6:11] <- c('retail', 'grocery', 'park', 'transit', 'work', 'res');
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

#google$category_daytype <- with(google, interaction(google$daytype, google$category));
#google$region_date <- with(google, interaction(google$date, google$region));

#print(xtabs(formula = value ~ region_date + category_daytype, google)))



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



########################################################################################
# Plotting

setupPlot <- function(p, startDate = '2020/03/01', isGoogle = TRUE, isDouble=FALSE, dateSpacing = '1 week') {
  isApple <- !isGoogle;
  isTop <- ifelse(isDouble, isApple, TRUE);
  isBottom <- ifelse(isDouble, isGoogle, TRUE);
  result <- p +
    coord_cartesian(xlim=c(as.Date(startDate), Sys.Date() - 1)) +
    theme_light() +
    scale_color_brewer(palette="Set1") +
    geom_hline(yintercept = 0, alpha=0.5) +
    theme(axis.title.x=element_blank()) +
    labs(y=ifelse(isGoogle, "Google Community Mobility Index", "Apple Mobility Index"),
         x="", color = "");
  if (isBottom)
    result <- result + labs(caption=ifelse(isDouble, ifelse(isGoogle, "Apple data rebaselined similar to Google. Rolling 7 day average.\ndrpritch.gitub.io/covid-mobility-canada", ""),
                     ifelse(isGoogle, "Rolling 7 day average. drpritch.github.io/covid-mobility-canada",
                            "Rebaselined similar to Google data. Rolling 7 day average. drpritch.githib.io/covid-mobility-canada")));
  if(isBottom) {
    result <- result +
      scale_x_date(date_breaks = dateSpacing, date_labels='%b %d') +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 90));
  } else {
    result <- result +
      theme(legend.position = 'none', axis.text.x = element_blank());
  }
  result;
}


region.labs = c('Canada', 'Brit Columbia', 'Alberta', 'Saskatchewan', 'Manitoba', 'Ontario', 'Quebec', 'New Brunsw', 'Nova Scotia', 'Newfoundland');
names(region.labs) = levels(google$region);
region.labs2 = c('Canada', 'Brit Colum', 'Alberta', 'Saskatch', 'Manitoba', 'Ontario', 'Quebec', 'New Bruns', 'Nova Scot', 'Newfound');
names(region.labs2) = levels(google$region);
category.labs = c('Workplace', 'Transit Stations', 'Grocery & Phar', 'Retail & Recreat', 'Residential', 'Park');
names(category.labs) = levels(google$category);
category.labs2 = c('Workplace', 'Transit Stations', 'Grocery & Pharmacy', 'Retail & Recreation', 'Residential', 'Park');
names(category.labs2) = levels(google$category);google$value7 <- getRolling(google);

setupPlot(
  ggplot(google, aes(y=value7, x=date)) +
    ggtitle(paste0("Mobility in Canada During Covid (as of ", format.Date(max(google$date), "%b %d"), ")")) +
    geom_line(aes(y=value7, color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales = 'free_y', switch='y',
               labeller = labeller(region = region.labs, category=category.labs)),
  startDate = '2020/03/01',
  isGoogle = TRUE, dateSpacing = '2 weeks');
ggsave(filename = '../output/google_all.png', device = 'png', dpi='print',
       width=6.5, height=5.5, units='in', scale=1.5
);

setupPlot(
  ggplot(google[google$date>=as.Date("2020-03-23") - 7,], aes(y=value, x=date)) +
    ggtitle(paste0("Mobility in Canada After Lockdown Low (as of ", format.Date(max(google$date), "%b %d"), ")")) +
    geom_point(aes(color=daytype), size=0.5, alpha=0.2) +
    geom_line(aes(y=value7, color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y',
               labeller = labeller(region = region.labs2, category=category.labs2)),
  startDate = '2020/03/22',
  isGoogle = TRUE);
# Deliberately narrower, to exaggerate slopes.
ggsave(filename = '../output/google_post.png', device = 'png', dpi='print',
       width=4, height=5.5, units='in', scale=2.0
);


google$valueMin <- getMin(google);
google$valueMin[google$category == 'park' & google$daytype=='Weekend/Holiday'] <- NA;
for (region in levels(google$region)) {
  regionFilter <- google$region == region;
  regionFilename <- tolower(gsub(' ','',region));
  setupPlot(
    ggplot(google[regionFilter,], aes(y=value, x=date)) +
      ggtitle(paste0("Mobility in ", region, " During Covid (as of ", format.Date(max(google$date), "%b %d"), ")")) +
      geom_ribbon(aes(ymin=valueMin, ymax=value7, fill=daytype), alpha=0.25, show.legend=FALSE) +
      geom_line(aes(y=value7, color=daytype)) +
      facet_wrap(~category, switch='y',
                 labeller = labeller(category=category.labs)),
    startDate = '2020/03/01',
    isGoogle = TRUE, isDouble = TRUE);
  ggsave(filename = paste0('../output/google_', regionFilename, '.png'), device = 'png', dpi='print',
         width=4, height=3, units='in', scale=1.5);
}

apple$value7 <- getRolling(apple);
apple$valueMin <- getMin(apple);

region.labs2 <- levels(apple$region);
region.labs2[2:3] <- c('Vancouv', 'Edmont');
names(region.labs2) <- levels(apple$region);

setupPlot(
  ggplot(apple, aes(y=value7, x=date)) +
    ggtitle(paste0("Mobility in Canada During Covid (as of ", format.Date(max(apple$date), "%b %d"), ")")) +
    geom_line(aes(y=value7, color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales = 'free_y', switch='y'),
  startDate = '2020/02/01',
  isGoogle=FALSE, dateSpacing = '2 weeks');
ggsave(filename = '../output/apple_all.png', device = 'png', dpi='print',
   width=6.5, height=4, units='in', scale=1.5
);
setupPlot(
  ggplot(apple[apple$date>=as.Date("2020-03-23") - 7,], aes(y=value, x=date)) +
    ggtitle(paste0("Mobility in Canada After Lockdown Low (as of ", format.Date(max(apple$date), "%b %d"), ")")) +
    geom_point(aes(color=daytype), size=0.5, alpha=0.2) +
    geom_line(aes(y=value7, color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y',
               labeller = labeller(region = region.labs2)),
  startDate='2020/03/22',
  isGoogle=FALSE);
ggsave(filename = '../output/apple_post.png', device = 'png', dpi='print',
   width=3, height=4, units='in', scale=2.0
);
for (region in levels(apple$region)) {
  regionFilter <- apple$region == region;
  regionFilename <- tolower(gsub(' ','',region));
  setupPlot(
    ggplot(apple[regionFilter,], aes(y=value, x=date)) +
      ggtitle(paste0("Mobility in ", region, " During Covid (as of ", format.Date(max(apple$date), "%b %d"), ")")) +
      geom_ribbon(aes(ymin=valueMin, ymax=value7, fill=daytype), alpha=0.25, show.legend=FALSE) +
      geom_line(aes(y=value7, color=daytype)) +
      facet_wrap(~category, switch='y'),
    startDate = '2020/03/01',
    isGoogle = FALSE, isDouble=TRUE);
  ggsave(filename = paste0('../output/apple_',regionFilename,'.png'), device = 'png', dpi='print',
         width=4, height=1.3, units='in', scale=1.5);
}
