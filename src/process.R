library('ggplot2');
library('forcats');
library('tidyr');
library('zoo');   # for rollmean
library('RColorBrewer');

provinceOrder <-
  c('Canada','BC', 'Alberta', 'Saskatchewan', 'Manitoba',
    'Ontario', 'Quebec',
    #    'PEI',
    'New Brunswick', 'Nova Scotia', 'Newfoundland'
    #    'Yukon', 'NWT', 'Nunavut'
  );
cityToProvince <- c(Vancouver = 'BC', Edmonton = 'Alberta', Calgary = 'Alberta',
  Toronto = 'Ontario', Ottawa = 'Ontario', Montreal = 'Quebec', Halifax = 'Nova Scotia');
# 2019 populations, StatsCan table 17-10-0135-01
# Ottawa: both Ontario and Quebec parts used
cityPopulation <- c(Vancouver=2691351, Edmonton=1447143, Calgary=1514723,
  Toronto=6471850, Ottawa=1441118, Montreal=4318505, Halifax=440348);
canadaPopulation <- 31484234;

# First day of the lowest week post-covid lockdown.
minDateRegion <- rep(as.Date('2020/03/30'), length(provinceOrder));
names(minDateRegion) <- provinceOrder;
minDateRegion[c('BC','Ontario','Newfoundland')] <- as.Date('2020/03/23');
for (city in names(cityToProvince))
  minDateRegion[city] <- minDateRegion[cityToProvince[city]];

getDaytype <- function(dates) {
  result <- weekdays(dates) %in% c('Saturday','Sunday');
  result <- factor(result, levels = c(FALSE, TRUE), labels = c('Weekday','Weekend/Holiday'));
  result[dates == as.Date('2020/04/10')] <- 'Weekend/Holiday';   # Good Friday
  result
}
getRolling <- function(data) {
  result <- rep(NA, nrow(data));
  # TODO: must be some way to do this with a group by.
  for(category in levels(data$category))
    for(region in levels(data$region)) {
      filter <- data$category == category & data$region == region;
      # New Brunswick has a number of Saturday transit datapoints that are NA.
      result[filter] <- rollapply(data[filter,]$value, 7, function(x) { mean(x, na.rm=TRUE) },
                                  fill=NA, align='center');
    }
  result
}
getMin <- function(data) {
  result <- rep(NA, nrow(data));
  for(category in levels(data$category))
    for(region in levels(data$region)) {
      filter <- data$category==category &  data$region==region &
        data$date>=minDateRegion[region] +3;
      # Using value7 with date+3 gives 7-day average from 3/23-3/29
      # Deliberately leave everything before date threshold as NA
      result[filter] <- data[filter,]$value7[1];
    }
  result
}
getValueLabel <- function(data, minDate) {
  result <- rep(NA, nrow(data));
  # Show: minimum date, final date (at 7-day rolling center), final date minus a week
  filter <- data$date %in% c(max(data$date) - 10, max(data$date) - 3);
  for(region in levels(data$region)) {
    filter[data$region==region & data$date==minDateRegion[region] + 3] <- TRUE;
  }
  result[filter] <- round(data[filter,]$value7, 0);
  result
}
signAndRound <- function(value) {
  ifelse(value <= 0.5, round(value), paste0('+', round(value)))
}
arrowAndRound <- function(value) {
  #  paste0(ifelse(value < 0, '~scriptstyle(▼)~', ifelse(value > 0, '~scriptstyle(▲)~', '')), abs(round(value,0)))
  #paste0(ifelse(value < 0, "phantom(0) %down% ", ifelse(value > 0, 'phantom(0) %up% ', '')), abs(round(value,0)))
  paste0(ifelse(value < 0, " %down% ", ifelse(value > 0, ' %up% ', '')), abs(round(value,0)))
}
getHeadlineLabel <- function(data, startDate, useTiny = TRUE) {
  result <- rep(NA, nrow(data));
  for(category in levels(data$category))
    for(region in levels(data$region)) {
      minDate <- minDateRegion[region] + 3;
      minDatePlotMath <- format.Date(minDate, '%b~%d');
      filter <- data$category==category & data$region==region;
      dateFilter <- data$date %in% c(minDate, max(data$date) - 10, max(data$date) - 3);
      points <- data$value7[filter & dateFilter];
      result[filter & data$date == startDate] <- paste0(
        signAndRound(points[3]-points[1]),
        ifelse(useTiny,'~scriptscriptstyle(','~'), 'from~', minDatePlotMath, ifelse(useTiny, ')',''))
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
  tidyr::pivot_longer(cols=5:ncol(apple), names_to='date', names_prefix='X')
);
apple$date <- as.Date(apple$date, "%Y.%m.%d");
regionOrder <-
  c('Canada', 'BC', 'Vancouver', 'Alberta', 'Edmonton', 'Calgary',
    'Saskatchewan', 'Manitoba', 'Ontario', 'Toronto', 'Ottawa',
    'Quebec', 'Montreal', 'New Brunswick', 'Nova Scotia', 'Halifax', 'Newfoundland');
apple$region <- fct_recode(apple$region,
                           BC='British Columbia',
                           PEI='Prince Edward Island',
                           Newfoundland='Newfoundland and Labrador',
                           NWT='Northwest Territories');
apple <- apple[apple$region%in% regionOrder,];
apple$region <- fct_relevel(apple$region, regionOrder);
names(cityPopulation) <- factor(names(cityPopulation), levels=regionOrder);
apple$province <- apple$region;
apple$province[apple$region=='Vancouver'] <- 'BC';
apple$province[apple$region %in% c('Edmonton', 'Calgary')] <- 'Alberta';
apple$province[apple$region %in% c('Toronto', 'Ottawa')] <- 'Ontario';
apple$province[apple$region == 'Montreal'] <- 'Quebec';
apple$province[apple$region == 'Halifax'] <- 'Nova Scotia';
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

setupPlot <- function(p, startDate = '2020/03/01', isGoogle = TRUE, isDouble=FALSE, dateSpacing = '1 week', regionName=NULL) {
  isApple <- !isGoogle;
  isTop <- ifelse(isDouble, isApple, TRUE);
  isBottom <- ifelse(isDouble, isGoogle, TRUE);
  isPost <- dateSpacing == '1 week' & isDouble==FALSE;
  ylim <- NULL;
  if(!isPost) {
    ylim <- c(-90, ifelse(isApple, 30, 90));
  }
  else if(isGoogle) {
    ylim <- c(-90, NA);
  }
  result <- p +
    theme_light() +
    scale_y_continuous(breaks=seq(-100,200,by=20), minor_breaks=seq(-100,200, by=10),
                       limits=ylim,
                       name=ifelse(isGoogle, "Google Community Mobility Index", "Apple Mobility Index")) +
    scale_x_date(date_breaks = dateSpacing, date_labels='%b %d',
                 limits = c(as.Date(startDate), Sys.Date() - 1)) +
    scale_color_brewer(palette="Set1") +
    geom_hline(yintercept = 0, alpha=0.5) +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90)) +
    labs(x="", color="");
  if (isBottom)
    result <- result + labs(caption=ifelse(isDouble, ifelse(isGoogle, "Apple data rebaselined similar to Google. Rolling 7 day average.\ndrpritch.gitub.io/covid-mobility-canada", ""),
                     ifelse(isGoogle, "Rolling 7 day average. drpritch.github.io/covid-mobility-canada",
                            "Rebaselined similar to Google data. Rolling 7 day average. drpritch.githib.io/covid-mobility-canada")));
  result;
}
redFill <- brewer.pal(n=8, 'Set1')[1];
blueFill <- brewer.pal(n=8, 'Set1')[2];


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
    geom_line(aes(y=value7)) +
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
    geom_point(size=0.5, alpha=0.2) +
    geom_line(aes(y=value7)) +
    facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y',
               labeller = labeller(region = region.labs2, category=category.labs2)),
  startDate = '2020/03/22',
  isGoogle = TRUE);
# Deliberately narrower, to exaggerate slopes.
ggsave(filename = '../output/google_post.png', device = 'png', dpi='print',
       width=4, height=5.5, units='in', scale=2.0
);


googleHeadlineTiny <- Sys.Date() - max(google$date) > 6;
google$valueMin <- getMin(google);
#google$valueMin[google$category == 'park'] <- NA;
google$value7_pos <- pmax(google$value7, google$valueMin);
google$value7_neg <- pmin(google$value7, google$valueMin);
google$valueLabel <- getValueLabel(google);
google$headlineLabel <- getHeadlineLabel(google, startDate='2020/03/01',
      # If headline is big, made the suffix tiny                                   
      useTiny=!googleHeadlineTiny);
for (region in levels(google$region)) {
  regionFilter <- google$region == region;
  regionFilename <- tolower(gsub(' ','',region));
  setupPlot(
    ggplot(google[regionFilter,], aes(y=value7, x=date)) +
      ggtitle(paste0("Mobility in ", region, " During Covid (as of ", format.Date(max(google$date), "%b %d"), ")")) +
      geom_point(aes(y=value), size=0.25, alpha=0.2) +
      geom_ribbon(data=google[regionFilter,],
                  aes(ymin=valueMin, ymax=value7_pos), fill=redFill, alpha=0.5, show.legend=FALSE) +
      geom_ribbon(data=google[regionFilter,],
                  aes(ymin=valueMin, ymax=value7_neg), fill=blueFill, alpha=0.5, show.legend=FALSE) +
      geom_line() +
      geom_text(aes(label=valueLabel), size=2, nudge_y = 5, color='#555555') +
      geom_label(aes(label = headlineLabel, y = -Inf), hjust='left', vjust='bottom',
                 size=ifelse(googleHeadlineTiny, 3, 5), parse=TRUE, label.size=0, fill='#ffffff00') +
      facet_wrap(~category, switch='y',
                 labeller = labeller(category=category.labs)),
    startDate = '2020/03/01',
    isGoogle = TRUE, isDouble = TRUE);
  ggsave(filename = paste0('../output/google_', regionFilename, '.png'), device = 'png', dpi='print',
         width=4, height=3, units='in', scale=1.5);
}

apple$value7 <- getRolling(apple);
apple$valueMin <- getMin(apple);
apple$value7_pos <- pmax(apple$value7, apple$valueMin);
apple$value7_neg <- pmin(apple$value7, apple$valueMin);
apple$valueLabel <- getValueLabel(apple);
apple$headlineLabel <- getHeadlineLabel(apple, startDate='2020/03/01');

region.labs2 <- levels(apple$region);
region.labs2[c(3,5)] <- c('Vancouv', 'Edmont');
names(region.labs2) <- levels(apple$region);

setupPlot(
  ggplot(apple, aes(y=value7, x=date)) +
    ggtitle(paste0("Mobility in Canada During Covid (as of ", format.Date(max(apple$date), "%b %d"), ")")) +
    geom_line(aes(y=value7)) +
    facet_grid(rows=vars(category), cols=vars(region), scales = 'free_y', switch='y'),
  startDate = '2020/02/01',
  isGoogle=FALSE, dateSpacing = '2 weeks');
ggsave(filename = '../output/apple_all.png', device = 'png', dpi='print',
   width=12, height=4, units='in', scale=1.5
);
setupPlot(
  ggplot(apple[apple$date>=as.Date("2020-03-23") - 7,], aes(y=value, x=date)) +
    ggtitle(paste0("Mobility in Canada After Lockdown Low (as of ", format.Date(max(apple$date), "%b %d"), ")")) +
    geom_point(size=0.5, alpha=0.2) +
    geom_line(aes(y=value7)) +
    facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y',
               labeller = labeller(region = region.labs2)),
  startDate='2020/03/22',
  isGoogle=FALSE);
ggsave(filename = '../output/apple_post.png', device = 'png', dpi='print',
   width=6, height=4, units='in', scale=2.0
);
for (province in levels(apple$province)) {
  provinceFilter <- apple$province == province;
  provinceFilename <- tolower(gsub(' ','',province));
  setupPlot(
    ggplot(apple[provinceFilter,], aes(y=value7, x=date)) +
      geom_point(aes(y=value), size=0.25, alpha=0.2) +
      geom_ribbon(data=apple[provinceFilter,],
                  aes(ymin=valueMin, ymax=value7_pos), fill=redFill, alpha=0.5, show.legend=FALSE) +
      geom_ribbon(data=apple[provinceFilter,],
                  aes(ymin=valueMin, ymax=value7_neg), fill=blueFill, alpha=0.5, show.legend=FALSE) +
      geom_line() +
      geom_text(aes(label=valueLabel), size=2, nudge_y = 5, color='#555555') +
      geom_label(aes(label = headlineLabel, y = -Inf), hjust='left', vjust='bottom',
                 size=5, parse=TRUE, label.size=0, fill='#ffffff00') +
      facet_grid(rows=vars(region), cols=vars(category), switch='y'),
    startDate = '2020/03/01',
    isGoogle = FALSE, isDouble=TRUE);
  nregions <- length(levels(droplevels(apple[provinceFilter,]$region)))
  ncats <- length(levels(droplevels(apple[provinceFilter,]$category)))
  ggsave(filename = paste0('../output/apple_',provinceFilename,'.png'), device = 'png', dpi='print',
         width=ifelse(ncats==3,4,1.5), height=nregions*1.2 + 0.4, units='in', scale=1.5);
}

apple$value7_urbWt <- apple$value7 * cityPopulation[match(apple$region, names(cityPopulation))] / sum(cityPopulation);
appleBigCities <- aggregate(value7_urbWt~date+category, apple[apple$region %in% names(cityPopulation),], sum);
appleBigCities$region <- 'BigCities';
colnames(appleBigCities)[3] <- 'value7';
appleBigCities<- rbind(appleBigCities, data.frame(apple[apple$region=='Canada',c('date','category','value7','region')]));
appleBigCities$value7_rurWt <- ifelse(appleBigCities$region=='Canada',canadaPopulation / nonCityPopulation,-sum(cityPopulation) / nonCityPopulation) * appleBigCities$value7;
appleNonCities <- aggregate(value7_rurWt ~ date + category, appleBigCities, sum);
appleNonCities$region <- 'SmallCitiesRural';
colnames(appleNonCities)[3] <- 'value7';
appleBigCities$value7_rurWt <- NULL;
appleBigCities <- rbind(appleBigCities, appleNonCities);
#ggplot(appleBigCities, aes(y=value7, x=date)) + geom_line(aes(color=region)) + facet_grid(row=vars(category))
#ggplot(apple[apple$category=='driving' & !apple$region %in% names(cityPopulation),], aes(y=value7, x=date)) + geom_line(aes(color=region))
#ggplot(apple[apple$region %in% names(cityPopulation),], aes(y=value7, x=date)) + geom_line(aes(color=region)) + facet_grid(rows=vars(category))

