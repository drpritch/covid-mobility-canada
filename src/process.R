library('ggplot2');
library('forcats');
library('tidyr');
library('zoo');   # for rollmean
library('RColorBrewer');


# TODO: use subset and summarize throughout


provinceOrder <-
  c('Canada','BC', 'Alberta', 'Saskatchewan', 'Manitoba',
    'Ontario', 'Quebec',
    #    'PEI',
    'New Brunswick', 'Nova Scotia', 'Newfoundland'
    #    'Yukon', 'NWT', 'Nunavut'
  );
cityToProvince <- c(Vancouver = 'BC', Edmonton = 'Alberta', Calgary = 'Alberta',
  Toronto = 'Ontario', Ottawa = 'Ontario', Montreal = 'Quebec', Halifax = 'Nova Scotia');
# July 1 2019 populations, StatsCan table 17-10-0009-01 and 17-10-0135-01
# Ottawa: both Ontario and Quebec parts used
regionPopulation <- c(
  Canada = 37589262,
  BC=5071336, Alberta=4371316, Ontario=14566547, Quebec=8484965, 'Nova Scotia'=971395,
  Vancouver=2691351, Edmonton=1447143, Calgary=1514723,
  Toronto=6471850, Ottawa=1441118, Montreal=4318505, Halifax=440348);

# First day of the lowest week post-covid lockdown.
minDateRegion <- rep(as.Date('2020/03/30'), length(provinceOrder));
names(minDateRegion) <- provinceOrder;
minDateRegion[c('BC','Ontario','Newfoundland')] <- as.Date('2020/03/23');
for (city in names(cityToProvince))
  minDateRegion[city] <- minDateRegion[cityToProvince[city]];

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
  if (!('cityRural' %in% colnames(data)))
    data$cityRural <- factor('dummy');
  for(category in levels(data$category))
    for(region in levels(data$region))
      for (cityRural in levels(data$cityRural)) {
        filter <- data$category==category & data$region==region & data$cityRural == cityRural &
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
  if (!('cityRural' %in% colnames(data)))
    data$cityRural <- factor('dummy');
  for (cityRural in levels(data$cityRural))
    for(region in levels(data$region)) {
      filter[data$region==region & data$cityRural==cityRural &
             data$date==minDateRegion[region] + 3] <- TRUE;
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
  if (!('cityRural' %in% colnames(data)))
    data$cityRural <- factor('dummy');
  for(category in levels(data$category))
    for(cityRural in levels(data$cityRural))
      for(region in levels(data$region)) {
        minDate <- minDateRegion[region] + 3;
        minDatePlotMath <- format.Date(minDate, '%b~%d');
        filter <- data$category==category & data$region==region & data$cityRural == cityRural;
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
# Change from all categories on one row, to one row per category.
google <- do.call('rbind', lapply(6:11, function(col) {
  result <- google[,c(1:5)];
  result$category <- colnames(google)[col];
  result$value <- google[,col];
  result
}));
google$value[google$value=='NA'] <- NA;
categoryOrder <- c('work', 'transit', 'grocery', 'retail', 'res', 'park');
google$category <- fct_relevel(google$category, categoryOrder);
google <- droplevels(google);


apple <- read.csv('../input/apple.csv');
apple <- as.data.frame(
  apple %>% pivot_longer(cols=7:ncol(apple), names_to='date', names_prefix='X')
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
names(regionPopulation) <- factor(names(regionPopulation), levels=regionOrder);
apple$province <- apple$region;
apple$province[apple$region=='Vancouver'] <- 'BC';
apple$province[apple$region %in% c('Edmonton', 'Calgary')] <- 'Alberta';
apple$province[apple$region %in% c('Toronto', 'Ottawa')] <- 'Ontario';
apple$province[apple$region == 'Montreal'] <- 'Quebec';
apple$province[apple$region == 'Halifax'] <- 'Nova Scotia';
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
    
    # Interpolate NAs. May 11/12 are both NAs throughout.
    apple$value[modeFilter] <- na.approx(apple$value[modeFilter]);
  }
}

extractCityRural <- function(apple, region, regionCityNames) {
  parentRegionDF <- data.frame(apple[apple$region==region, c('date','category','value','region')]);
  parentRegionDF$cityRural <- 'all';
  parentRegionPopulation <- regionPopulation[match(region, names(regionPopulation))];
  if (length(regionCityNames) > 0) {
    # Just pops of cities within the region
    regionCityPopulation <- regionPopulation[match(regionCityNames, names(regionPopulation))];
    apple$value_urbWt <- apple$value * regionCityPopulation[match(apple$region, names(regionCityPopulation))] / sum(regionCityPopulation);
    bigCities <- aggregate(value_urbWt~date+category, apple[apple$region %in% names(cityToProvince),], sum);
    bigCities$region <- region;
    bigCities$cityRural <- 'bigcities';
    colnames(bigCities)[3] <- 'value';
    
    # TODO: need a special case for Ottawa-Gatineau, as it spans two provinces. Sigh.
    smallCitiesRuralPopulation <- parentRegionPopulation - sum(regionCityPopulation);
    
    bigCities$value_rurWt <- -sum(regionCityPopulation) / smallCitiesRuralPopulation;
    parentRegionDF$value_rurWt <- parentRegionPopulation / smallCitiesRuralPopulation;
    bigCitiesAndParent <- rbind(bigCities, parentRegionDF);
    bigCitiesAndParent$value_rurWt <- bigCitiesAndParent$value * bigCitiesAndParent$value_rurWt;
    
    smallCitiesRural <- aggregate(value_rurWt ~ date + category, bigCitiesAndParent, sum);
    smallCitiesRural$region <- region;
    smallCitiesRural$cityRural <- 'smallcitiesrural';
    colnames(smallCitiesRural)[3] <- 'value';
    bigCities$value_rurWt <- NULL;
    if(region!='Canada') {
      smallCitiesRural <- smallCitiesRural[smallCitiesRural$category=='driving',];
      # Throw away aggregated bigCities, keep individual cities; also throw away region-level total data.
      bigCitiesOrig <- data.frame(apple[apple$region %in% regionCityNames, c('date','category','value','region')]);
      bigCitiesOrig$cityRural <- 'bigcity';
      bigCities <- rbind(bigCities, bigCitiesOrig);
    } else {
      # region=='Canada'
      # Keep aggregated bigCities, rbind to smallCities. Throw away Canada (all) data.
    }
    result <- rbind(bigCities, smallCitiesRural);
  } else {
    smallCitiesRural <- parentRegionDF;
    smallCitiesRural$cityRural <- 'smallcitiesrural';
    result <- smallCitiesRural;
  }
  result$province <- region;
  result$cityRural <- fct_relevel(result$cityRural, c('bigcity', 'bigcities', 'smallcitiesrural'));
  # clone of getRolling, with cityRural instead of region
  result$value7 <- rep(NA, nrow(result));
  for(category in levels(result$category))
    for(region in levels(factor(result$region)))
      for(cityRural in levels(result$cityRural)) {
        filter <- result$category == category & result$cityRural == cityRural & result$region == region;
        result$value7[filter] <- rollapply(result[filter,]$value, 7, function(x) { mean(x, na.rm=TRUE) },
                                           fill=NA, align='center');
      }
  result;
}

appleCityRural <- rbind(
  extractCityRural(apple, 'Canada', names(cityToProvince)),
  extractCityRural(apple, 'BC', c('Vancouver')),
  extractCityRural(apple, 'Alberta', c('Edmonton','Calgary')),
  extractCityRural(apple, 'Ontario', c('Toronto', 'Ottawa')),
  extractCityRural(apple, 'Manitoba', c()),
  extractCityRural(apple, 'Saskatchewan', c()),
  extractCityRural(apple, 'Quebec', c('Montreal')),
  extractCityRural(apple, 'New Brunswick', c()),
  extractCityRural(apple, 'Nova Scotia', c('Halifax')),
  extractCityRural(apple, 'Newfoundland', c())
);
appleCityRural$province <- fct_relevel(appleCityRural$province, provinceOrder);
appleCityRural$cityRural <- fct_relevel(appleCityRural$cityRural, c('bigcity', 'bigcities', 'smallcitiesrural'));


########################################################################################
# Plotting

setupPlot <- function(p, startDate = '2020/03/01', isGoogle = TRUE, isDouble=FALSE, dateSpacing = '1 week', regionName=NULL,palette='Set1') {
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
    geom_hline(yintercept = 0, alpha=0.5) +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90));
  if(palette=='Set1') {
    result <- result +
      scale_color_brewer(palette=palette) +
      labs(x="", color="");
  }
  if (isBottom) {
    result <- result + labs(caption=ifelse(isDouble, ifelse(isGoogle, "Apple data rebaselined similar to Google. Rolling 7 day average.\ndrpritch.github.io/covid-mobility-canada", ""),
                     ifelse(isGoogle, "Rolling 7 day average. drpritch.github.io/covid-mobility-canada",
                            "Rebaselined similar to Google data. Rolling 7 day average. drpritch.githib.io/covid-mobility-canada")));
  }
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
names(category.labs2) = levels(google$category);
google$value7 <- getRolling(google);

setupPlot(
  ggplot(google, aes(y=value7, x=date)) +
    geom_line(aes(y=value7)) +
    facet_grid(rows=vars(category), cols=vars(region), scales = 'free_y', switch='y',
               labeller = labeller(region = region.labs, category=category.labs)),
  startDate = '2020/03/01',
  isGoogle = TRUE, dateSpacing = '2 weeks');
ggsave(filename = '../output/google_all.png', device = 'png', dpi='print',
       width=6.5, height=5.5, units='in', scale=1.5
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
      geom_point(aes(y=value), size=0.25, alpha=0.2) +
      geom_ribbon(data=google[regionFilter,],
                  aes(ymin=valueMin, ymax=value7_pos), fill=redFill, alpha=0.5, show.legend=FALSE) +
      geom_ribbon(data=google[regionFilter,],
                  aes(ymin=valueMin, ymax=value7_neg), fill=blueFill, alpha=0.5, show.legend=FALSE) +
      geom_line() +
      geom_text(aes(label=valueLabel), size=2, nudge_y = 5, color='#555555') +
      geom_label(aes(label = headlineLabel, y = -Inf), hjust='left', vjust='bottom',
                 size=ifelse(googleHeadlineTiny, 3, 5), parse=TRUE, label.size=0, fill='#ffffff00') +
      facet_wrap(~category, strip.position='top'),
    startDate = '2020/03/01',
    isGoogle = TRUE, isDouble = TRUE);
  ggsave(filename = paste0('../output/google_', regionFilename, '.png'), device = 'png', dpi='print',
         width=4, height=2.6, units='in', scale=1.5);
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

appleCityRural$region <- factor(appleCityRural$region);
appleCityRural$valueMin <- getMin(appleCityRural);
appleCityRural$value7_pos <- pmax(appleCityRural$value7, appleCityRural$valueMin);
appleCityRural$value7_neg <- pmin(appleCityRural$value7, appleCityRural$valueMin);
appleCityRural$valueLabel <- getValueLabel(appleCityRural);
appleCityRural$headlineLabel <- getHeadlineLabel(appleCityRural, startDate='2020/03/01');
appleCityRural$region_Rest <- as.character(appleCityRural$region);
filter <- appleCityRural$cityRural == 'smallcitiesrural' & appleCityRural$region %in% c('BC', 'Alberta', 'Ontario', 'Quebec', 'Nova Scotia');
appleCityRural$region_Rest[filter] <- paste0('Rest of ', appleCityRural$region[filter]);
appleCityRural$region_Rest <- factor(appleCityRural$region_Rest);
# Pull the cities to the start of the factor order, so that "Rest of" is the last row.
appleCityRural$region_Rest <- fct_relevel(appleCityRural$region_Rest,
    c('Vancouver', 'Edmonton', 'Calgary', 'Toronto', 'Ottawa', 'Montreal', 'Halifax'));
appleCityRural$region_Rest <- fct_recode(appleCityRural$region_Rest,
                            'Metro Vancouver'='Vancouver',
                            'Greater Edmonton'='Edmonton',
                            'Calgary Region'='Calgary',
                            'Greater Toronto'='Toronto',
                            'Ottawa-Gatineau'='Ottawa',
                            'Greater Montreal'='Montreal');
for (theProvince in levels(appleCityRural$province)) {
  provinceFilename <- tolower(gsub(' ','', theProvince));
  appleSubset <- subset(appleCityRural, province==theProvince & cityRural %in% c('bigcity', 'smallcitiesrural'));
  setupPlot(
    ggplot(appleSubset, aes(y=value7, x=date)) +
      geom_point(aes(y=value), size=0.25, alpha=0.2) +
      geom_ribbon(aes(ymin=valueMin, ymax=value7_pos), fill=redFill, alpha=0.5, show.legend=FALSE) +
      geom_ribbon(aes(ymin=valueMin, ymax=value7_neg), fill=blueFill, alpha=0.5, show.legend=FALSE) +
      ggtitle(paste0("Mobility in ", theProvince, " During Covid (as of ", format.Date(max(apple$date), "%b %d"), ")")) +
      geom_line() +
      geom_text(aes(label=valueLabel), size=2, nudge_y = 5, color='#555555') +
      geom_label(aes(label = headlineLabel, y = -Inf), hjust='left', vjust='bottom',
                 size=5, parse=TRUE, label.size=0, fill='#ffffff00') +
      facet_grid(rows=vars(region_Rest), cols=vars(category), switch='y'),
    startDate = '2020/03/01',
    isGoogle = FALSE, isDouble=TRUE);
  nregions <- length(levels(droplevels(appleSubset$region_Rest)))
  ncats <- length(levels(droplevels(appleSubset$category)))
  ggsave(filename = paste0('../output/apple_',provinceFilename,'.png'), device = 'png', dpi='print',
         width=ifelse(ncats==3,4,1.5), height=nregions*1.2 + 0.8, units='in', scale=1.5);
}


provinceGroupDef <- factor(c(
  BC='West', Alberta='West', Saskatchewan = 'West', Manitoba = 'West',
  Ontario='Central', Quebec='Central',
  'New Brunswick' = 'Maritimes', 'Nova Scotia' = 'Maritimes', Newfoundland = 'Maritimes',
  Canada='Canada'
));
provinceGroupDef <- fct_relevel(provinceGroupDef, c('West','Central','Maritimes','Canada'));
appleCityRural$region <- fct_relevel(appleCityRural$region, regionOrder);
provinceGroupColours <- brewer.pal(n=8, 'Set2')[1:4];
names(provinceGroupColours) <- levels(provinceGroupDef);
appleCityRural$provinceGroup <- provinceGroupDef[match(appleCityRural$region, names(provinceGroupDef))];
#appleCityRural$provinceGroupColour <- provinceGroupColours[appleCityRural$provinceGroup];
provinceColours <- provinceGroupColours[provinceGroupDef[match(levels(appleCityRural$province), names(provinceGroupDef))]];
names(provinceColours) <- levels(appleCityRural$province);

hsvMultValue <- function(c, vMult, hOffset = 0) {
  cHsv <- rgb2hsv(col2rgb(c));
  hsv((cHsv[1] + hOffset) %% 1, cHsv[2], cHsv[3] * vMult)
}
provinceColours['Alberta'] <- hsvMultValue(provinceColours['Alberta'], 0.8,)
provinceColours['Saskatchewan'] <- hsvMultValue(provinceColours['Saskatchewan'], 1.0, -0.1)
provinceColours['Manitoba'] <- hsvMultValue(provinceColours['Manitoba'], 0.8, -0.1)
provinceColours['Quebec'] <- hsvMultValue(provinceColours['Quebec'], 0.8)
provinceColours['Nova Scotia'] <- hsvMultValue(provinceColours['Nova Scotia'], 0.8)
provinceColours['Newfoundland'] <- hsvMultValue(provinceColours['Newfoundland'], 0.6)

cityRural.labs = c(bigcities='Major Cities', smallcitiesrural='Small Cities / Rural');
setupPlot(
  ggplot(subset(appleCityRural, category=='driving' & province != 'Canada' & cityRural %in% c('bigcities', 'smallcitiesrural')),
         aes(y=value7, x=date)) + geom_line(aes(color=province), size=1) +
#    geom_point(aes(y=value, color=region), size=0.25, alpha=0.2) +
    scale_color_manual(values=provinceColours) +
    #      geom_text(aes(label=valueLabel), size=2, nudge_y = 2, color='#555555') +
      facet_grid(row=vars(category), col=vars(cityRural), labeller = labeller(cityRural = cityRural.labs)),
  palette = '', isGoogle=FALSE
);
ggsave(filename = '../output/apple_cityRural.png', device = 'png', dpi='print',
       width=4.5, height=2, units='in', scale=1.5);
#ggplot(appleCityRural, aes(y=value7, x=date)) + geom_line(aes(color=cityRural)) + facet_grid(row=vars(category), col=vars(region))

appleCityRural_sub  <- subset(appleCityRural, category=='driving' & cityRural %in% c('bigcity','smallcitiesrural') & region != 'Canada');
appleCityRural_sub$valueB <- round(pmax(pmin(appleCityRural_sub$value,10), -60)/4)*4

# Sort regions by final week
foo <- subset(appleCityRural_sub, date == max(appleCityRural$date) - 3)[,c('region_Rest','value7')];
appleCityRural_sub$region_Rest <- droplevels(
  fct_relevel(appleCityRural_sub$region_Rest,
              as.character(foo$region_Rest[order(foo$value7, decreasing=T)])));

regionRestLevels <- levels(appleCityRural_sub$region_Rest);
ggplot(appleCityRural_sub,
  # as.numeric + scale_y_continuous: horrible hack to enable repeated y axis labels on right side, as per
  # https://stackoverflow.com/questions/45361904/duplicating-and-modifying-discrete-axis-in-ggplot2
  aes(y=as.numeric(region_Rest), x=date, fill=valueB)) +
  geom_tile(color='white', size=0.3) +
  scale_fill_distiller(palette='RdBu') +
  theme_minimal() +
  ggtitle('Driving Reductions in Canada during COVID-19') +
  scale_x_date(date_breaks = '1 week', date_labels='%b %d', expand=c(0,0),
               limits = c(as.Date('2020-03-02'), Sys.Date())) +
  scale_y_continuous(breaks = 1:length(regionRestLevels), labels=regionRestLevels, 
                     sec.axis = sec_axis(~., breaks=1:length(regionRestLevels), labels=regionRestLevels)) +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90),
        axis.title.y=element_blank()) +
  labs(fill='Reduction', caption='Source: Apple Mobility Report, rebaselined and with small city/rural estimates. drpritch.github.io/covid-mobility-canada')
ggsave(filename = '../output/apple_heatmap.png', device = 'png', dpi='print',
       width=8, height=2, units='in', scale=1.5);


