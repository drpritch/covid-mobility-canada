library('xts');
library('ggplot2');
library('forcats');
library('matrixStats'); # for colMedians

aggregateAndExtract <- function(cat) {
  lapply(cat, function(catDays) {
    # Aggregate. Appears to handle Sat+Sun merging correctly for weekends.
    # Date attached is last day of the period (i.e., Friday or Sunday; Saturday for last weekend).
    catDays <- apply.weekly(catDays, colMeans);
    # Adjust date to Saturday of its week.
    isWeekdays <- .indexwday(catDays)[1] %in% 1:5;
    index(catDays) <- as.Date(index(catDays)) +
      ifelse(isWeekdays,
             #   5 (Friday) => -2
             3 - .indexwday(catDays),
             #   6 (Saturday) => +0
             #   0 (Sunday) => -1
             (6 - .indexwday(catDays) + 1) %% 7 - 1);
    # Find the "minimum" week and subsequent weeks
    # (Mar 23-27 + 28-29)
    # Also include preceding 7 days, to help with graphing - show the preceding region.
    # Don't just use all data for graphing - it'll distort the y bounds too much.
    postDays <- catDays[index(catDays)>=as.Date("2020-03-23") - 7,]
    
    # Line-by-line diff
    #    deltas <- diff(postDays);
    #    deltas[is.na(deltas)] <- 0;
    
    # Diff vs. base week
    baseDays <- postDays[1,];
    for(i in 2:nrow(postDays))
      baseDays <- rbind(baseDays, postDays[1,]);
    index(baseDays) <- index(postDays);
    deltas <- postDays - baseDays;
    # For first row, show value (minimum). For subsequent, show weekly deltas
    catDays <- list(all = as.data.frame(catDays),
                    post = postDays,
                    minimum = as.data.frame(postDays[1,]),
                    deltas = deltas);
  });
}

restructure1 <- function(dataset, attr) {
  lapply(dataset, function(cat) {
    fs <- lapply(cat, function(catDays) {
      f <- as.data.frame(catDays[[attr]]);
      result <- stack(f);
      result$date <- rep(rownames(f), ncol(f));
      colnames(result)[2] <- 'region';
      result;
    })
    fs$weekdays$daytype <- 'Weekday';
    fs$weekends$daytype <- 'Weekend/Holiday';
    rbind(fs$weekdays, fs$weekends);
  })
}

doGoogle <- function()
{
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
  google$daytype <- weekdays(google$date) %in% c('Saturday','Sunday');
  google$daytype <- factor(google$daytype, levels = c(FALSE, TRUE), labels = c('Weekday','Weekend/Holiday'));
  google$daytype[google$date == as.Date('2020/04/10')] <- 'Weekend/Holiday';   # Good Friday
  google$daytype <- factor(google$daytype);
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
  google$value7 <- NA;
  # TODO: must be some way to do this with a group by.
  for(daytype in levels(google$daytype))
    for(category in levels(google$category))
        for(region in levels(google$region)) {
          filter <- google$daytype == daytype & google$category == category & google$region == region;
          google[filter,]$value7 <- rollmean(google[filter,]$value, ifelse(daytype=='Weekday',5,2), fill=NA, align='right');
        }
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
}

doApple <- function() {
  apple <- read.csv('../input/apple.csv');
  regionOrder <-
    c('Canada', 'Vancouver', 'Edmonton', 'Calgary', 'Toronto', 'Ottawa', 'Montreal', 'Halifax');
  apple <- apple[apple$region %in% regionOrder,];

  regions <- apple[apple$transportation_type=='driving',]$region;
  apple <- list(
    driving = t(apple[apple$transportation_type=='driving',4:ncol(apple)]),
    transit = t(apple[apple$transportation_type=='transit',4:ncol(apple)]),
    walking = t(apple[apple$transportation_type=='walking',4:ncol(apple)])
  )
  apple <- lapply(apple, function(mode) {
    colnames(mode) <- regions;
    mode <- xts(mode, order.by=as.Date('2020/01/13') + 0:(nrow(mode)-1));
    baseline <- mode[index(mode) < as.Date('2020/02/10'),];
    mode <- mode[index(mode) >= as.Date('2020/02/09')];
    # Definition of row #1: "Sundays are generally this % of Mon Jan 13th"
    baselineDayOfWeek <- t(sapply(0:6, function(day) {
      # Factor to change baseline from row 1 (as shipped by Apple) to average of
      # equivalent weekday in period.
      baseline[1,] / colMedians(baseline[.indexwday(baseline) == day,])
    }));
    #print(baselineDayOfWeek);
    # Weekend-to-weekday ratio. About +15% for drive, +20% for transit, +10% for walk
    # Makes sense, for a trip planner app.
    #print(colMeans(baselineDayOfWeek[c(1,7),])/colMeans(baselineDayOfWeek[2:6,]));
    baselineDayOfWeek <- do.call("rbind", replicate(ceiling(nrow(mode)/7), baselineDayOfWeek, simplify=FALSE));
    stopifnot(.indexwday(mode)[1] == 0);  # algorithm assumes this
    baselineDayOfWeek <- baselineDayOfWeek[1:nrow(mode),]
    mode <- mode * baselineDayOfWeek - 100;
    isweekday <- .indexwday(mode)%in% 1:5;
    isweekday[index(mode) == as.Date('2020/04/10')] <- FALSE;   # Good Friday
    mode <- list(weekdays = mode[isweekday,],
                weekends = mode[!isweekday,]);
    mode <- aggregateAndExtract(mode);
  });
  
  
  appleRestructure <- function(apple, attr) {
    result <- restructure1(apple, attr)
    result$driving$category <- 'Driving';
    result$transit$category <- 'Transit';
    result$walking$category <- 'Walking';
    result <- rbind(result$driving, result$transit, result$walking);
    result$region <- factor(result$region);
    result$daytype <- factor(result$daytype);
    result$category <- factor(result$category);
    #  result$date <- factor(result$date);
    result$date <- as.Date(result$date);
    
    categoryOrder <- c('Driving','Transit','Walking');
    result$region <- fct_relevel(result$region, regionOrder);
    result$category <- fct_relevel(result$category, categoryOrder);
    result$category_daytype <- with(result, interaction(result$daytype, result$category));
    result$region_date <- with(result, interaction(result$date, result$region));
    result
  }
#  deltas <- appleRestructure(apple, 'deltas');
#  mins <- appleRestructure(apple, 'minimum');
  appleAll <- appleRestructure(apple, 'all');
  applePost <- appleRestructure(apple, 'post');
  
  ggplot(appleAll, aes(y=values, x=date)) +
    ggtitle("Apple Mobility Index: Feb. 2 - Apr. 22") +
    geom_line(aes(color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales = 'free_y', switch='y') +
    theme_light() +
    scale_color_brewer(palette="Set1") +
    geom_hline(yintercept = 0, alpha=0.5) +
    theme(legend.position="bottom") +
    labs(y="Mobility Index", x="", color = "Day type", caption="Rebaselined similar to Google Mobility Index. Aggregated to Wed/Sat dates. Analysis by @drpritch2.") +
    theme(axis.text.x = element_text(angle = 90)) +
  ggsave(filename = '../output/apple_all.png',
         device = 'png',
         width=6.5, height=4, units='in', scale=1.5,
         dpi='print'
  );
  ggplot(applePost, aes(y=values, x=date)) +
    ggtitle("Apple Mobility Index: Mar. 22 - Apr. 22") +
    geom_line(aes(color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y') +
    coord_cartesian(xlim=c(as.Date("2020/03/22"), Sys.Date() - 1)) +
    scale_color_brewer(palette="Set1") +
    geom_hline(yintercept = 0, alpha=0.5) +
    theme_light() +
    theme(legend.position="bottom") +
    labs(y="Mobility Index", x="", color = "Day type", caption="Rebaselined similar to Google Mobility Index. Aggregated to Wed/Sat dates. Analysis by @drpritch2.") +
    theme(axis.text.x = element_text(angle = 90))
  ggsave(filename = '../output/apple_post.png',
         device = 'png',
         width=3, height=4, units='in', scale=1.8,
         dpi='print'
  );
}

doGoogle();
doApple();
