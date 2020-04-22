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

doGoogle <- function() {
  # TODO: migrate to https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv
  # TODO: include caveats at https://www.google.com/covid19/mobility/data_documentation.html
  google <- read.csv('../input/google.csv');
  google <- google[google$Country=='CA',];
  google <- google[!google$location %in% c('Newfoundland and Labrador', 'Northwest Territories', 'Nunavut', 'Prince Edward Island', 'Yukon'),];
  locations <- google[google$category=='Workplace',]$location;
  google <- list(
    work = t(google[google$category=='Workplace',4:ncol(google)]),
    transit = t(google[google$category=='Transit stations',4:ncol(google)]),
    rec = t(google[google$category=='Retail & recreation',4:ncol(google)]),
    grocery = t(google[google$category=='Grocery & pharmacy',4:ncol(google)]),
    res = t(google[google$category=='Residential',4:ncol(google)]),
    park = t(google[google$category=='Parks',4:ncol(google)])
  )
  google <- lapply(google, function(cat) {
    colnames(cat) <- locations;
    cat <- xts(cat, order.by=as.Date('2020/02/29') + 0:(nrow(cat)-1));
    isweekday <- .indexwday(cat)%in% 1:5;
    isweekday[index(cat) == as.Date('2020/04/10')] <- FALSE;   # Good Friday
    cat <- list(weekdays = cat[isweekday,],
                weekends = cat[!isweekday,]);
    cat <- aggregateAndExtract(cat);
  });

  googleRestructure <- function(google, attr) {
    result <- restructure1(google, attr);
    result$work$category <- 'Workplace';
    result$transit$category <- 'Transit Stations';
    result$rec$category <- 'Retail & Recreation';
    result$park$category <- 'Park';
    result$grocery$category <- 'Grocery & Pharmacy';
    result$res$category <- 'Residential';
    result <- rbind(result$work, result$transit, result$rec, result$grocery, result$res, result$park);
    result$region <- factor(result$region);
    result$daytype <- factor(result$daytype);
    result$category <- factor(result$category);
  #  result$date <- factor(result$date);
    result$date <- as.Date(result$date);
    
    provinceOrder <-
      c('British Columbia', 'Alberta', 'Saskatchewan', 'Manitoba',
        'Ontario', 'Quebec',
    #    'Prince Edward Island',
        'New Brunswick', 'Nova Scotia'#, 'Newfoundland and Labrador',
    #    'Yukon', 'Northwest Territories', 'Nunavut'
      );
    categoryOrder <- c('Workplace','Transit Stations','Grocery & Pharmacy','Retail & Recreation','Residential','Park');
    result$region <- fct_relevel(result$region, provinceOrder);
    result$category <- fct_relevel(result$category, categoryOrder);
    result$category_daytype <- with(result, interaction(result$daytype, result$category));
    result$region_date <- with(result, interaction(result$date, result$region));
#    result$province_major <- factor(result$province %in% c('British Columbia', 'Alberta', 'Ontario', 'Quebec'),
#                                    levels=c(TRUE,FALSE), labels=c('big','small'));
#    result$province_west <- factor(result$province %in% c('British Columbia', 'Alberta', 'Saskatchewan', 'Manitoba'),
#                                   levels=c(TRUE,FALSE), labels=c('west','east'));
    result
  }
  deltas <- googleRestructure(google, 'deltas');
  mins <- googleRestructure(google, 'minimum');
  all <- googleRestructure(google, 'all');
  post <- googleRestructure(google, 'post');
  #print(round(xtabs(formula = values ~ region_date + category_daytype, all), 1))
  #print(round(xtabs(formula = values ~ region_date + category_daytype, post), 1))
  ggplot(all, aes(y=values, x=date)) +
    ggtitle("Google Community Mobility Index: Mar. 1 - Apr. 11") +
    geom_line(aes(color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales = 'free_y', switch='y') +
    theme_light() +
    scale_color_brewer(palette="Set1") +
    geom_hline(yintercept = 0, alpha=0.5) +
    theme(legend.position="bottom") +
    labs(y="Mobility Index", x="", color = "Day type", caption="Aggregated to Wed/Sat dates. Analysis by @drpritch2.") +
    theme(axis.text.x = element_text(angle = 90)) +
  ggsave(filename = '../output/google_all.png',
    device = 'png',
    width=6.5, height=6, units='in', scale=1.5,
    dpi='print'
  );
  #TODO: add point data too!
  
  ggplot(post, aes(y=values, x=date)) +
    ggtitle("Google Community Mobility Index: Mar. 22 - Apr. 11") +
    geom_line(aes(color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y') +
    coord_cartesian(xlim=c(as.Date("2020/03/22"), as.Date("2020/04/10"))) +
    scale_color_brewer(palette="Set1") +
    geom_hline(yintercept = 0, alpha=0.5) +
    theme_light() +
    theme(legend.position="bottom") +
    labs(y="Mobility Index", x="", color = "Day type", caption="Aggregated to Wed/Sat dates. Analysis by @drpritch2.") +
    theme(axis.text.x = element_text(angle = 90))
  # Deliberately narrower, to exaggerate slopes.
  ggsave(filename = '../output/google_post.png',
         device = 'png',
         width=3.5, height=6, units='in', scale=2.0,
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
    ggtitle("Apple Mobility Index: Feb. 2 - Apr. 20") +
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
    ggtitle("Apple Mobility Index: Mar. 22 - Apr. 20") +
    geom_line(aes(color=daytype)) +
    facet_grid(rows=vars(category), cols=vars(region), scales='free_y', switch='y') +
    coord_cartesian(xlim=c(as.Date("2020/03/22"), as.Date("2020/04/19"))) +
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
