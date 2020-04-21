library('xts');
library('ggplot2');
library('forcats');
google <- read.csv('../google-mobility-reports-data/csvs/international_local_area_trends_G20_20200417.csv');
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
  cat <- lapply(cat, function(catDays) {
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
});

googleRestructure <- function(google, attr) {
  result <- lapply(google, function(cat) {
     fs <- lapply(cat, function(catDays) {
       f <- as.data.frame(catDays[[attr]]);
       result <- stack(f);
       result$date <- rep(rownames(f), ncol(f));
       colnames(result)[2] <- 'province';
       result;
     })
     fs$weekdays$daytype <- 'Weekday';
     fs$weekends$daytype <- 'Weekend/Holiday';
     rbind(fs$weekdays, fs$weekends);
  })
  result$work$category <- 'Workplace';
  result$transit$category <- 'Transit';
  result$rec$category <- 'Retail & Recreation';
  result$park$category <- 'Park';
  result$grocery$category <- 'Grocery & Pharmacy';
  result$res$category <- 'Residential';
  result <- rbind(result$work, result$transit, result$rec, result$grocery, result$res, result$park);
  result$province <- factor(result$province);
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
  categoryOrder <- c('Workplace','Transit','Grocery & Pharmacy','Retail & Recreation','Residential','Park');
  result$province <- fct_relevel(result$province, provinceOrder);
  result$category <- fct_relevel(result$category, categoryOrder);
  result$category_daytype <- with(result, interaction(result$daytype, result$category));
  result$province_date <- with(result, interaction(result$date, result$province));
  result$province_major <- factor(result$province %in% c('British Columbia', 'Alberta', 'Ontario', 'Quebec'),
                                  levels=c(TRUE,FALSE), labels=c('big','small'));
  result$province_west <- factor(result$province %in% c('British Columbia', 'Alberta', 'Saskatchewan', 'Manitoba'),
                                 levels=c(TRUE,FALSE), labels=c('west','east'));
  result
}
deltas <- googleRestructure(google, 'deltas');
mins <- googleRestructure(google, 'minimum');
all <- googleRestructure(google, 'all');
post <- googleRestructure(google, 'post');
#print(round(xtabs(formula = values ~ province_date + category_daytype, mins), 1))
#print(round(xtabs(formula = values ~ province_date + category_daytype, deltas), 1))
#write.csv(xtabs(formula = values ~ province_date + category_daytype, mins), 'mins.csv')
#write.csv(xtabs(formula = values ~ province_date + category_daytype, deltas), 'deltas.csv')
#ggplot(all[all$category != 'park' & all$province_major=='big',], aes(y=values, x=date)) +
#  geom_line(aes(color=province)) +
#  facet_wrap(~category + daytype, nrow=4, ncol=2) +
#  scale_color_brewer(palette="Set1");
#ggplot(all[all$category != 'park' & all$province_major=='small',], aes(y=values, x=date)) +
#  geom_line(aes(color=province)) +
#  facet_wrap(~category + daytype, nrow=4, ncol=2) +
#  scale_color_brewer(palette="Set2");
ggplot(all, aes(y=values, x=date)) +
  ggtitle("Google Community Mobility Index: Mar. 1 - Apr. 11") +
  geom_line(aes(color=daytype)) +
  facet_grid(rows=vars(category), cols=vars(province), scales = 'free_y', switch='y') +
  theme_light() +
  scale_color_brewer(palette="Set1") +
  geom_hline(yintercept = 0, alpha=0.5) +
  theme(legend.position="bottom") +
  labs(y="Mobility Index", x="", color = "Day type", caption="Aggregated to Wed/Sat dates. Analysis by @drpritch2.") +
  theme(axis.text.x = element_text(angle = 90)) +
ggsave(filename = 'google_all.png',
  device = 'png',
  width=6.5, height=6, units='in', scale=1.5,
  dpi='print'
);

ggplot(post, aes(y=values, x=date)) +
  ggtitle("Google Community Mobility Index: Mar. 22 - Apr. 11") +
  geom_line(aes(color=daytype)) +
  facet_grid(rows=vars(category), cols=vars(province), scales='free_y', switch='y') +
  coord_cartesian(xlim=c(as.Date("2020/03/22"), as.Date("2020/04/10"))) +
  scale_color_brewer(palette="Set1") +
  geom_hline(yintercept = 0, alpha=0.5) +
  theme_light() +
  theme(legend.position="bottom") +
  labs(y="Mobility Index", x="", color = "Day type", caption="Aggregated to Wed/Sat dates. Analysis by @drpritch2.") +
  theme(axis.text.x = element_text(angle = 90))
# Deliberately narrower, to exaggerate slopes.
ggsave(filename = 'google_post.png',
       device = 'png',
       width=3.5, height=6, units='in', scale=1.6,
       dpi='print'
);


