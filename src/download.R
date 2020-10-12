library('jsonlite');
source <- fromJSON(readLines('https://covid19-static.cdn-apple.com/covid19-mobility-data/current/v3/index.json', encoding = "UTF-8"));
# That's... one beautiful URL, Apple.
#'https://covid19-static.cdn-apple.com/covid19-mobility-data/2010HotfixDev19/v3/en-us/applemobilitytrends-2020-06-15.csv',

dir.create('../input');
download.file(paste0(
  'https://covid19-static.cdn-apple.com/', source$basePath, source$regions$`en-us`$csvPath),
  '../input/apple.csv');
download.file(
  'https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip',
  '../input/google.zip'
);
unzip('../input/google.zip', c('2020_CA_Region_Mobility_Report.csv'), exdir='../input');

