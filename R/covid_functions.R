# Function to distill the JHU database into a df for graphing
covid19 <- function(area = 'Countries', nDay = 7, topN = 10, addA = NULL) {

  suppressMessages(library(dplyr))

  df <- load_data()

  nmes <- check_area(area = area)

  if (nmes == 'US States') {

    tmp1 <- df[which(df$adm1 == 'US' & df$adm2 != ''), c(2, 4, 5)]

    names(tmp1)[1] <- 'adm1'

  } else {

    tmp1 <- df[which(df$adm1 != 'US States'), c(1, 4, 5)]

  }

  tmp2 <- aggregate(tmp1$case, by = list(tmp1$date), FUN = sum, na.rm = T)

  if (nmes == 'Countries') {tmp2$adm1 <- 'The World'} else {tmp2$adm1 <- 'USA'}

  tmp2 <- tmp2[, c(3, 1, 2)]

  temp <- rbind(analyze_new_v_cfrm(df = tmp1, nday = nDay, ncty = topN),
                analyze_new_v_cfrm(df = tmp2, nday = nDay, ncty = topN))

  if(!is.null(addA)) {

    tmp3 <- add_areas(addA = addA, df = df)

    tmp3 <- analyze_new_v_cfrm(df = tmp3, nday = nDay, ncty = topN)

    temp <- rbind(temp, tmp3)

  }

  temp$are2 <- area

  return(temp)

}

# Function to output the covid frames
graph_covid <- function(df = NULL, strD = NULL, dir = NULL) {

  suppressMessages(library(ggplot2))

  # Check for the save folder
  if (is.null(dir)) {stop('Please specify a valid folder for the frames')}

  # Add a backslash to the save directory if not given
  if (substr(dir, nchar(dir), nchar(dir)) != '/') {dir <- paste0(dir, '/')}

  area <- ifelse(unique(df$are2) == 'Countries', 'wrld', 'usa')

  # Specify the bounds of the graph
  xMax <- axis_max(max(df$case, na.rm = T))

  yMax <- axis_max(max(df$mean, na.rm = T))

  # Reduce the data frame to after the specified date
  if (!is.null(strD)) {

    strD <- as.POSIXct(strD, '%Y-%m-%d', tz = 'America/Los_Angeles')

    df <- df[which(df$date >= strD), ]

  }

  # Start graphing
  dtes <- unique(df$date)

  for (i in 1 : length(dtes)) {

    temp <- df[which(df$date <= dtes[i]), ]

    xRow <- temp[last_row(temp), ]

    dteL <- data.frame(case = 1, mean = 0.75 * yMax, area = 'none',
                       date = dtes[i], stringsAsFactors = F)

    pl <- ggplot(data = temp, aes(x = case, y = mean, group = area)) +
          geom_line() + scale_x_log10(limits = c(1, xMax), labels = plain) +
          scale_y_log10(limits = c(1, yMax), labels = plain) +
          geom_point(data = xRow, aes(x = case, y = mean), size = 1.5) +
          theme_bw() + xlab('Total Confirmed Cases') + ylab('New Cases') +
          theme(legend.position = 'none') +
          geom_text(data = dteL, aes(label = date), hjust = 0, vjust = 1,
                    size = 8) +
          geom_text(data = xRow, aes(label = area), hjust = 0, vjust = 1,
                    nudge_x = 0.05)

    fNme <- paste0(dir, area, '_', format(dtes[i], '%Y%m%d'), '.png')

    ggsave(filename = fNme, plot = pl, width = 11, height = 8.5, units = 'in',
           dpi = 300)

  }

  return(cat('Your graphs are finished\n'))

}

# Function to process additional regions (states or countries)
add_areas <- function(addA = NULL, df = NULL) {

  stts <- df[which(df$adm1 == 'US' & df$adm2 != ''), c(1, 2, 4, 5)]

  addS <- stts[which(stts$adm2 %in% addA), 2 : 4]

  wrld <- df[which(df$adm1 != 'US States'), c(1, 2, 4, 5)]

  addC <- wrld[which(wrld$adm1 %in% addA), c(1, 3, 4)]

  if (nrow(addS) == 0 & nrow(addC) == 0) {

    stop(paste0('None of the regions you have entered are in the JHU list of\n',
                'US states or coutries of the world. Please check your names.\n'))

  } else {

    notA <- addA[which(!addA %in% stts$adm2 & !addA %in% wrld$adm1)]

    verb <- ifelse(length(notA) == 1, 'is', 'are')

    if (length(notA) != 0) {

      cat(paste0('Sorry--', paste0(notA, collapse = ', '), '--', verb,
                 ' not found in the list of countries or US states.\n',
                 'Please check your list and try again.\n'))

    }
  }

  names(addS) <- names(addC) <- c('adm1', 'date', 'case')

  addA <- rbind(addC, addS)

  return(addA)

}

# Function to process the COCID data for figures
analyze_new_v_cfrm <- function(df = NULL, nday = 7, ncty = 10) {
  names(df) <- c('area', 'date', 'case')
  df <- aggregate(df$case, by = list(df$area, df$date), FUN = sum, na.rm = T)
  names(df) <- c('area', 'date', 'case')
  dX <- aggregate(df$case, by = list(df$area), 'max', na.rm = T)
  dX <- dX %>% arrange(desc(x))
  df <- df[which(df$area %in% dX$Group.1[1 : ncty]), ]
  df <- new_cases(df = df)
  df$mean <- 0
  df$newC[which(df$newC == 0)] <- NA
  for (i in 1 : length(unique(df$area))) {
    cond <- which(df$area == unique(df$area)[i])
    df$mean[cond] <- rolling_average(x = df$newC[cond], nday = nday)
  }
  for (i in 3 : 5) {df[which(df[, i] == 0), i] <- NA}
  return(df)
}

# Function to calculate new cases per day
new_cases <- function(df = NULL) {
  names(df) <- c('area', 'date', 'case')
  df <- df %>% arrange(area, date)
  df$newC <- 0; area <- unique(df$area)
  for (i in 1 : length(area)) {
    temp <- df[which(df$area == area[i]), ]
    for (j in 2 : nrow(temp)) {
      temp$newC[j] <- temp$case[j] - temp$case[j - 1]
    }
    df$newC[which(df$area == area[i])] <- temp$newC
  }
  return(df)
}

# Function to check the name of 'area'
check_area <- function(area = NULL) {
  area <- tolower(area)
  usa <- c('state', 'us'); cty <- c('world', 'count'); t1 = 0; t2 = 0
  for(i in 1 : length(usa)) {if (grepl(usa[i], area)) area <- 'US States'}
  for(i in 1 : length(cty)) {if (grepl(cty[i], area)) area <- 'Countries'}
  if (area != 'US States' & area != 'Countries') {
    stop('Please specify a valid area (i.e., Countries or US states)')
  }
  return(area)
}

# Function to load and pre-process COVID data
load_data <- function() {
  path <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
                 'master/csse_covid_19_data/csse_covid_19_time_series/')
  cfrU <- read.csv(paste0(path, 'time_series_covid19_confirmed_US.csv'),
                   stringsAsFactors = F)
  cfrW <- read.csv(paste0(path, 'time_series_covid19_confirmed_global.csv'),
                   stringsAsFactors = F)
  cfrU <- cfrU[, c(6, 7, 8, 12 : length(cfrU))]
  names(cfrU)[1 : 3] <- c('adm3', 'adm2', 'adm1')
  cfrW$adm3 <- ''; cfrW <- cfrW[, c(length(cfrW), 1, 2, 5 : (length(cfrW) - 1))]
  names(cfrW)[1 : 3] <- c('adm3', 'adm2', 'adm1')
  cfrm <- rbind(cfrW, cfrU)
  cfrm <- reshape2::melt(cfrm, id.vars = paste0('adm', 1 : 3),
                         variable.name = 'date', value.name = 'case')
  cfrm$date <- covid_dates(x = cfrm$date)
  return(cfrm)
}

# Function to convert dates to POSIX
covid_dates <- function(x = NULL) {
  x <- data.frame(date = gsub('X', '', x))
  prLc <- data.frame(do.call('rbind', gregexpr('\\.', x$date)),
                     stringsAsFactors = F)
  yr <- as.numeric(apply(X = x, MARGIN = 2, FUN = 'substr', prLc$X2 + 1,
                         nchar(x))) + 2000
  mn <- as.numeric(apply(X = x, MARGIN = 2, FUN = 'substr', 1, prLc$X1 - 1))
  dy <- as.numeric(apply(X = x, MARGIN = 2, FUN = 'substr', prLc$X1 + 1, prLc$X2 - 1))
  x <- as.POSIXct(paste0(yr, '-', mn, '-', dy), '%Y-%m-%d', tz = 'America/Los_Angeles')
  return(x)
}

# Function to calculate a rolling average of N days
rolling_average <- function(x = NULL, nday = NULL) {
  if (length(x) <= nday) {
    stop('Your vector is less than the number of averaging days')
  } else {
    y <- rep(0, length(x))
    for (n in 1 : length(x)) {
      if (n < nday) {
        y[n] <- mean(x[1 : n], na.rm = T)
      } else {
        y[n] <- mean(x[(n - (nday - 1)) : n], na.rm = T)
      }
    }
  }
  return(y)
}

# Function to extract the last row of each 'area' (for labels on graphs)
last_row <- function(df = NULL) {
  df <- df %>% arrange(area, date)
  lRow <- NULL
  for (i in 1 : nrow(df)) {
    if (i != nrow(df)) {
      if (df$area[i + 1] != df$area[i]) {lRow <- append(lRow, i)}
    } else {
      lRow <- append(lRow, i)
    }
  }
  return(lRow)
}

# Function to format dates from scientific to plain
plain <- function(x) {format(x, scientific = FALSE, trim = TRUE)}

# Function to output the maximum values of the x and y axis
axis_max <- function(x = NULL) {

  xLog <- as.integer(log10(x))

  lDgt <- as.integer(x / 10^xLog) + 1

  xMax <- lDgt * 10^xLog

  return(xMax)
}

