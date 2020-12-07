library(httr)
library(aws.s3)

source('functions.R')

data_file_country <- paste0('data/country_data_', as.character(Sys.Date()), '.csv')
data_file_state   <- paste0('data/us_state_data_', as.character(Sys.Date()), '.csv')

defStartdate <- NA
defBuffer <- 7
#defBaseline is the default value of points to use to compute the exponential growth control limits
defBaseline <- 20

if (!file.exists(data_file_country)) {
  covid_data <- httr::GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                          authenticate(":", ":", type="ntlm"),
                          write_disk(data_file_country, overwrite=TRUE))
}

df_country <- read.csv(data_file_country,
                       header = TRUE,
                       stringsAsFactors = FALSE)

df_country$state          <- df_country$countriesAndTerritories
df_country$datex          <- as.Date(df_country$dateRep, format = '%d/%m/%Y')
df_country$New_Deaths     <- df_country$deaths
df_country$New_Deaths_max <- ave(df_country$New_Deaths, df_country$state, FUN = max)
df_country$level          <- 'country'

df_country <- df_country[c('level', 'state', 'datex', 'New_Deaths', 'New_Deaths_max')]

if (!file.exists(data_file_state)) {
  download.file(url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv',
                destfile = data_file_state)
}


df_state <- read.csv(data_file_state,
                     header = TRUE,
                     stringsAsFactors = FALSE)

df_state$datex <- as.Date(df_state$date)

df_state <- df_state[order(df_state$state, df_state$datex), ]
df_state$New_Deaths     <- ave(df_state$deaths, df_state$state, FUN = function(x) { c(x[1], diff(x)) })
df_state$New_Deaths_max <- ave(df_state$New_Deaths, df_state$state, FUN = max)
df_state$level          <- 'state'

df_state <- df_state[c('level', 'state', 'datex', 'New_Deaths', 'New_Deaths_max')]

df_all <- rbind(df_country, df_state)

# plot(df_all$datex, df_all$New_Deaths, type='l')

# find phase dates for both raw and adjusted data
df_all_raw <- by(
  data = df_all,
  INDICES = df_all[c('level', 'state')],
  FUN = find_phase_dates,
  adjust = FALSE)

df_all_raw <- do.call(dplyr::bind_rows, df_all_raw)

df_all_raw$type <- 'raw'

df_all_adjusted <- by(
  data = df_all,
  INDICES = df_all[c('level', 'state')],
  FUN = find_phase_dates,
  adjust = TRUE)


df_all_adjusted <- do.call(dplyr::bind_rows, df_all_adjusted)

df_all_adjusted$type <- 'adjusted'

df_all <- do.call(dplyr::bind_rows, list(df_all_raw, df_all_adjusted))



if (interactive()) {
  
  df_all_plot <- do.call(dplyr::bind_rows, list(df_all_raw, df_all_adjusted))
  
  #df_all_plot <- df_all_adjusted
  
  df_all_plot$lcl <- as.numeric(df_all_plot$lcl)
  df_all_plot$New_Deaths_Dump <- as.numeric(df_all_plot$New_Deaths_Dump)
  library(ggplot2)
  
  for (type in sort(unique(df_all_plot$type))) {
    
    df_type <- df_all_plot[df_all_plot$type == type, ]
  
    for (level in sort(unique(df_type$level))) {
    
      pdf(sprintf('samples/%s-%s.pdf', type, level), width = 11, height = 8.5)
    
        df_level <- df_type[df_type$level == level, ]
        
        for (state in sort(unique(df_level$state))) {
          
          df_state <- df_level[df_level$state == state, ]
          
          g <- ggplot(
            data = df_state,
            aes(
              x = datex,
              y = New_Deaths)) +
            geom_point() +
            geom_line() +
            geom_line(
              aes(
                y = midline),
              color = 'red') +
            geom_line(
              aes(
                y = ucl),
              color = 'blue',
              linetype = 'dashed') +
            geom_line(
              aes(
                y = lcl),
              color = 'blue',
              linetype = 'dashed') +
            geom_point(
              aes(
                y = New_Deaths_Dump),
              color = 'red') +
            scale_y_continuous(
              limits = c(0, max(20, 2 * max(df_state$New_Deaths, na.rm = TRUE), max(df_state$New_Deaths_Dump, na.rm = TRUE)))) +
            labs(
              x = df_state$state[1],
              y = paste(df_state$type[1], 'deaths'))
          
          print(g)
        }
        
      dev.off(which = dev.cur())
    }
  }
  }

##

df_split <- split(df_all,
                  f = df_all[c('type', 'level')])

df_split <- lapply(
  df_split,
  FUN = function(df) {

    df$MIDLINE_P1 <- ifelse(df$phase == 1,
                                df$midline,
                                NA)
    
    df$UPPER_P1 <- ifelse(df$phase == 1,
                              df$ucl,
                              NA)
    
    for (phase in setdiff(sort(unique(df$phase)), 1)) {
      
      df[[paste0('MIDLINE_', phase)]] <- ifelse(
        df$phase == phase,
        df$midline,
        NA)
    
      df[[paste0('UPPER_', phase)]] <- ifelse(
        df$phase == phase,
        df$ucl,
        NA)
      
      df[[paste0('LOWER_', phase)]] <- ifelse(
        df$phase == phase,
        df$lcl,
        NA)
      
    }
    
    df[c('level', 'type', 'phase', 'midline', 'lcl', 'ucl')] <- NULL
    
    df
  })

epoch_counts_by_date <- lapply(df_split,
  FUN = function(data) {
    
    n_states <- length(unique(data$state))
    
    # When phases end with less than 5 days remaining in the overall series,
    # we stop estimating phases. But this leaves NAs for epoch for the final few days,
    # which throws off the epoch counts by date. The next step looks at the data by
    # state, and if there are trailing NAs in epoch, converts them to the most recently
    # observed non-NA epoch value.
    data <- by(
      data = data,
      INDICES = data['state'],
      FUN = function(data_state) {
        
        na_epoch_index <- which(is.na(data_state$epoch))
        
        for (index in na_epoch_index) {
          
          if (index > 1) {
            
            data_state$epoch[index] <- data_state$epoch[index - 1]
          }
        }
        
        data_state
      })
    
    data <- do.call(rbind, data)
    
    results <- by(
      data = data,
      INDICES = data['datex'],
      FUN = function(data_date, n_states) {
        
        epoch <- data_date$epoch
        
        data.frame(
          datex = data_date$datex[1],
          EPOCH1 = n_states - sum(!is.na(epoch) & epoch > 1, na.rm = TRUE),
          EPOCH2 = sum(!is.na(epoch) & epoch == 2, na.rm = TRUE),
          EPOCH3 = sum(!is.na(epoch) & epoch == 3, na.rm = TRUE),
          EPOCH4 = sum(!is.na(epoch) & epoch == 4, na.rm = TRUE),
          stringsAsFactors = FALSE)
       },
      n_states = n_states)
    
    do.call(rbind, results)
  })

df_split <- lapply(df_split,
                   FUN = function(data) {
                     data$epoch <- NULL
                     data
                   })

write.csv(epoch_counts_by_date$adjusted.state,
          'output/Dates by Phase II.csv',
          row.names = FALSE,
          na = '')

write.csv(df_split$raw.state,
          'output/NYT Daily MultiPhase.csv',
          row.names = FALSE,
          na = '')

write.csv(df_split$adjusted.state,
          'output/NYT Daily MultiPhase ADJ.csv',
          row.names = FALSE,
          na = '')

write.csv(df_split$raw.country,
          'output/Country Daily MultiPhase.csv',
          row.names = FALSE,
          na = '')

write.csv(df_split$adjusted.country,
          'output/Country Daily MultiPhase ADJ.csv',
          row.names = FALSE,
          na = '')

access_key <- Sys.getenv('AWS_ACCESS_KEY_ID')
secret_key <- Sys.getenv('AWS_SECRET_ACCESS_KEY')
bucket     <- 'ihi-covid-output'

if (access_key != '' && secret_key != '') {

  output_files <- list.files(path = 'output',
                             pattern = '\\.csv$',
                             full.names = TRUE)
  
  for (output_file in output_files) {
    
    aws.s3::put_object(
      file = output_file,
      object = basename(output_file),
      bucket = bucket)
    
    filename_date <- sub('\\.csv', 
                         paste0(' - ', as.character(Sys.Date()), '.csv'),
                         basename(output_file))
    
    aws.s3::put_object(
      file = output_file,
      object = filename_date,
      bucket = bucket)
    
  }
}

