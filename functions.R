#NA functions

#LF trick  function
nudge_zero <- function(x){
  if(identical(x,0)){
    x <- 0.1
  }
  return(x)
}

#function to do NA conversion
zero_NA <- function(x){
  x[x == 0] <- NA
  x
}


# A function factory for getting integer y-axis values.
# from: https://joshuacook.netlify.com/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}


#function to find index that marks first sequence of length_use values.  Default length = 8 per Lloyd Provost 30 March 2020 
index_test <- function(x,
                       index,
                       length_use=8){
  x_check <- x[index:(index+length_use - 1)]
  if(all(x_check>0)){
    use_seq <- TRUE
    index_use <- index
  } else {
    use_seq <- FALSE
    index_use <- index + 1
  }
  return(list(use_seq,index_use))
}

detect_outlier_dates <- function(
  data,
  ratio_limit = 6)
{
  model <- loess(New_Deaths ~ as.numeric(datex), data = data, span = 0.25)
  

  outlier_index <- data$New_Deaths > max(10, median(data$New_Deaths, na.rm = TRUE)) &
                   (data$New_Deaths / model$fitted) >= ratio_limit
  
  data$datex[outlier_index]
}

force_monotonicity <- function(x) {
  
  sum_original <- sum(x, na.rm = TRUE)
  
  # spread negative values backwards in series so that cumulative sum never decreases
  negative_index <- which(x < 0)
  
  while (any(negative_index)) {
    
    index <- tail(negative_index, 1)
    
    value <- abs(x[index])
    
    transferred <- 0
    
    x[index] <- 0
    
    while (transferred < value) {
      
      index <- index - 1
      
      transfer <- min(x[index], value - transferred)
      
      x[index] <- x[index] - transfer
      
      transferred <- transferred + transfer
    }
    
    negative_index <- which(x < 0)
  }
  
  stopifnot(sum(x, na.rm = TRUE) == sum_original)
  
  x
}


model_phase_change <- function(
  data,
  y)
{
  data[[y]][!is.na(data[[y]]) & data[[y]] <= 0] <- NA
  
  data$log_10_deaths <- log10(data[[y]])
  
  data$log_10_deaths[is.infinite(data$log_10_deaths)] <- NA

  data$serial_day <- as.numeric(difftime(data$datex, min(data$datex, na.rm = TRUE), units = 'days')) + 1
  
  repeat {
    
    lm_raw <- try(lm(log_10_deaths ~ serial_day,
                     data = data))

    if ('try-error' %in% class(lm_raw)) break
    else {
      residual_diff <- diff(lm_raw$residuals)

      median_moving_range <- median(abs(residual_diff), na.rm=TRUE)

      data <- merge(data,
                           cbind(serial_day = lm_raw$model$serial_day,
                                 fitted_value = lm_raw$fitted.values),
                           by = 'serial_day',
                           all.x = TRUE)

      distance <- abs(data$log_10_deaths - data$fitted_value) / median_moving_range

      data$fitted_value <- NULL
      
      break
    }
  }
    
  if ('try-error' %in% class(lm_raw)) {
    
    output <- list(
      lm = NULL,
      exp_growth = FALSE,
      median_moving_range = NULL)
    
  } else {
    
    serial_day_coefficient <- lm_raw$coefficients['serial_day']

    growth_sign <- sign(serial_day_coefficient)
    
    serial_day_pvalue <- try(summary(lm_raw)$coefficients['serial_day', 'Pr(>|t|)'])
    
    exp_growth <- !is.na(serial_day_pvalue) &&
                  serial_day_pvalue < 0.05
    
    mean_deaths <- mean(data$log_10_deaths, na.rm = TRUE)
    
    # If exponential growth (up or down) is not detected, 
    # re-define median moving range based on mean_deaths as midline
    if (!exp_growth) {
      
      residuals_mean <- data$log_10_deaths - mean_deaths
      
      median_moving_range <- median(abs(residuals_mean), na.rm=TRUE)
      
    }
    
    output <- list(
      lm = lm_raw,
      mean_deaths = mean_deaths,
      growth_sign = growth_sign,
      exp_growth = exp_growth,
      median_moving_range = median_moving_range)
  }
  
  output
}

find_phase_dates <- function(
  data,
  adjust,
  ghost = TRUE,
  extend_days = 10)
{
  message(sprintf(' -- %s-level: finding phase dates for %s', data$level[1], data$state[1]))
  
  result <- try({
    
    if (adjust) {
      extend_days_unadjusted <- extend_days
      extend_days <- 0
    }
    
    data <- data[order(data$datex), ]
    
    # Remove negative deaths by iteratively reducing the counts (to floor of 0) 
    # on preceding days.
    data$New_Deaths <- force_monotonicity(data$New_Deaths)
    
    # Detect 'ghost' points - only run for raw series
    # Note ghost argument is TRUE by default, because 1) we want it TRUE for raw series, and
    # 2) for adjusted series, we want it TRUE for the first iteration of the function,
    # where the phases get estimated using the raw series. On the recursive call to find_phase_dates,
    # ghost will be set to FALSE since we don't want to apply ghosting to the adjusted series.
    if (ghost) {
      
      data$New_Deaths_Dump <- NA
      
      ghost_dates <- detect_outlier_dates(data[c('datex', 'New_Deaths')])
    
      ghost_index <- data$datex %in% ghost_dates
      
      if (any(ghost_index)) {
        
        data$New_Deaths_Dump[ghost_index] <- data$New_Deaths[ghost_index]
        data$New_Deaths[ghost_index]      <- NA
        
      }
    }
    
    phase_data <- list()
    
    if (any(data$New_Deaths > 0, na.rm = TRUE)) {
      
      date_phase_start <- min(data$datex[data$New_Deaths > 0], na.rm = TRUE)
      
      data_deaths <- data[data$datex >= date_phase_start, ]
      
      data_deaths$weekday <- lubridate::wday(data_deaths$datex)
      
      date_max <- max(data_deaths$datex, na.rm = TRUE)
       
      cumulative_deaths <- cumsum(ifelse(is.na(data_deaths$New_Deaths),
                                         0,
                                         data_deaths$New_Deaths))
      
      epoch <- 1
      
      # phase is the number of phases detected so far (always increasing)
      # epoch_phase is the number of phases detected in the current epoch
      # (increments on every phase change, except new epochs, when it resets to 1)
      phase <- 1
      epoch_phase <- 1
            
      if (any(cumulative_deaths >= 8)) {
        
        while (date_phase_start <= (date_max - 4)) {
          
          message(' -- Epoch ', epoch, ', phase ', phase, ' overall, phase ', epoch_phase, ' within epoch: ', date_phase_start)

          phase_index <- data_deaths$datex >= date_phase_start
          
          # need 8+ cumulative deaths in phase to estimate midline and ucl
          cumulative_deaths_phase <- cumsum(data_deaths$New_Deaths[phase_index])
          parameter_date <- data_deaths$datex[phase_index][which.max(cumulative_deaths_phase >= 8)]
          
          parameter_index <- data_deaths$datex >= date_phase_start & 
            data_deaths$datex <= max(date_phase_start + 20, parameter_date)
          
          test_index <- data_deaths$datex >= parameter_date
          
          if (epoch %in% c(1, 4)) {
            
            date_check_original <-
              date_check <- min(max(date_phase_start + 4, parameter_date), date_max)

            while (date_check <= date_max) {
              
              midline_index <- data_deaths$datex >= date_phase_start & data_deaths$datex <= min(date_check, date_phase_start + 20)
              
              midline <- mean(data_deaths$New_Deaths[midline_index], na.rm = TRUE)
              
              residuals <- data_deaths$New_Deaths - midline
              
              observed_values <- data_deaths$New_Deaths
              
              check_index <- data_deaths$datex >= date_check_original & data_deaths$datex <= date_check
              
              residuals_check <- (observed_values - midline)[check_index]
              
              mmr <- median(abs(diff(residuals_check)))
              
              distance_check <- abs(residuals_check) / mmr
              
              ucl <- midline + 3 * sqrt(midline)
              lcl <- midline - 3 * sqrt(midline)
              
              if (!is.na(lcl) && lcl <= 0) lcl <- NA
              
              # find two consecutive dates that values cross above ucl, take the earliest
              above_ucl <- observed_values[check_index] > ucl & !is.na(observed_values[check_index])
              above_ucl_streak <- ave(above_ucl, cumsum(!above_ucl), FUN = cumsum)
              
              # In phase 1, we only want the date the series first exceeds ucl
              # otherwise, we want the first day of two consecutive days exceeding the ucl
              if (epoch_phase == 1) above_ucl_streak_index <- head(which(above_ucl_streak == 1, 1))
              else above_ucl_streak_index <- head(which(above_ucl_streak == 2), 1) - 1
              
              date_above_ucl <- data_deaths$datex[check_index][above_ucl_streak_index]

              # find the first date that values are above or below midline for 8 days in a row
              above_midline <- observed_values[check_index] > midline & !is.na(observed_values[check_index])
              above_midline_streak <- ave(above_midline, cumsum(!above_midline), FUN = cumsum)
              date_above_midline <- data_deaths$datex[check_index][head(which(above_midline_streak == 8), 1)]

              below_midline <- observed_values[check_index] < midline & !is.na(observed_values[check_index])
              below_midline_streak <- ave(below_midline, cumsum(!below_midline), FUN = cumsum)
              date_below_midline <- data_deaths$datex[check_index][head(which(below_midline_streak == 8), 1)]
              
              date_phase_end <- min(date_above_ucl, date_above_midline, date_below_midline) - 1
              
              # if no phase end condition was detected, date_phase_end will be -Inf
              # if date_phase was detected (finite), break the loop
              if (is.finite(date_phase_end)) {

                break
                
              } else date_check <- date_check + 1
            }
            
            if (!is.finite(date_phase_end)) {              
              date_phase_end <- date_max + extend_days
            }
            
          } else if (epoch %in% c(2, 3)) {
            
            if (phase_change_result$exp_growth) {
              
              data_deaths$serial_day <- as.numeric(difftime(data_deaths$datex, date_phase_start, units = 'days')) + 1
  
              midline <- predict(phase_change_result$lm, data_deaths)
              
              data_deaths$serial_day <- NULL
            
            } else {
              
              midline <- phase_change_result$mean_deaths
              
            }
            
            date_check_original <-
              date_check <- min(max(date_phase_start + 21, parameter_date), date_max)
            
            mmr <- phase_change_result$median_moving_range
            
            observed_values <- log10(data_deaths$New_Deaths)
            
            while (date_check <= date_max) {
              
              ucl <- midline + (3.14 * mmr)
              lcl <- midline - (3.14 * mmr)
              
              # In epoch 3 with no exponential growth, midline is a constant 
              # (and so is mmr), so ucl and lcl are also length 1.
              # This causes problems when indexing by check_index, so for epoch 3, we expand the constant
              # out to a length equal to the number of rows in data_deaths. Not the best solution, but easier
              # than adding conditional logic in the streak checks though.
              if (epoch == 3 && length(midline == 1)) {
                ucl <- rep(ucl, nrow(data_deaths))
                lcl <- rep(lcl, nrow(data_deaths))
              }
              
              check_index <- data_deaths$datex >= date_check_original & data_deaths$datex <= date_check
              
              above_ucl <- observed_values[check_index] > ucl[check_index] & !is.na(observed_values[check_index])
              above_ucl_streak <- ave(above_ucl, cumsum(!above_ucl), FUN = cumsum)
              date_above_ucl <- data_deaths$datex[check_index][head(which(above_ucl_streak == 2), 1) - 1]
              
              below_ucl <- observed_values[check_index] < lcl[check_index] & !is.na(observed_values[check_index])
              below_ucl_streak <- ave(below_ucl, cumsum(!below_ucl), FUN = cumsum)
              date_below_ucl <- data_deaths$datex[check_index][head(which(below_ucl_streak == 2), 1) - 1]
              
              residuals_check <- (observed_values - midline)[check_index]
              
              streak_sign <- 0
              streak_length <- 0
            
              for (residual_i in seq_along(residuals_check)) {
                
                residual <- residuals_check[residual_i]
                
                if (is.finite(residual)) {
                  if (sign(residual) == streak_sign) {
                    streak_length <- streak_length + 1
                  } else {
                    streak_sign <- sign(residual)
                    streak_length <- 0
                  }
                }
                
                streak_found <- streak_length == 8 
                  
                if (streak_found) {
                  date_streak <- data_deaths$datex[check_index][residual_i]
                  break
                } else if (residual_i == length(residuals_check)) {
                  date_streak <- numeric(0)
                  break
                }
              }
              
              date_phase_end <- min(date_below_ucl, date_above_ucl, date_streak) - 1
              
              if (is.finite(date_phase_end)) {
                
                break
                
              } else date_check <- date_check + 1
            }
            
            if (!is.finite(date_phase_end)) {              
              
              date_phase_end <- date_max + extend_days
              
              if (extend_days > 0) {
                # extend midline and ucl/lcl
                data_extend <- data.frame(
                  datex = c(data_deaths$datex, date_max + seq_len(extend_days))) 
                  
                data_extend$serial_day <- as.numeric(difftime(data_extend$datex,
                                                              date_phase_start, 
                                                              units = 'days')) + 1
                
                # if exponential growth is detected (up or down),
                # extend the model prediction, otherwise just
                # extend the constant value to the extended date interval
                if (phase_change_result$exp_growth) {
                  midline <- predict(phase_change_result$lm, data_extend)
                } else if (epoch == 3) {
                  midline <- rep(midline[1], nrow(data_extend))
                }
                
                # need to extend phase_index too
                phase_index <- data_extend$datex >= date_phase_start
              }
            }
              
            phase_days <- as.numeric(difftime(date_phase_end, date_phase_start, units = 'day')) + 1
            
            if (length(midline) > 1) {
              midline <- head(midline[phase_index], phase_days)
            }
            
            ucl     <- midline + (3.14 * mmr)
            lcl     <- midline - (3.14 * mmr)
            
            midline  <- 10 ^ midline
            ucl      <- 10 ^ ucl
            lcl      <- 10 ^ lcl
          }
          
          phase_index <- data_deaths$datex >= date_phase_start & data_deaths$datex <= date_phase_end
          
          if (epoch %in% c(1, 4)) {
            
            observed_values_phase <- observed_values[phase_index]
            
          } else if (epoch %in% c(2, 3)) {
            
            observed_values_phase <- 10 ^ observed_values[phase_index]
            mmr <- 10 ^ mmr
            
          }
          
          residuals_phase <- (observed_values_phase - midline)
          
          distance_phase <- abs(residuals_phase) / mmr
          
          phase_parameters <- list(
            phase = phase,
            epoch = epoch,
            midline = midline,
            lcl = lcl,
            ucl = ucl,
            start = date_phase_start,
            end   = date_phase_end)
          
          phase_data[[phase]] <- phase_parameters

          if (date_phase_end < (date_max - 4)) {
            
            model_index <- data_deaths$datex > date_phase_end & data_deaths$datex <= min(date_phase_end + 21, date_max, na.rm = TRUE)
            
            phase_change_result <- model_phase_change(
              data = data_deaths[model_index, ],
              y = 'New_Deaths')
            
            phase <- phase + 1
            epoch_phase <- epoch_phase + 1
            
            if (phase_change_result$exp_growth && phase_change_result$growth_sign == 1) {
           
              if (epoch != 2) {
                epoch <- 2
                epoch_phase <- 1
              }
              
            } else {

              if (epoch == 2) {
                
                epoch <- 3
                epoch_phase <- 1
                
              } else if (epoch == 3) {
              
                # Epoch 4 requires:
                # 1. current phase LCL < 2, AND
                #   2a. phase change due to points below UCL, OR
                #   2b. phase change due to streak below midline (residual sign -1)
                # This excludes phase changes due to an uptick in cases.
                epoch_4 <- phase_parameters$lcl < 2 &&
                          (length(date_below_ucl) == 1 ||
                          (length(date_streak) == 1 && streak_sign == -1))
                
                if (epoch_4) {
                  epoch <- 4
                  epoch_phase <- 1
                }
              }
            }
          }
          
          date_phase_start <- date_phase_end + 1
        }
        
        # Add blank phase extending out 10 days from date_phase_max
        if (date_phase_start < date_max) {
          
          phase <- phase + 1
          
          date_phase_end <- date_max + extend_days
          
          phase_parameters[[phase]] <- list(
            phase = phase,
            epoch = epoch,
            midline = NA,
            lcl = NA,
            ucl = NA,
            start = date_phase_start,
            end   = date_phase_end)
          
        }
        
      } else {
        
        phase_data[[phase]] <- list(
          phase = phase,
          epoch = epoch,
          midline = NA,
          lcl = NA,
          ucl = NA,
          start = date_phase_start,
          end   = max(data_deaths$datex))
        
      }
    }
    
    if (extend_days > 0) {
      
      data_extend <- data.frame(
        level = data$level[1],
        state = data$state[1],
        datex = seq(date_max + 1, date_max + extend_days, by = 'day'),
        New_Deaths = NA,
        New_Deaths_max = NA,
        New_Deaths_Dump = NA,
        stringsAsFactors = FALSE)
    
      data <- rbind(data, data_extend)  
    }
    
    for (phase_parameters in phase_data) {
      
      index <- data$datex >= phase_parameters$start & data$datex <= phase_parameters$end
      
      data$phase[index]   <- phase_parameters$phase
      
      data$epoch[index]   <- phase_parameters$epoch
      data$midline[index] <- phase_parameters$midline
      data$lcl[index]     <- phase_parameters$lcl
      data$ucl[index]     <- phase_parameters$ucl
      data[[paste0('DatePhase', phase_parameters$phase)]] <- phase_parameters$start

      # Only adjust series if 1) requested by user,
      # 2) epoch is 2 or 3,
      # 3) at least 21 days of records were observed in this phase.
      # if (adjust && phase_parameters$epoch %in% c(2, 3) && sum(index) >= 21) {
      if (adjust && sum(index) >= 21) {
        
        if (phase_parameters$epoch %in% c(2, 3)) {
          residuals <- log10(data$New_Deaths) - log10(data$midline)
        } else {
          residuals <- data$New_Deaths - data$midline
        }

        data$weekday <- lubridate::wday(data$datex)
        
        data$residual_by_weekday[index] <- 
          ave(residuals[index],
              data$weekday[index],
              FUN = function(x) median(x, na.rm = TRUE))
        
        if (phase_parameters$epoch %in% c(2, 3)) {
          
          adjusted_deaths <- 10 ^ 
            (log10(data$New_Deaths[index]) - data$residual_by_weekday[index])
        } else {
          adjusted_deaths <- data$New_Deaths[index] - data$residual_by_weekday[index]
        }
        
        adjusted_deaths[is.na(data$New_Deaths_Dump[index]) & (!is.finite(adjusted_deaths) | adjusted_deaths < 0)] <- 0
        
        # normalize to actual total deaths in phase
        adjusted_deaths <- adjusted_deaths * (sum(data$New_Deaths[index], na.rm = TRUE) / sum(adjusted_deaths, na.rm = TRUE))
        
        data$New_Deaths[index] <- round(adjusted_deaths)
        
        data$residual_by_weekday <- NULL
      }
    }
    
    if (adjust) {
      
      data$New_Deaths[!is.finite(data$New_Deaths) & is.na(data$New_Deaths_Dump)] <- 0
      
      data <- find_phase_dates(data = data[c('level', 'state', 'datex', 'New_Deaths', 'New_Deaths_max', 'New_Deaths_Dump')],
                               adjust = FALSE,
                               ghost = FALSE,
                               extend_days = extend_days_unadjusted)
      
    }
    
    if (data$level[1] == 'state') {
      data$EPOCH_last <- tail(data$epoch[!is.na(data$epoch)], 1)
      
      data$EPOCH_last[is.infinite(data$EPOCH_last)] <- NA
    } else if (data$level[1] == 'country') {
      data$EPOCH <- data$epoch
    }
    
    data <- data[data$datex >= phase_data[[1]]$start, ]
  })
  
  if ('try-error' %in% class(result)) {
    #browser()
    message(' -- Error encountered for ', data$state[1])
  }

  data
}
  
  
  