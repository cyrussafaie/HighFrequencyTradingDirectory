#!/usr/bin/env Rscript
source("hft2_connection.R")


library(MASS)
load('hft2_probit_model.Rdata')  # load probit model
load('hft2_decomp_model.Rdata')  # load decomposition model

# global vars 
## !!! barrier should be changed for test assignment !!!
barrier <- 193900  # ES barrier multiplied by 100
tick <- 25         # ES tick is multiplied by 100 since prices are also multiplied by 100
position <- 0      # current position
# create buffers in advance:
trades_counter <- 0                                               # market trades event counter
orderexec_counter <- 0                                            # order execution event counter
BUF_SIZE <- 2000                                                  # we create buffers in advance:
trades_df <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),         # dataframe for ES and VX market trades
                        symbol=character(BUF_SIZE),               # 'ES' for s&p500 futures, 'VX' for vix futures
                        trade_price=as.integer(rep(NaN, BUF_SIZE)),
                        trade_size=as.integer(rep(NaN, BUF_SIZE)),
                        trade_side=factor(rep(NA, BUF_SIZE), levels = c('B','A')),  # bid or ask trade side
                        stringsAsFactors = F
                        )
executed_orders <- data.frame(time=.POSIXct(rep(NA, BUF_SIZE)),   # dataframe for executed orders
                              side=factor(rep(NA, BUF_SIZE), levels = c('BOUGHT','SOLD')),
                              quantity=as.integer(rep(NaN, BUF_SIZE)),
                              price=as.integer(rep(NaN, BUF_SIZE)),
                              forced=as.logical(rep(NA, BUF_SIZE))
                              )

start_time <- Sys.time()
plot_timestamp <- Sys.time()  # last plotting time, we use it to plot the graph once in a second

# we use local_df to store values used for predictions
probit_local_df <- data.frame(y1=factor('0',levels=m1$xlevels$y1),
                       y2=factor('0',levels=m1$xlevels$y2),
                       v1=1,
                       v2=1)
decomp_local_df <- data.frame(aim1 = 0, dim1 = 0, sim1 = 0)

last_es_price <- 0





## markettrade_handler is expected to have 4 arguments:
### 1) symbol('ES' or 'VX')
### 2) trade_price (integer vector of unit length)
### 3) trade_size (integer vector of unit length)
### 4) trade_side('A' or 'B')
## and to return 1 to buy@ask, -1 to sell@bid and 0 to do nothing.
markettrade_handler <- function(symbol, trade_price, trade_size, trade_side) {
    now <- Sys.time()
    # update trades_df dataframe:
    trades_counter <<- trades_counter + 1
    trades_df[trades_counter,] <<- list(now, symbol, trade_price, trade_size, trade_side)
    
    if (symbol == 'ES') {
        # reassemble probit_local_df dataframe
        pch <- (trade_price - last_es_price) / tick
        cut_pch <- min(1, max(-1, pch))  # cut too high jumps
        probit_local_df$y2 <<- probit_local_df$y1
        probit_local_df$y1 <<- factor(cut_pch, levels=m1$xlevels$y1)
        probit_local_df$v2 <<- probit_local_df$v1
        probit_local_df$v1 <<- trade_size
        # reassemble decomp_probit_local_df
        decomp_local_df$aim1 <<- ifelse(pch == 0, 0, 1)
        decomp_local_df$dim1 <<- sign(pch)
        decomp_local_df$sim1 <<- abs(pch)
        # cache last trade price
        last_es_price <<- trade_price
    }
    
    # now apply the model to probit_local_df:
    probit_predicted_prob <- predict(m1, probit_local_df, type = "p")
    
    # now calculate the signal (Note : it's up to you to change decision algorithm! )
    # default signal = 0 means nothing to do
    signal <- 0
    
    if ( position < 1 ) { # possible cross left to right
        # calc probability of barrier cross using probit model
        if (barrier < last_es_price - tick)
            probit_cross_prob <- 1
        else if (barrier == last_es_price - tick)
            probit_cross_prob <- probit_predicted_prob[2] + probit_predicted_prob[3]
        else if (barrier == last_es_price)
            probit_cross_prob <- probit_predicted_prob[3]
        else 
            probit_cross_prob <- 0
        
        # calc probability of barrier cross using decomposition model
        k <- (barrier - last_es_price) / tick
        decomp_cross_prob <- 1 - pch_decomposition_cdf(k, decomp_local_df$aim1, decomp_local_df$dim1, 
                                                       decomp_local_df$sim1, decomp_params)
        
        
        cross_prob <- (probit_cross_prob + decomp_cross_prob ) / 2  # example of voting function
        if (cross_prob > 0.5)
            signal <- +1
    }
    else {  # position >= 1, possible cross from right to left
        if (barrier > last_es_price + tick)
            probit_cross_prob <- 1
        else if (barrier == last_es_price + tick)
            probit_cross_prob <- probit_predicted_prob[1] + probit_predicted_prob[2]
        else if (barrier == last_es_price)
            probit_cross_prob <- probit_predicted_prob[1]
        else 
            probit_cross_prob <- 0
        
        # calc probability of barrier cross using decomposition model
        k <- (barrier - last_es_price) / tick
        decomp_cross_prob <- pch_decomposition_cdf(k-1, decomp_local_df$aim1, decomp_local_df$dim1, 
                                                   decomp_local_df$sim1, decomp_params)
        
        cross_prob <- (probit_cross_prob + decomp_cross_prob ) / 2  # example of voting function
        if (cross_prob > 0.7) # changed from 0.5 to 0.7:-275
            signal <- -1
    }
    
    if (signal != 0)
        message(now, " : ","Sending signal: ", signal)
    else
        Draw()  # drow only when signal==0 to send signal faster
    
    return(signal)
}


# orderexec_handler is expected to have 4 arguments:
## 1) side ('BOUGHT' or 'SOLD')
## 2) quantity (int vector of unit length), always positive
## 3) price (int vector of unit length)
## 4) forced (bool vector of unit length) - TRUE when the order is initiated by the system 
# and to return nothing
orderexec_handler <- function(side, quantity, price, forced) {
    now <- Sys.time()
    message(now, " : ", side, " @ ", price, " (forced=", forced, ")")
    # update executed_orders dataframe
    orderexec_counter <<- orderexec_counter + 1
    executed_orders[orderexec_counter,] <<- list(now, side, quantity, price, forced)
    # update current position
    if (side == 'BOUGHT')
        position <<- position + quantity
    else if (side == 'SOLD')
        position <<- position - quantity
    
    Draw() # plot once in a second
    
    return
}


# draw graph once in a second
Draw <- function()
{
    now <- Sys.time()
    if (difftime(now, plot_timestamp, unit="sec") >= 1) {
        plot_timestamp <<- now
        if (trades_counter > 0) {
            good_trades <- trades_df[1:trades_counter,]
            es_trades <- good_trades[good_trades$symbol == 'ES',]
            if (nrow(es_trades) > 0) {
                trades_time_values <- difftime(es_trades$time, start_time, units="sec")
                # trades:
                plot (x=trades_time_values, y=es_trades$trade_price, type='s', col='blue', 
                      ylim=c(min(es_trades$trade_price, barrier)-tick, max(es_trades$trade_price, barrier)+tick),
                      xlab="time (seconds)", ylab="price")
                # barrier line:
                abline(h=barrier, col='black') 
                if (orderexec_counter > 0) {
                    # executed orders:
                    exord_good_df <- executed_orders[1:orderexec_counter,]
                    client_buy_idx  <- (exord_good_df$side == 'BOUGHT') & (!exord_good_df$forced)
                    client_sell_idx <- (exord_good_df$side == 'SOLD') & (!exord_good_df$forced)
                    forced_buy_idx  <- (exord_good_df$side == 'BOUGHT') & (exord_good_df$forced)
                    forced_sell_idx <- (exord_good_df$side == 'SOLD') & (exord_good_df$forced)
                    tr_time_values <- difftime(exord_good_df$time, start_time, units="sec") 
                    points(x=tr_time_values[client_buy_idx], 
                           y=exord_good_df$price[client_buy_idx],
                           pch=19, col='green') # green circles
                    points(x=tr_time_values[client_sell_idx], 
                           y=exord_good_df$price[client_sell_idx],
                           pch=19, col='red')   # red circles
                    points(x=tr_time_values[forced_buy_idx], 
                           y=exord_good_df$price[forced_buy_idx],
                           pch=15, col='green') # green squares
                    points(x=tr_time_values[forced_sell_idx], 
                           y=exord_good_df$price[forced_sell_idx],
                           pch=15, col='red')   # red squares
                }
            }
        }
    }
}


# server options
host <- "datastream.ilykei.com"
port <- 30333
login <- 'Your email'
password <- 'Your streaming password'
stream_name <- "hft2"
catch_handler_errors <- TRUE  # we recommend using TRUE during the test and FALSE during homework
# make connection with your personal handlers
result <- Connect(host, port, login, password, stream_name, markettrade_handler, orderexec_handler, catch_handler_errors);


# remove empty values from buffers
trades_df <- trades_df[!is.na(trades_df$time),]
executed_orders <- executed_orders[!is.na(executed_orders$time),]

# after all you can dump your data/results and analyze it later
dump(c("trades_df", "executed_orders", "result"), file = "results.txt")

