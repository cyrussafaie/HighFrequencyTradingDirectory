# hft2_train_probit.R

# 'hft2_trades_train.csv' contains information about SnP500 trades for 1 day.
# It has 5 columns: "timestamp", "price", "size", "side" and "vix"
# 1) "timestamp" is a big number measured in microseconds (10^-6 seconds) at which trade has occured
# 2) "price" is a trade price of SnP500 futures multiplied by 100
# 3) "size" is a trade size
# 4) "side" is 'B' when the trade occured on bid and 'A' otherwise
# 5) "vix" is the value of VIX futures price multiplied by 100 at the time of SnP500 futures trade occurs

train_file <- 'hft2_trades_train.csv'
df <- read.csv(train_file, header=T)

# calculate seconds since market opening (timestamp is in microseconds (10^-6 seconds) )
df$seconds = (df$timestamp - df$timestamp[1]) / 10^6

head(df)

tick <- 25  # snp500 futures tick (multiplied by 100 since prices are also multiplied by 100)

# snp500 futures price change in ticks:
df$pch <- c(0, df$price[2:nrow(df)] - df$price[2:nrow(df) - 1] ) / tick
df$pch[df$pch > +1] <- +1  # force too high...
df$pch[df$pch < -1] <- -1  # ...and too low jumps
cf <- as.factor(df$pch)  # make factors from snp500 price changes

head(df)

range1 <- 3:length(cf)      # start from 3 since we use lag1 & lag2 predictors

y  <- cf[range1]            # we predict y - next price change in ticks
y1 <- cf[range1 - 1]        # lag1 y value as predictor
y2 <- cf[range1 - 2]        # lag2 y value as predictor
v1 <- df$size[range1 - 1]   # lag1 trade size as predictor
v2 <- df$size[range1 - 2]   # lag2 trade size as predictor

library(MASS)
m1 <- polr(y~y1+y2+v1+v2, method='probit')
summary(m1)


# now suppose we have such a situation where y1=0, y2=-1, v1=5, v2=3
# to make predictions we have to construct a dataframe with one observation
local_df <- data.frame(y1=factor( 0, levels = m1$xlevels$y1), 
                       y2=factor(-1, levels = m1$xlevels$y1), 
                       v1=5, 
                       v2=3)
# to predict next price movement we use general `predict` function
predicted_prob <- predict(m1, local_df, type = "p")
predicted_prob

# now save the model to use it in client handler
save(list="m1",file = "hft2_probit_model.Rdata")

