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

pch <- c(0, df$price[2:nrow(df)] - df$price[2:nrow(df) - 1] ) / tick

idx <- c(1:length(pch))[pch > 0]
jdx <- c(1:length(pch))[pch < 0]

A <- rep(0, length(pch))
A[idx] <- 1
A[jdx] <- 1

range1 <- 2:length(pch)

# fit model for A
Ai <- A[range1]
Aim1 <- A[range1 - 1]

m1 <- glm(Ai~Aim1, family='binomial')
summary(m1)
beta_0 <- unname(m1$coefficients[1])
beta_1 <- unname(m1$coefficients[2])

plogis(beta_0)             # prob of Ai = 1 when Aim1 = 0
plogis(beta_0 + 1*beta_1)  # prob of Ai = 1 when Aim1 = 1


D <- rep(0, length(pch))
D[idx] <- 1
D[jdx] <- -1

Di <- D[range1]
Dim1 <- D[range1 - 1]

# fit model for D when Ai = 1
di <- Di[Ai==1]
dim1 <- Dim1[Ai==1]
di <- (di+abs(di)) / 2  # make 'di' binary

m2 <- glm(di~dim1, family="binomial")
summary(m2)
gamma_0 <- unname(m2$coefficients[1])
gamma_1 <- unname(m2$coefficients[2])
plogis(gamma_0 - 1*gamma_1)  # prob of Di = 1 when Ai = 1, Dim1 = -1
plogis(gamma_0)              # prob of Di = 1 when Ai = 1, Dim1 =  0
plogis(gamma_0 + 1*gamma_1)  # prob of Di = 1 when Ai = 1, Dim1 = +1



S <- abs(pch)

Si <- S[range1]
Sim1 <- S[range1 - 1]


# fit model for Si when Ai = 1, Di = +1
si <- Si[Di==1]
sim1 <- Sim1[Di==1]
source('GeoSize.R')
m3 <- GeoSize(si, sim1)

theta_u0 <- unname(m3$par[1])
theta_u1 <- unname(m3$par[2])
theta_u0
theta_u1
# si ~ 1 + Geom(lambda_up), lambda_up = plogis(theta_u0 + theta_u1 * Sim1)


# fit model for Si when Ai = 1, Di = -1
nsi <- Si[Di== -1 ]
nsim1 <- Sim1[Di== -1 ]
m4 <- GeoSize(nsi, nsim1)

theta_d0 <- unname(m4$par[1])
theta_d1 <- unname(m4$par[2])
theta_d0
theta_d1
# nsi ~ 1 + Geom(lambda_down), lambda_down = plogis(theta_d0 + theta_d1 * Sim1)



# Pr( next_pch <= x | aim1, dim1, sim1 )
pch_decomposition_cdf <- function(x, aim1, dim1, sim1, decomp_params) {  
  pch_cdf <- 0
  p <- plogis(decomp_params$beta_0 + decomp_params$beta_1 * aim1)    # Pr( Ai =  1 | aim1 )
  q <- plogis(decomp_params$gamma_0 + decomp_params$gamma_1 * dim1)  # Pr( Di = +1 | dim1 )
  
  lambda_up = plogis(decomp_params$theta_u0 + decomp_params$theta_u1 * sim1)
  lambda_down = plogis(decomp_params$theta_d0 + decomp_params$theta_d1 * sim1)
  
  if (x < 0) {
    # P( next_pch <= x ) = Pr( Ai = 1, Di = -1, Si >= -x ) = Pr( Ai = 1, Di = -1, Si > -x-1 ) 
    # since Si ~ 1 + geom(lambda_down) when Di = -1 we have:
    pch_cdf <- p * (1-q) * pgeom(-x-2, prob=lambda_down, lower.tail = FALSE) 
  } else if (x >= 0) {
    # P( next_pch <= x ) = Pr( Ai = 0 ) + Pr( Ai = 1, Di = 1 ) + Pr( Ai = 1, Di = -1, Si <= x ) = 
    # = (1-p) + p*(1-q) + Pr( Ai = 1, Di = 1, Si <= x ) 
    # since Si ~ 1 + geom(lambda_up) when Di = 1 we have:
    pch_cdf <- (1-p) + p * (1-q) + p * q * pgeom(x-1, prob=lambda_up)
  }
  
  return(pch_cdf)
}

# combine and save decomposition model fitted params
decomp_params <- list(beta_0 = beta_0, beta_1 = beta_1,
                      gamma_0 = gamma_0, gamma_1 = gamma_1,
                      theta_u0 = theta_u0, theta_u1 = theta_u1,
                      theta_d0 = theta_d0, theta_d1 = theta_d1)

save(list=c("decomp_params", "pch_decomposition_cdf"), file = "hft2_decomp_model.Rdata")