# Name:    Jonah Landas
# Date:    2025-04-23
# Purpose: STAT 486 HW 10

data<- read.csv("gas_mileage_data.csv")

clean_data<- subset( data, !is.na( mpg ) & !is.na( ethanol ) )

clean_data$ethanol <- ifelse( clean_data$ethanol > 0,
                              "Yes",
                              "No" )

q1.1 <- nrow( clean_data )

q1.2 <- mean( clean_data$ethanol == "Yes" )

q1.3 <- mean( clean_data$mpg[ clean_data$ethanol == "Yes" ] )

q1.4 <- mean( clean_data$mpg[ clean_data$ethanol == "No" ] )

mod1 <- lm( mpg ~ ethanol,
            data = clean_data )

q1.5 <- coef( mod1 )

conf1 <- confint( mod1, level = 0.95 )
q1.6  <- conf1[ "ethanolYes", ]

#par( mfrow = c( 2, 2 ) )
#plot( mod1 )

mod2 <- lm( log( mpg ) ~ ethanol,
            data = clean_data )

q1.7 <- coef( mod2 )

conf2 <- confint( mod2, level = 0.95 )
q1.8  <- exp( conf2[ "ethanolYes", ] )

#par( mfrow = c( 2, 2 ) )
#plot( mod2 )
