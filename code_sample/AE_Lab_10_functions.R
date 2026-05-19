

##########################################################################################

regression_e1e2 <- function(nreps)
	{
	results <- data.frame(n = 0, dw = 0, t = 0, sig_t = 0)
	for(iter in 1:nreps) 
		{
			e1 <- rnorm(1000, mean = 0, sd = 1)
			e2 <- rnorm(1000, mean = 0, sd = 1)
			
			model1 <- lm(e1~e2)
			t <- summary(model1)$coefficients[2,3]
			sig_t <- summary(model1)$coefficients[2,4]
			sig_t <- ifelse(sig_t<0.05, 1, 0)
			
			dw <- dwtest(model1, alternative = "two.sided")
			dw <- dw$statistic
			names(dw) <- NULL
			
			results <- rbind(results, data.frame(n = iter, dw = dw, t = t, sig_t = sig_t))
			rm(e1, e2, model1, t, sig_t, dw)
		}
	 results <- results[results$n>0,]	
	 return(results)
	}


##########################################################################################

regression_yx <- function(nreps)
	{
	results <- data.frame(n = 0, dw = 0, t = 0, sig_t = 0)
	for(iter in 1:nreps) 
		{
			x <- array(0, 1000)
			y <- array(0, 1000)
			x[1] <- rnorm(1, mean = 0, sd = 1)
			y[1] <- rnorm(1, mean = 0, sd = 1)
			e1 <- rnorm(1000, mean = 0, sd = 1)
			e2 <- rnorm(1000, mean = 0, sd = 1)

			for(i in 2:1000)
				{ x[i] <- x[i-1] + e1[i]
				  y[i] <- y[i-1] + e2[i]
				}
			model2 <- lm(y~x)
			t <- summary(model2)$coefficients[2,3]
			sig_t <- summary(model2)$coefficients[2,4]
			sig_t <- ifelse(sig_t<=0.05, 1, 0)
			
			dw <- dwtest(model2, alternative = "two.sided")
			dw <- dw$statistic
			names(dw) <- NULL
			
			results <- rbind(results, data.frame(n = iter, dw = dw, t = t, sig_t = sig_t))
		  rm(e1, e2, model2, t, sig_t, dw)			
		}
	 results <- results[results$n>0,]	
	 return(results)
	}