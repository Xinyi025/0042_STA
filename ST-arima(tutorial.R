source("starima_package.R")

# Space-time Autocorrelation and partial autocorrelation analysis
data_mtx <- as.matrix(ts_data_multivar)
stacf(data_mtx, weights_norm, 24*3)

# 进行季节性差分
df_mtx.diff <- diff(train_data_mtx,lag=24,differences=1)

stacf(df_mtx.diff, weights_norm, 24*3)

# 查看stpacf图
stpacf(data_mtx, weights_norm, 24*3)
stpacf(df_mtx.diff,weights_norm,48)



# Step 3. Model Identification of STARIMA
W_fit<-list(w1=weights_norm)
fit.star <- starima_fit(data_mtx[1:600,],W_fit,p=2,d=24,q=6)
stacf(fit.star$RES, weights_norm,48)
hist(fit.star$RES[,6])


pre.star <- starima_pre(data_mtx[(600-24-8+1):744, ],model=fit.star)
matplot(1:144,cbind(data_mtx[601:744,1],pre.star$PRE[,1]),type="l")

# nrow(test_data_mtx[13:144,])  
# nrow(pre.star$PRE)

