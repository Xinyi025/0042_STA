source("starima_package.R")

# Space-time Autocorrelation and partial autocorrelation analysis
train_data_mtx <- as.matrix(train_data)
stacf(train_data_mtx, weights_norm, 24*3)

# 进行季节性差分
df_mtx.diff <- diff(train_data_mtx,lag=24,differences=1)

stacf(df_mtx.diff, weights_norm, 24*3)

# 查看stpacf图
stpacf(train_data_mtx, weights_norm, 24*3)
stpacf(df_mtx.diff,weights_norm,48)


# Step 3. Model Identification of STARIMA
W_fit<-list(w1=weights_norm)
fit.star <- starima_fit(train_data_mtx,W_fit,p=5,d=1,q=6)
stacf(fit.star$RES, weights_norm,48)
hist(fit.star$RES[,6])

test_data_mtx <- as.matrix(test_data)
pre.star <- starima_pre(test_data_mtx,model=fit.star)
matplot(1:132,cbind(test_data_mtx[13:144,1],pre.star$PRE[,1]),type="l")

nrow(test_data_mtx[13:144,])  
nrow(pre.star$PRE)
