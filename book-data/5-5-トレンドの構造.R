
# トレンドの構造｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 状態空間モデルの図示をする関数の読み込み
source("plotSSM.R", encoding="utf-8")


## データの読み込みと図示

# データの読み込み
sales_df_3 <- read.csv("5-5-1-sales-ts-3.csv")
sales_df_3$date <- as.POSIXct(sales_df_3$date)
head(sales_df_3, n = 3)

# 図示
autoplot(ts(sales_df_3[, -1]))


# ローカルレベルモデルの推定 -----------------------------------------------------------

# データの準備
data_list <- list(
  y = sales_df_3$sales, 
  T = nrow(sales_df_3)
)

# ローカルレベルモデルの推定
local_level <- stan(
  file = "5-2-1-local-level.stan",
  data = data_list,
  seed = 1
)

# ローカルレベルモデルの推定結果
print(local_level, 
      par = c("s_w", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))


# 平滑化トレンドモデルの推定 ----------------------------------------------------------------------


# 平滑化トレンドモデルの推定
smooth_trend <- stan(
  file = "5-5-1-smooth-trend.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6,
  control = list(adapt_delta = 0.9, max_treedepth = 15)
)

# 平滑化トレンドモデルの推定結果
print(smooth_trend, 
      par = c("s_z", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))



# ローカル線形トレンドモデルの推定 --------------------------------------------------------

# ローカル線形トレンドモデルの推定
local_linear_trend <- stan(
  file = "5-5-2-local-linear-trend.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)

# ローカル線形トレンドモデルの推定結果
print(local_linear_trend, 
      par = c("s_w", "s_z", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))



# 参考：収束の確認など
mcmc_rhat(rhat(local_level))
mcmc_rhat(rhat(smooth_trend))
mcmc_rhat(rhat(local_linear_trend))

check_hmc_diagnostics(local_level)
check_hmc_diagnostics(smooth_trend)
check_hmc_diagnostics(local_linear_trend)

# 参考：推定結果一覧
options(max.print=100000)
print(local_level, probs = c(0.025, 0.5, 0.975))
print(smooth_trend, probs = c(0.025, 0.5, 0.975))
print(local_linear_trend, probs = c(0.025, 0.5, 0.975))

# 参考：トレースプロット
mcmc_sample_1 <- rstan::extract(local_level, permuted = FALSE)
mcmc_sample_2 <- rstan::extract(smooth_trend, permuted = FALSE)
mcmc_sample_3 <- rstan::extract(local_linear_trend, permuted = FALSE)
mcmc_trace(mcmc_sample_1, pars = c("s_w", "s_v", "lp__"))
mcmc_trace(mcmc_sample_2, pars = c("s_z", "s_v", "lp__"))
mcmc_trace(mcmc_sample_3, pars = c("s_w", "s_z", "s_v", "lp__"))


# 推定された状態の図示 -----------------------------------------------------------------

# MCMCサンプルの取得
mcmc_sample_ll <- rstan::extract(local_level)
mcmc_sample_st <- rstan::extract(smooth_trend)
mcmc_sample_llt <- rstan::extract(local_linear_trend)

# ローカルレベルモデル
p_ll <- plotSSM(mcmc_sample = mcmc_sample_ll, 
        time_vec = sales_df_3$date,
        obs_vec = sales_df_3$sales,
        state_name = "mu", 
        graph_title = "ローカルレベルモデル", 
        y_label = "sales") 

# 平滑化トレンドモデル
p_st <- plotSSM(mcmc_sample = mcmc_sample_st, 
        time_vec = sales_df_3$date,
        obs_vec = sales_df_3$sales,
        state_name = "mu", 
        graph_title = "平滑化トレンドモデル", 
        y_label = "sales") 

# ローカル線形トレンドモデル
p_llt <- plotSSM(mcmc_sample = mcmc_sample_llt, 
                time_vec = sales_df_3$date,
                obs_vec = sales_df_3$sales,
                state_name = "mu", 
                graph_title = "ローカル線形トレンドモデル", 
                y_label = "sales") 

grid.arrange(p_ll, p_st, p_llt)


# ドリフト成分の図示
plotSSM(mcmc_sample = mcmc_sample_llt, 
        time_vec = sales_df_3$date,
        state_name = "delta", 
        graph_title = "ドリフト成分", 
        y_label = "delta") 



