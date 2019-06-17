
# 周期性のモデル化｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
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
sales_df_4 <- read.csv("5-6-1-sales-ts-4.csv")
sales_df_4$date <- as.POSIXct(sales_df_4$date)
head(sales_df_4, n = 3)

# 図示
autoplot(ts(sales_df_4[, -1]))


# 基本構造時系列モデルの推定 --------------------------------------------------------

# データの準備
data_list <- list(
  y = sales_df_4$sales, 
  T = nrow(sales_df_4)
)

# 基本構造時系列モデルの推定
basic_structual <- stan(
  file = "5-6-1-basic-structual-time-series.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6,
  control = list(adapt_delta = 0.97, max_treedepth = 15)
)

# 基本構造時系列モデルの推定結果
print(basic_structual, 
      par = c("s_z", "s_s", "s_v", "lp__"),
      probs = c(0.025, 0.5, 0.975))


# 参考：収束の確認
mcmc_rhat(rhat(basic_structual))
check_hmc_diagnostics(basic_structual)

# 参考:トレースプロット
mcmc_sample <- rstan::extract(basic_structual, permuted = FALSE)
mcmc_trace(mcmc_sample, pars = c("s_z", "s_s", "s_v", "lp__"))

# 参考：推定結果一覧
options(max.print=100000)
print(basic_structual, probs = c(0.025, 0.5, 0.975))


# 推定結果の図示 -----------------------------------------------------------------

# MCMCサンプルの取得
mcmc_sample <- rstan::extract(basic_structual)

# すべての成分を含んだ状態推定値の図示
p_all <- plotSSM(mcmc_sample = mcmc_sample, 
                 time_vec = sales_df_4$date,
                 obs_vec = sales_df_4$sales,
                 state_name = "alpha", 
                 graph_title = "すべての成分を含んだ状態推定値", 
                 y_label = "sales") 

# 周期成分を除いた状態推定値の図示
p_trend <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = sales_df_4$date,
        obs_vec = sales_df_4$sales,
        state_name = "mu", 
        graph_title = "周期成分を除いた状態推定値", 
        y_label = "sales") 

# 周期成分の図示
p_cycle <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = sales_df_4$date,
        state_name = "gamma", 
        graph_title = "周期成分", 
        y_label = "gamma") 

grid.arrange(p_all, p_trend, p_cycle)

