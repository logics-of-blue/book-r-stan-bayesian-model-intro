
# 状態空間モデルによる予測と補間｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 状態空間モデルの図示をする関数の読み込み
source("plotSSM.R", encoding="utf-8")

# データの読み込み
sales_df_all <- read.csv("5-2-1-sales-ts-1.csv")
sales_df_all$date <- as.POSIXct(sales_df_all$date)

# ローカルレベルモデルによる予測の実行 ----------------------------------------------------------------------

# データの準備
data_list_pred <- list(
  T = nrow(sales_df_all),
  y = sales_df_all$sales, 
  pred_term  = 20
)

# モデルの推定
local_level_pred <- stan(
  file = "5-3-1-local-level-pred.stan",
  data = data_list_pred,
  seed = 1
)

# 参考：収束の確認
mcmc_rhat(rhat(local_level_pred))

# 参考：結果の表示
print(local_level_pred,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))


## 図示

# 予測対象期間も含めた日付を用意
date_plot <- seq(
  from = as.POSIXct("2010-01-01"), 
  by = "days",
  len = 120)

# 参考
seq(from = as.POSIXct("2010-01-01"), 
    by = 60*60*24,
    len = 120)

# 生成された乱数を格納
mcmc_sample_pred <- rstan::extract(local_level_pred)

# 予測結果の図示
plotSSM(mcmc_sample = mcmc_sample_pred, 
        time_vec = date_plot, 
        state_name = "mu_pred", 
        graph_title = "予測の結果",
        y_label = "sales") 


# 欠損があるデータ ----------------------------------------------------------------

# データの読み込み
sales_df_NA <- read.csv("5-3-1-sales-ts-1-NA.csv")

# 日付をPOSIXct形式にする
sales_df_NA$date <- as.POSIXct(sales_df_NA$date)

# 売り上げデータに一部欠損がある
head(sales_df_NA, n = 3)


# 欠損データの取り扱い ------------------------------------------------------------------

# NAがある行を削除
sales_df_omit_NA <- na.omit(sales_df_NA)
head(sales_df_omit_NA, n = 3)

# データを取得した期間
nrow(sales_df_NA)

# 正しくデータが取得できた日数
nrow(sales_df_omit_NA)

# NAがどこにあるのかを判別
!is.na(sales_df_NA$sales)

# TRUEである要素番号の取得
which(c(TRUE, FALSE, TRUE))

# データがある行番号の取得
which(!is.na(sales_df_NA$sales))


# ローカルレベルモデルによる補間の実行 ------------------------------------------------------------------

# データの準備
data_list_interpolation <- list(
  T       = nrow(sales_df_NA),
  len_obs = nrow(sales_df_omit_NA),
  y       = sales_df_omit_NA$sales, 
  obs_no  = which(!is.na(sales_df_NA$sales))
)

# モデルの推定
local_level_interpolation <- stan(
  file = "5-3-2-local-level-interpolation.stan",
  data = data_list_interpolation,
  seed = 1,
  iter = 4000
)

# 参考：収束の確認
mcmc_rhat(rhat(local_level_interpolation))

# 参考：結果の表示
print(local_level_interpolation,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))


## 図示

# 生成された乱数を格納
mcmc_sample_interpolation <- rstan::extract(
  local_level_interpolation)

# 図示
plotSSM(mcmc_sample = mcmc_sample_interpolation, 
        time_vec = sales_df_all$date, 
        obs_vec = sales_df_all$sales,
        state_name = "mu", 
        graph_title = "補間の結果",
        y_label = "sales") 



# 参考：予測区間 -----------------------------------------------------------------


# モデルの推定
local_level_prediction_interval <- stan(
  file = "5-3-3-local-level-interpolation-prediction-interval.stan",
  data = data_list_interpolation,
  seed = 1,
  iter = 4000
)

# 参考：収束の確認
mcmc_rhat(rhat(local_level_prediction_interval))

# 参考：結果の表示
print(local_level_prediction_interval,
      pars = c("s_w", "s_v","lp__"),
      probs = c(0.025, 0.5, 0.975))


## 図示

# 生成された乱数を格納
mcmc_sample_prediction_interval <- rstan::extract(
  local_level_prediction_interval)

# 図示
plotSSM(mcmc_sample = mcmc_sample_prediction_interval, 
        time_vec = sales_df_all$date, 
        obs_vec = sales_df_all$sales,
        state_name = "y_pred", 
        graph_title = "補間の結果:予測分布",
        y_label = "sales") 



