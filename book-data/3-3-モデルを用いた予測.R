
# モデルを用いた予測｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ
file_beer_sales_2 <- read.csv("3-2-1-beer-sales-2.csv")

# サンプルサイズ
sample_size <- nrow(file_beer_sales_2)


# 予測のためのデータの整理 ------------------------------------------------------------

# 気温を11度から30度まで変化させて、その時の売り上げを予測する
temperature_pred <-11:30
temperature_pred


# listにまとめる
data_list_pred <- list(
  N = sample_size,
  sales = file_beer_sales_2$sales, 
  temperature = file_beer_sales_2$temperature, 
  N_pred = length(temperature_pred),
  temperature_pred = temperature_pred
)


# MCMCの実行 -----------------------------------------------------------------

# MCMCの実行
mcmc_result_pred <- stan(
  file = "3-3-1-simple-lm-pred.stan",
  data = data_list_pred,
  seed = 1
)

# 結果の表示
print(mcmc_result_pred, probs = c(0.025, 0.5, 0.975))


# 予測分布の図示 -------------------------------------------------------------

# MCMCサンプルの抽出
mcmc_sample_pred <- rstan::extract(mcmc_result_pred, 
                                   permuted = FALSE)


# 気温が11度～30度まで1度ずつ変えたの時の
# 予測売り上げの95%予測区間の図示
mcmc_intervals(
  mcmc_sample_pred, 
  regex_pars = c("sales_pred."), # 正規表現を用いてパラメタ名を指定
  prob = 0.8,        # 太い線の範囲
  prob_outer = 0.95  # 細い線の範囲
)

# 95%区間の比較
mcmc_intervals(
  mcmc_sample_pred, 
  pars = c("mu_pred[1]", "sales_pred[1]"),
  prob = 0.8,        # 太い線の範囲
  prob_outer = 0.95  # 細い線の範囲
)


# 気温が11度と30度の時の、売り上げの予測分布
mcmc_areas(
  mcmc_sample_pred, 
  pars = c("sales_pred[1]", "sales_pred[20]"), 
  prob = 0.6,        # 薄い青色で塗られた範囲
  prob_outer = 0.99  # 細い線が描画される範囲
)





