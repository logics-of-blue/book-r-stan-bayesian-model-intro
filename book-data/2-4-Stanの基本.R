
# Stanの基本｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉



# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データの読み込み ----------------------------------------------------------------

# 分析対象のデータ
file_beer_sales_1 <- read.csv("2-4-1-beer-sales-1.csv")

# データの確認
head(file_beer_sales_1, n = 3)


# Stanに渡すためにデータを整形する ------------------------------------------------------

# サンプルサイズ
sample_size <- nrow(file_beer_sales_1)
sample_size

# listにまとめる
data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)
data_list


# MCMCによるサンプリングの実施 -----------------------------------------------------------------

# 乱数の生成
mcmc_result <- stan(
  file = "2-4-1-calc-mean-variance.stan", # stanファイル
  data = data_list,                       # 対象データ
  seed = 1,                               # 乱数の種
  chains = 4,                             # チェーン数
  iter = 2000,                            # 乱数生成の繰り返し数
  warmup = 1000,                          # バーンイン期間
  thin = 1                                # 間引き数(1なら間引き無し) 
)

# 結果の表示
print(
  mcmc_result,                   # MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)   # 中央値と95%信用区間を出力
)


# 収束の確認 -------------------------------------------------------------------

# トレースプロット(バーンイン期間無し)
traceplot(mcmc_result)

# トレースプロット(バーンイン期間あり)
traceplot(mcmc_result, inc_warmup = T)


# ベクトル化 -------------------------------------------------------------------
# 乱数の生成
mcmc_result_vec <- stan(
  file = "2-4-2-calc-mean-variance-vec.stan", # stanファイル(ここだけ変更した)
  data = data_list,                           # 対象データ
  seed = 1,                                   # 乱数の種
  chains = 4,                                 # チェーン数
  iter = 2000,                                # 乱数生成の繰り返し数
  warmup = 1000,                              # バーンイン期間
  thin = 1                                    # 間引き数(1なら間引き無し) 
)

# 結果の表示
print(
  mcmc_result_vec,               # MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)   # 事後分布の四分位点を出力
)













