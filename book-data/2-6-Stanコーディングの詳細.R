
# Stanコーディングの詳細｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 書籍のコードを実行するまでの前準備 -------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(bayesplot)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ読み込み
file_beer_sales_1 <- read.csv("2-4-1-beer-sales-1.csv")

# サンプルサイズ
sample_size <- nrow(file_beer_sales_1)

# listにまとめる
data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)

# 乱数の生成(3章と同じモデル)
mcmc_result_1 <- stan(
  file = "2-4-1-calc-mean-variance.stan",
  data = data_list,
  seed = 1
)

print(
  mcmc_result_1,
  probs = c(0.025, 0.5, 0.975)
)

# サンプリング文 -----------------------------------------------------------------

# 乱数の生成(正規分布に従う事前分布を指定)
mcmc_result_2 <- stan(
  file = "2-6-1-normal-prior.stan",
  data = data_list,
  seed = 1
)

print(
  mcmc_result_2,
  probs = c(0.025, 0.5, 0.975)
)


# 対数密度加算文 -----------------------------------------------------------------

# 乱数の生成(対数密度加算文の使用)
mcmc_result_3 <- stan(
  file = "2-6-2-lp.stan",
  data = data_list,
  seed = 1
)

print(
  mcmc_result_3,
  probs = c(0.025, 0.5, 0.975)
)


# 乱数の生成(対数密度加算文の使用、事前分布を正規分布にした)
mcmc_result_4 <- stan(
  file = "2-6-3-lp-normal-prior.stan",
  data = data_list,
  seed = 1
)

print(
  mcmc_result_4,
  probs = c(0.025, 0.5, 0.975)
)


# 乱数の生成(対数密度加算文の使用、ベクトル化)
mcmc_result_5 <- stan(
  file = "2-6-4-lp-normal-prior-vec.stan",
  data = data_list,
  seed = 1
)

print(
  mcmc_result_5,
  probs = c(0.025, 0.5, 0.975)
)


# 平均値の差の評価とgenerated quantitiesブロック ---------------------------------------

# 分析対象のデータ読み込み
file_beer_sales_ab <- read.csv("2-6-1-beer-sales-ab.csv")
head(file_beer_sales_ab, n = 3)

# ビールの種類別のヒストグラム
ggplot(data = file_beer_sales_ab, 
       mapping = aes(x = sales, y = ..density.., 
                     color = beer_name, fill = beer_name)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)


# ビールの種類別にデータを分ける
sales_a <- file_beer_sales_ab$sales[1:100]
sales_b <- file_beer_sales_ab$sales[101:200]

# listにまとめる
data_list_ab <- list(
  sales_a = sales_a,
  sales_b = sales_b,
  N = 100
)

# 乱数の生成
mcmc_result_6 <- stan(
  file = "2-6-5-difference-mean.stan",
  data = data_list_ab,
  seed = 1
)

print(
  mcmc_result_6,
  probs = c(0.025, 0.5, 0.975)
)


# 参考
mcmc_sample <- rstan::extract(mcmc_result_6, permuted = FALSE)
mcmc_dens(mcmc_sample, pars = "diff")

