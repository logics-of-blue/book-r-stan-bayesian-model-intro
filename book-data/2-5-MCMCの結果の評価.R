
# MCMCの結果の評価｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉

# パッケージの読み込み
library(rstan)
library(bayesplot)

library(ggfortify)


# MCMCの実行(ここまでは2部3章と同じコード) -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ
file_beer_sales_1 <- read.csv("2-4-1-beer-sales-1.csv")

# サンプルサイズ
sample_size <- nrow(file_beer_sales_1)

# listにまとめる
data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)

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


# MCMCサンプルの抽出 -------------------------------------------------------------

# MCMCサンプルの抽出
mcmc_sample <- rstan::extract(mcmc_result, permuted = FALSE)

## どんな中身か

# クラス
class(mcmc_sample)

# 次元数
dim(mcmc_sample)
# 各々の名称
dimnames(mcmc_sample)

# パラメタmuの1回目のチェーンのMCMCサンプルのburn-in後の最初のMCMCサンプル
mcmc_sample[1,"chain:1","mu"]

# パラメタmuの1回目のチェーンのMCMCサンプル
mcmc_sample[,"chain:1","mu"]

# パラメタmuの1回目のチェーンのMCMCサンプルの個数
length(mcmc_sample[,"chain:1","mu"])

# 4つのチェーンすべてのMCMCサンプルの個数
length(mcmc_sample[,,"mu"])

# 4つのチェーンがあるので、1000iter×4ChainのMatrix
dim(mcmc_sample[,,"mu"])
class(mcmc_sample[,,"mu"])


# MCMCサンプルの代表値の計算 ---------------------------------------------------------

# ベクトルにする
mu_mcmc_vec <- as.vector(mcmc_sample[,,"mu"])

# 事後中央値
median(mu_mcmc_vec)

# 事後期待値
mean(mu_mcmc_vec)

# 95%ベイズ信用区間
quantile(mu_mcmc_vec, probs = c(0.025, 0.975))

# 参考
print(
  mcmc_result,                   # MCMCサンプリングの結果
  probs = c(0.025, 0.5, 0.975)   # 事後分布の四分位点を出力
)


# トレースプロットの描画 -------------------------------------------------------------

# 参考：標準のトレースプロット
traceplot(mcmc_result, par = "mu")

# MCMCサンプルを使って、トレースプロットを描く
library(ggfortify)
autoplot(ts(mcmc_sample[,,"mu"]), 
         facets = F,  # 4つのChainをまとめて1つのグラフにする
         ylab = "mu", # y軸ラベル
         main = "トレースプロット")

# 事後分布の図示 ------------------------------------------------------

# データの整形
mu_df <- data.frame(
  mu_mcmc_sample = mu_mcmc_vec
)

# 図示
ggplot(data = mu_df, mapping = aes(x = mu_mcmc_sample)) +
  geom_density(size = 1.5)


# bayesplotを用いた事後分布の図示 ----------------------------------------------------

# ライブラリの読み込み
library(bayesplot)

# ヒストグラム
mcmc_hist(mcmc_sample, pars = c("mu", "sigma"))

# カーネル密度推定
mcmc_dens(mcmc_sample, pars = c("mu", "sigma"))


# bayesplotによるグラフの一覧表示 ----------------------------------------------------

# 参考：トレースプロット
mcmc_trace(mcmc_sample, pars = c("mu", "sigma"))

# 事後分布とトレースプロットをまとめて図示
mcmc_combo(mcmc_sample, pars = c("mu", "sigma"))


#	bayesplotで事後分布の範囲を比較する -------------------------------------------------------

# 事後分布の範囲を比較
mcmc_intervals(
  mcmc_sample, pars = c("mu", "sigma"), 
  prob = 0.8,        # 太い線の範囲
  prob_outer = 0.95  # 細い線の範囲
)

# 密度の情報も加える
mcmc_areas(mcmc_sample, pars = c("mu", "sigma"), 
           prob = 0.6,        # 薄い青色で塗られた範囲
           prob_outer = 0.99  # 細い線が描画される範囲
)

# bayesplotによるMCMCサンプルの評価 -----------------------------------------------------------------

# MCMCサンプルのコレログラム
mcmc_acf_bar(mcmc_sample, pars = c("mu", "sigma"))

# (参考)チェーン別の事後分布
mcmc_dens_overlay(mcmc_sample, pars = c("mu", "sigma"))

# (参考)チェーン別のヒストグラム
mcmc_hist_by_chain(mcmc_sample, pars = c("mu", "sigma"))



# 事後予測チェック：MCMCの実行 ----------------------------------------------------------------

# 分析対象のデータ
animal_num <- read.csv("2-5-1-animal-num.csv")
head(animal_num, n = 3)

# サンプルサイズ
sample_size <- nrow(animal_num)

# listにまとめる
data_list <- list(animal_num = animal_num$animal_num, N = sample_size)

# MCMCの実行：正規分布仮定のモデル
mcmc_normal <- stan(
  file = "2-5-1-normal-dist.stan",
  data = data_list,
  seed = 1
)

# MCMCの実行：ポアソン分布仮定のモデル
mcmc_poisson <- stan(
  file = "2-5-2-poisson-dist.stan",
  data = data_list,
  seed = 1
)

# 参考：推定されたパラメタ
print(mcmc_normal, par = c("mu", "sigma", "lp__"))
print(mcmc_poisson, par = c("lambda", "lp__"))


# 事後予測チェックの実施 -------------------------------------------------------------


# 事後予測値のMCMCサンプルの取得
y_rep_normal <- rstan::extract(mcmc_normal)$pred
y_rep_poisson <- rstan::extract(mcmc_poisson)$pred

# サンプルサイズ(nrow(animal_num))は200
# 4000回分のMCMCサンプル
dim(y_rep_normal)


# 事後予測値の1回目のMCMCサンプルを抽出
# 正規分布を仮定したモデル
y_rep_normal[1,]
# ポアソン分布を仮定したモデル
y_rep_poisson[1,]

# 参考；観測データの分布と、事後予測分布の比較
hist(animal_num$animal_num) # 観測データの分布
hist(y_rep_normal[1,])      # 正規分布を仮定した事後予測分布
hist(y_rep_poisson[1,])     # ポアソン分布を仮定した事後予測分布

# 元データのヒストグラムと、
# 1~5回分のMCMCサンプルの事後予測値のヒストグラム

# 正規分布を仮定したモデル
ppc_hist(y = animal_num$animal_num,  
         yrep = y_rep_normal[1:5, ])

# ポアソン分布を仮定したモデル
ppc_hist(y = animal_num$animal_num, 
         yrep = y_rep_poisson[1:5, ])


# ヒストグラムの代わりにカーネル密度推定を利用した結果

# 正規分布を仮定したモデル
ppc_dens(y = animal_num$animal_num, 
                 yrep = y_rep_normal[1:10, ]) 

# ポアソン分布を仮定したモデル
ppc_dens(y = animal_num$animal_num, 
                 yrep = y_rep_poisson[1:10, ])


# 正規分布を仮定したモデル
ppc_dens_overlay(y = animal_num$animal_num, 
                 yrep = y_rep_normal[1:10, ]) 

# ポアソン分布を仮定したモデル
ppc_dens_overlay(y = animal_num$animal_num, 
                 yrep = y_rep_poisson[1:10, ])




