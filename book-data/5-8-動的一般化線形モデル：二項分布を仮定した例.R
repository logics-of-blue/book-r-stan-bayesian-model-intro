
# 動的一般化線形モデル：二項分布を仮定した例｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(bayesplot)
library(KFAS)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 状態空間モデルの図示をする関数の読み込み
source("plotSSM.R", encoding="utf-8")


# データの読み込み
data("boat")
boat


# 二項分布を仮定したDGLMの推定 --------------------------------------------------------

# 参考
!is.na(boat)         # データがあればTRUE
which(!is.na(boat))  # データがある時点一覧

# NAを除く
boat_omit_NA <- na.omit(as.numeric(boat))

# データの準備
data_list <- list(
  T       = length(boat),
  len_obs = length(boat_omit_NA),
  y       = boat_omit_NA, 
  obs_no  = which(!is.na(boat))
)

# モデルの推定
dglm_binom <- stan(
  file = "5-8-1-dglm-binom.stan",
  data = data_list,
  seed = 1,
  iter = 30000,
  warmup = 10000,
  thin = 20
)

# 推定されたパラメタ
print(dglm_binom, 
      par =  c("s_w", "lp__"),
      probs = c(0.025, 0.5, 0.975))


# 参考：収束の確認
mcmc_rhat(rhat(dglm_binom))
check_hmc_diagnostics(dglm_binom)

# 参考:トレースプロット
mcmc_sample <- rstan::extract(dglm_binom, permuted = FALSE)
mcmc_trace(mcmc_sample, pars = c("s_w", "lp__"))

# 参考：推定結果一覧
options(max.print=100000)
print(dglm_binom, probs = c(0.025, 0.5, 0.975))

# 推定された状態の図示 -----------------------------------------------------------------

# 時間ラベルの作成
years <- seq(from = as.POSIXct("1829-01-01"), 
             by = "1 year", 
             len = length(boat))
head(years, n = 3)

# MCMCサンプルの取得
mcmc_sample <- rstan::extract(dglm_binom)

# ケンブリッジ大学の勝率の推移のグラフ
plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = years,
        obs_vec = as.numeric(boat),
        state_name = "probs", 
        graph_title = "ケンブリッジ大学の勝率の推移", 
        y_label = "勝率",
        date_labels = "%Y年") 


# ケンブリッジ大学の平均勝率
mean(boat_omit_NA)


