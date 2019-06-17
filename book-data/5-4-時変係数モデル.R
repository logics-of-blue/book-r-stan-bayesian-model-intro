
# 時変係数モデル｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(brms)
library(bayesplot)
library(ggfortify)
library(gridExtra)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 状態空間モデルの図示をする関数の読み込み
source("plotSSM.R", encoding="utf-8")


# データの読み込みと図示-------------------------------------------------------------

# データの読み込み
sales_df_2 <- read.csv("5-4-1-sales-ts-2.csv")
sales_df_2$date <- as.POSIXct(sales_df_2$date)
head(sales_df_2, n = 3)

# 図示
autoplot(ts(sales_df_2[, -1]))


# 普通の単回帰モデルの適用 -------------------------------------------------------------

mod_lm <- brm(
  formula = sales ~ publicity,
  family = gaussian(link = "identity"),
  data = sales_df_2,
  seed = 1
)

fixef(mod_lm)


# 時点を分けた、2つの単回帰モデルの適用 --------------------------------------------------------------

# データの分割
sales_df_2_head <- head(sales_df_2, n = 50)
sales_df_2_tail <- tail(sales_df_2, n = 50)

# 前半のモデル化
mod_lm_head <- brm(
  formula = sales ~ publicity,
  family = gaussian(link = "identity"),
  data = sales_df_2_head,
  seed = 1
)

# 後半のモデル化
mod_lm_tail <- brm(
  formula = sales ~ publicity,
  family = gaussian(link = "identity"),
  data = sales_df_2_tail,
  seed = 1
)

# 結果の出力
# 前半
fixef(mod_lm_head)
# 後半
fixef(mod_lm_tail)


# 時変係数モデルの推定 ----------------------------------------------------------------------

# データの準備
data_list <- list(
  y = sales_df_2$sales, 
  ex = sales_df_2$publicity, 
  T = nrow(sales_df_2)
)

# モデルの推定
time_varying_coef_stan <- stan(
  file = "5-4-time-varying-coef.stan",
  data = data_list,
  seed = 1,
  iter = 8000,
  warmup = 2000,
  thin = 6
)

# 結果の表示
print(time_varying_coef_stan,
      pars = c("s_w", "s_t", "s_v", "b[100]"),
      probs = c(0.025, 0.5, 0.975))


# 参考:収束の確認など
mcmc_rhat(rhat(time_varying_coef_stan))
check_hmc_diagnostics(time_varying_coef_stan)

# 参考:トレースなどのチェック
mcmc_sample <- rstan::extract(time_varying_coef_stan, permuted = FALSE)
mcmc_acf_bar(mcmc_sample, pars = c("s_w", "s_t", "s_v", "lp__"))
mcmc_trace(mcmc_sample, pars = c("s_w", "s_t", "s_v", "lp__"))

# 参考:すべての推定値を出力
options(max.print=100000)
print(time_varying_coef_stan, probs = c(0.025, 0.5, 0.975))


# 推定された状態の図示 --------------------------------------------------------------

# MCMCサンプルの取得
mcmc_sample <- rstan::extract(time_varying_coef_stan)

# 図示
p_all <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = sales_df_2$date, 
        obs_vec = sales_df_2$sales,
        state_name = "alpha", 
        graph_title = "推定結果:状態", 
        y_label = "sales") 

p_mu <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = sales_df_2$date, 
        obs_vec = sales_df_2$sales,
        state_name = "mu", 
        graph_title = "推定結果:集客効果を除いた", 
        y_label = "sales") 

p_b <- plotSSM(mcmc_sample = mcmc_sample, 
        time_vec = sales_df_2$date,
        state_name = "b", 
        graph_title = "推定結果:集客効果の変遷", 
        y_label = "coef") 

grid.arrange(p_all, p_mu, p_b)


