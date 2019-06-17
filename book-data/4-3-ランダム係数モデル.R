
# ランダム係数モデル｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データの読み込み -------------------------------------------------------------

# 分析対象のデータ
fish_num_climate_4 <- read.csv("4-3-1-fish-num-4.csv")
head(fish_num_climate_4, n = 3)

# データの要約
summary(fish_num_climate_4)


# 交互作用を用いたモデル化 ------------------------------------------------------------

# 交互作用を組み込んだポアソン回帰モデル
glm_pois_brms_interaction <- brm(
  formula = fish_num ~ temperature * human,
  family = poisson(),
  data = fish_num_climate_4,
  seed = 1,
  prior = c(set_prior("", class = "Intercept"))
)

# 参考：推定結果
glm_pois_brms_interaction

# 参考：収束の確認
stanplot(glm_pois_brms_interaction, type = "rhat")


# 回帰曲線を描く
# データの分割
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))

# 図示
eff_1 <- marginal_effects(glm_pois_brms_interaction,
                          effects = "temperature",
                          conditions = conditions)
plot(eff_1, points = TRUE)

# brmsによるランダム係数モデルの推定 --------------------------------------------------------

# ランダム係数モデル
glmm_pois_brms_keisu <- brm(
  formula = fish_num ~ temperature + (temperature||human),
  family = poisson(),
  data = fish_num_climate_4,
  seed = 1,
  iter = 6000,
  warmup = 5000,
  control = list(adapt_delta = 0.97, max_treedepth = 15)
)


# 参考：推定結果
glmm_pois_brms_keisu

# 参考：トレースプロットなど
plot(glmm_pois_brms_keisu)

# 参考：弱情報事前分布
prior_summary(glmm_pois_brms_keisu)

# 参考：収束の確認
stanplot(glmm_pois_brms_keisu, type = "rhat")

# 回帰曲線を描く
# データの分割
conditions <- data.frame(
  human = c("A","B","C","D","E","F","G","H","I","J"))

# 図示
eff_2 <- marginal_effects(glmm_pois_brms_keisu,
                          re_formula = NULL,
                          effects = "temperature",
                          conditions = conditions)
plot(eff_2, points = TRUE)



