
# 階層ベイズモデルと一般化線形混合モデルの基本｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(bayesplot)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データの読み込み -------------------------------------------------------------

# 分析対象のデータ
fish_num_climate_2 <- read.csv("4-1-1-fish-num-2.csv")

# id列を数値ではなくfactorとして扱う
fish_num_climate_2$id <- as.factor(fish_num_climate_2$id)
head(fish_num_climate_2, n = 3)


# 通常のポアソン回帰モデルの当てはめ -------------------------------------------------------

# ポアソン回帰モデルを作る
glm_pois_brms <- brm(
  formula = fish_num ~ weather + temperature,  # modelの構造を指定
  family = poisson(),                          # ポアソン分布を使う
  data = fish_num_climate_2,                   # データ
  seed = 1,                                    # 乱数の種
  prior = c(set_prior("", class = "Intercept"))# 無情報事前分布にする
)

glm_pois_brms

# 当てはめ値と99%予測区間の計算
set.seed(1)
eff_glm_pre <- marginal_effects(
  glm_pois_brms, 
  method = "predict",
  effects = "temperature:weather",
  probs = c(0.005, 0.995))

# 結果の図示
plot(eff_glm_pre, points = T)



# StanによるGLMMの推定 -------------------------------------------------------------

# ダミー変数を作る
formula_pois <- formula(fish_num ~ weather + temperature)
design_mat <- model.matrix(formula_pois, fish_num_climate_2)
sunny_dummy <- as.numeric(design_mat[, "weathersunny"])

# データの作成
data_list_1 <- list(
  N = nrow(fish_num_climate_2),
  fish_num = fish_num_climate_2$fish_num,
  temp = fish_num_climate_2$temperature,
  sunny = sunny_dummy
)
# 結果の表示
data_list_1

# MCMCの実行
glmm_pois_stan <- stan(
  file = "4-1-1-glmm-pois.stan",
  data = data_list_1,
  seed = 1
)

# 収束の確認
mcmc_rhat(rhat(glmm_pois_stan))

# 参考：トレースプロットなど
mcmc_sample <- rstan::extract(glmm_pois_stan, permuted = FALSE)
mcmc_combo(
  mcmc_sample, 
  pars = c("Intercept", "b_sunny", "b_temp", "sigma_r", "lp__"))


# 結果の表示
print(glmm_pois_stan,
      pars = c("Intercept", "b_sunny", "b_temp", "sigma_r"),
      probs = c(0.025, 0.5, 0.975))


# brmsによるGLMMの推定 -------------------------------------------------------------

# brmsによるGLMMの推定
glmm_pois_brms <- brm(
  formula = fish_num ~ weather + temperature + (1|id), # ランダム効果
  family = poisson(),                            # ポアソン分布を使う
  data = fish_num_climate_2,                     # データ
  seed = 1,                                      # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sd"))         # 無情報事前分布にする
)

# 結果の表示
glmm_pois_brms

# 参考：トレースプロットなど
plot(glmm_pois_brms)

# 参考：stancode
stancode(glmm_pois_brms)


