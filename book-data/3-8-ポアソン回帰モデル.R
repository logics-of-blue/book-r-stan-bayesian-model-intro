
# ポアソン回帰モデル｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データの読み込みと図示 -------------------------------------------------------------

# 分析対象のデータ
fish_num_climate <- read.csv("3-8-1-fish-num-1.csv")
head(fish_num_climate, 3)

# データの要約
summary(fish_num_climate)

# 図示
ggplot(data = fish_num_climate, 
       mapping = aes(x = temperature, y = fish_num)) +
  geom_point(aes(color = weather)) +
  labs(title = "釣獲尾数と気温・天気の関係")



# brmsによるポアソン回帰モデルの推定 -------------------------------------------------------------------

# ポアソン回帰モデルを作る
glm_pois_brms <- brm(
  formula = fish_num ~ weather + temperature,  # modelの構造を指定
  family = poisson(),                          # ポアソン分布を使う
  data = fish_num_climate,                     # データ
  seed = 1,                                    # 乱数の種
  prior = c(set_prior("", class = "Intercept"))# 無情報事前分布にする
)

# MCMCの結果の確認
glm_pois_brms

# 参考
exp(-0.59)
exp(0.08)


# ポアソン回帰の回帰曲線 -------------------------------------------------------------

# 95%ベイズ信用区間付きのグラフ
eff <- marginal_effects(glm_pois_brms, 
                        effects = "temperature:weather")

plot(eff, points = TRUE)


# 99%ベイズ予測区間付きのグラフ
set.seed(1)
eff_pre <- marginal_effects(glm_pois_brms, 
                            method = "predict",
                            effects = "temperature:weather",
                            probs = c(0.005, 0.995))
plot(eff_pre, points = TRUE)


# 補足：brmsを用いない実装の方法 -------------------------------------------------------------

# 参考：デザイン行列の作成
formula_pois <- formula(fish_num ~ weather + temperature)
design_mat <- model.matrix(formula_pois, fish_num_climate)

design_mat

# 参考：データの作成
data_list_1 <- list(
  N = nrow(fish_num_climate),
  fish_num = fish_num_climate$fish_num,
  temp = fish_num_climate$temperature,
  sunny = as.numeric(design_mat[, "weathersunny"])
)
data_list_1

# 参考：自分で変換処理を入れる
glm_pois_stan_exp <- stan(
  file = "3-8-1-glm-pois-1.stan",
  data = data_list_1,
  seed = 1
)

# 参考：結果の表示
print(glm_pois_stan_exp,
      probs = c(0.025, 0.5, 0.975))


# 参考：poisson_log関数を使用
glm_pois_stan <- stan(
  file = "3-8-2-glm-pois-2.stan",
  data = data_list_1,
  seed = 1
)

# 参考：結果の表示
print(glm_pois_stan,
      probs = c(0.025, 0.5, 0.975))


# 補足：デザイン行列を使ったモデルの推定 --------------------------------------------------------

# 参考：Stanに渡すデータ
data_list_2 <- list(
  N = nrow(fish_num_climate),
  K = 3,
  Y = fish_num_climate$fish_num,
  X = design_mat
)
data_list_2

# 参考：MCMCの実行
glm_pois_stan_design_mat <- stan(
  file = "3-8-3-glm-pois-design-matrix.stan",
  data = data_list_2,
  seed = 1
)

# 参考：結果の表示
print(glm_pois_stan_design_mat,
      probs = c(0.025, 0.5, 0.975))



