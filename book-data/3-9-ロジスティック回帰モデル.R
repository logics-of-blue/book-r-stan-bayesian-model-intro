
# ロジスティック回帰モデル｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
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
germination_dat <- read.csv("3-9-1-germination.csv")
head(germination_dat, n = 3)

# データの要約
summary(germination_dat)

# 図示
ggplot(data = germination_dat, 
       mapping = aes(x = nutrition, y = germination, color = solar)) +
  geom_point() +
  labs(title = "種子の発芽数と、日照の有無・栄養素の量の関係")


# brmsによるロジスティック回帰モデルの推定 -------------------------------------------------------------------

# ロジスティク回帰モデルを作る
glm_binom_brms <- brm(
  germination | trials(size) ~ solar + nutrition, # modelの構造を指定
  family = binomial(),                         # 二項分布を使う
  data = germination_dat,                      # データ
  seed = 1,                                    # 乱数の種
  prior = c(set_prior("", class = "Intercept"))# 無情報事前分布にする
)

# MCMCの結果の確認
glm_binom_brms


# 推定されたモデルの解釈 -------------------------------------------------------------

# 係数の解釈
# 説明変数を作る
newdata_1 <- data.frame(
  solar = c("shade", "sunshine", "sunshine"),
  nutrition = c(2,2,3),
  size = c(10,10,10)
)
newdata_1

# 発芽率を予測
# 線形予測子の予測値
linear_fit <- fitted(glm_binom_brms, newdata_1, scale = "linear")[,1]
# ロジスティック関数を適用して、成功確率を計算
fit <- 1 / (1 + exp(-linear_fit))
fit

# オッズを計算
odds_1 <- fit[1] / (1 - fit[1])
odds_2 <- fit[2] / (1 - fit[2])
odds_3 <- fit[3] / (1 - fit[3])

# モデルの係数を取得
coef <- fixef(glm_binom_brms)[,1]
coef

# solarがshadeからsunshineに変わった時のオッズ比
odds_2 / odds_1
exp(coef["solarsunshine"])

# nutritionが1から2に変わった時のオッズ比
odds_3 / odds_2
exp(coef["nutrition"])


# 95%ベイズ信用区間付きの回帰曲線
eff <- marginal_effects(glm_binom_brms, 
                        effects = "nutrition:solar")

plot(eff, points = TRUE)



# 参考：事後分布の図示
plot(glm_binom_brms, pars = "^b_")

# 参考：係数の信頼区間
stanplot(glm_binom_brms, type = "intervals", pars = "^b_")

# 参考：95%ベイズ予測区間付きのグラフ
set.seed(1)
eff_pre <- marginal_effects(glm_binom_brms, 
                            method = "predict",
                            effects = "nutrition:solar")
plot(eff_pre, points = TRUE)


# brmsを用いない実装の方法 -------------------------------------------------------------

# 参考：ダミー変数の作成
solar_dummy <- as.numeric(germination_dat$solar == "sunshine")

# 参考：データの作成
data_list_1 <- list(
  N = nrow(germination_dat),
  germination = germination_dat$germination,
  binom_size = germination_dat$size,
  solar = solar_dummy,
  nutrition = germination_dat$nutrition
)
data_list_1

# 参考：自分でStanコードを実装
glm_binom_stan <- stan(
  file = "3-9-1-glm-binom-1.stan",
  data = data_list_1,
  seed = 1
)

# 参考：結果の表示
print(glm_binom_stan,
      probs = c(0.025, 0.5, 0.975))



# 補足：試行回数が常に1の場合 ----------------------------------------------------------

# 参考：0/1データの場合（このコードは実行できません）
# glm_bernoulli_brms <- brm(
#   formula = 0/1データ ~ 説明変数,              # modelの構造を指定
#   family = bernoulli(),                        # ベルヌーイ分布を使う
#   data = データ,                               # データ
#   seed = 1,                                    # 乱数の種
#   prior = c(set_prior("", class = "Intercept"))# 無情報事前分布にする
# )
