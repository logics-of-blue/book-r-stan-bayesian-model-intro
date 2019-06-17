
# brmsの使い方｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# 分析の準備 -------------------------------------------------------------------

# パッケージの読み込み
library(rstan)
library(brms)

# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ
file_beer_sales_2 <- read.csv("3-2-1-beer-sales-2.csv")


# brmsによる単回帰モデルの推定 -------------------------------------------------------------------

# 単回帰モデルを作る
simple_lm_brms <- brm(
  formula = sales ~ temperature,         # modelの構造を指定
  family = gaussian(link = "identity"),  # 正規分布を使う
  data = file_beer_sales_2,              # データ
  seed = 1                               # 乱数の種
)

# MCMCの結果の確認
simple_lm_brms

# 参考：MCMCサンプルの取得
as.mcmc(simple_lm_brms, combine_chains = TRUE)

# 事後分布の図示
plot(simple_lm_brms)


# brmsの基本的な使い方 -------------------------------------------------------------

# formulaとfamilyでモデルの構造を指定する
# 複雑なformulaはbf関数で作成
simple_lm_formula <- bf(sales ~ temperature)

# familyは様々選べる。
# デフォルトのリンク関数は省略可能
# 正規分布
gaussian()
# 二項分布
binomial()
# ポアソン分布
poisson()


# simple_lm_brmsと結果は同じ
simple_lm_brms_2 <- brm(
  formula = simple_lm_formula, # bf関数で作成済みのformulaを指定
  family = gaussian(),      # 正規分布を使う(リンク関数省略)
  data = file_beer_sales_2, # データ
  seed = 1,                 # 乱数の種
  chains = 4,               # チェーン数
  iter = 2000,              # 乱数生成の繰り返し数
  warmup = 1000,            # バーンイン期間
  thin = 1                  # 間引き数(1なら間引き無し) 
)

# 参考
simple_lm_brms_2


# 事前分布の変更 -----------------------------------------------------------------

# 事前分布の取得
prior_summary(simple_lm_brms)

# 事前分布を無情報事前分布にする
simple_lm_brms_3 <- brm(
  formula = sales ~ temperature,
  family = gaussian(),
  data = file_beer_sales_2, 
  seed = 1,
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

# 参考：推定結果
simple_lm_brms_3

# 参考：弱情報事前分布が設定されなくなった
prior_summary(simple_lm_brms_3)

# 参考:その他の係数の事前分布の変更
# 気温の係数
set_prior("normal(0,100000)", class = "b", coef = "temperature")

# 事前分布の標準設定の確認
get_prior(
  formula = sales ~ temperature,
  family = gaussian(), 
  data = file_beer_sales_2
)

# 参考：stanコードの抽出
stancode(simple_lm_brms_3)

# 参考：Stanに渡すデータの抽出
standata(simple_lm_brms_3)

# 補足：make_stancode関数による、Stanコードの作成 ------------------------------------

# Stanコードを作る
make_stancode(
  formula = sales ~ temperature,
  family = gaussian(),
  data = file_beer_sales_2, 
  prior = c(prior("", class = "Intercept"),
            prior("", class = "sigma"))
)


# 補足：make_standata関数による、Stanに渡すデータの作成 ------------------------------------

# rstanに渡すデータを作る
standata_brms <- make_standata(
  formula = sales ~ temperature,
  family = gaussian(),
  data = file_beer_sales_2
)

standata_brms

# 補足：rstanでbrmsの結果を再現する ---------------------------------------------------

# rstanでbrmsのモデルを実行
simple_lm_brms_stan <- stan(
  file = "3-5-1-brms-stan-code.stan",
  data = standata_brms,
  seed = 1
)

# rstanを使ったときの実行結果
print(simple_lm_brms_stan,
      pars = c("b_Intercept", "b[1]", "sigma"),
      probs = c(0.025, 0.5, 0.975))

# brmsを使ったときの実行結果
simple_lm_brms_3


# 事後分布の可視化 ----------------------------------------------------------------

# 係数の95%ベイズ信用区間
stanplot(simple_lm_brms, 
         type = "intervals",
         pars = "^b_",
         prob = 0.8,        # 太い線の範囲
         prob_outer = 0.95) # 細い線の範囲


# brmsによる予測 ---------------------------------------------------------------

# 予測のための説明変数
new_data <- data.frame(temperature = 20)

# 回帰直線の信用区間付きの予測値
fitted(simple_lm_brms, new_data)

# 予測区間付きの予測値
set.seed(1)
predict(simple_lm_brms, new_data)


# 補足：predict関数を使わない予測の実装 ----------------------------------------------------------------

# MCMCサンプルを取り出す
mcmc_sample <- as.mcmc(simple_lm_brms, combine_chains = TRUE)
head(mcmc_sample, n = 2)

# 推定されたパラメタ別に保存しておく
mcmc_b_Intercept   <- mcmc_sample[,"b_Intercept"]
mcmc_b_temperature <- mcmc_sample[,"b_temperature"]
mcmc_sigma         <- mcmc_sample[,"sigma"]

# 予測された期待値のMCMCサンプルを得る
saigen_fitted <- mcmc_b_Intercept + 20 * mcmc_b_temperature

# fittedの再現
mean(saigen_fitted)
quantile(saigen_fitted, probs = c(0.025, 0.975))
fitted(simple_lm_brms, new_data)

# 予測分布のMCMCサンプルを得る
set.seed(1)
saigen_predict <- do.call(
  rnorm, 
  c(4000, list(mean = saigen_fitted, sd = mcmc_sigma))
)

# predictの再現
mean(saigen_predict)
quantile(saigen_predict, probs = c(0.025, 0.975))
set.seed(1)
predict(simple_lm_brms, data.frame(temperature = 20))


# 回帰直線の図示 --------------------------------------------------------

# 回帰直線の95%ベイズ信用区間付きのグラフ
eff <- marginal_effects(simple_lm_brms)
plot(eff, points = TRUE)

# 95%予測区間付きのグラフ
set.seed(1)
eff_pre <- marginal_effects(simple_lm_brms, method = "predict")
plot(eff_pre, points = TRUE)


# 参考：複数の説明変数があるときは、特定の要因だけを切り出せる
marginal_effects(simple_lm_brms,
                 effects = "temperature")

# 参考：複数の説明変数を同時に図示(このコードは動きません)
# marginal_effects(brms_model,
#                  effects = "x1:x2")





