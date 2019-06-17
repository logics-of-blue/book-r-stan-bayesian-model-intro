
# ダミー変数と分散分析モデル｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
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
sales_weather <- read.csv("3-6-1-beer-sales-3.csv")
head(sales_weather, 3)

# データの要約
summary(sales_weather)

# 図示
ggplot(data = sales_weather, mapping = aes(x = weather, y = sales)) +
  geom_violin() +
  geom_point(aes(color = weather)) +
  labs(title = "ビールの売り上げと天気の関係")



# brmsによる分散分析モデルの推定 -------------------------------------------------------------------

# 分散分析モデルを作る
anova_brms <- brm(
  formula = sales ~ weather,  # modelの構造を指定
  family = gaussian(),        # 正規分布を使う
  data = sales_weather,       # データ
  seed = 1,                   # 乱数の種
  prior = c(set_prior("", class = "Intercept"),
            set_prior("", class = "sigma"))
)

# MCMCの結果の確認
anova_brms

# 推定された天気別の平均売り上げのグラフ
eff <- marginal_effects(anova_brms)
plot(eff, points = FALSE)



# 補足：分散分析モデルのデザイン行列 ----------------------------------------------------------

# デザイン行列の作成
formula_anova <- formula(sales ~ weather)
design_mat <- model.matrix(formula_anova, sales_weather)

# stanに渡すlistの作成
data_list <- list(
  N = nrow(sales_weather), # サンプルサイズ
  K = 3,                   # デザイン行列の列数
  Y = sales_weather$sales, # 応答変数
  X = design_mat           # デザイン行列
)
# Stanに渡すデータの表示
data_list



# 補足：brmsを使わない分散分析モデルの推定 -----------------------------------------------------

# rstanで分散分析モデルを実行
anova_stan <- stan(
  file = "3-4-1-lm-design-matrix.stan",
  data = data_list,
  seed = 1
)

# 結果の確認
print(anova_stan, probs = c(0.025, 0.5, 0.975))

anova_brms





