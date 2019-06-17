
# データの要約｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉



# 度数・度数分布・相対度数分布 ----------------------------------------------------------

# データの読み込み
fish <- read.csv("2-2-1-fish.csv")
head(fish, n = 3)

# ヒストグラム
hist(fish$length)


# カーネル密度推定 ----------------------------------------------------------------

# カーネル密度推定
kernel_density <- density(fish$length)
plot(kernel_density)

# バンド幅をadjust倍に変更します
kernel_density_quarter <- density(fish$length, adjust = 0.25)
kernel_density_quadruple <- density(fish$length, adjust = 4)

# 結果の図示
plot(kernel_density, 
     lwd = 2,                   # 線の太さ
     xlab = "",                 # x軸ラベル名称をなくす 
     ylim = c(0, 0.26),         # y軸の範囲
     main = "バンド幅を変える") # グラフのタイトル
lines(kernel_density_quarter, col = 2)
lines(kernel_density_quadruple, col = 4)

# 凡例を追加
legend("topleft",       # 凡例の位置
       col = c(1,2,4),  # 線の色
       lwd = 1,         # 線の太さ
       bty = "n",       # 凡例の囲み線を消す
       legend = c("標準", "バンド幅1/4", "バンド幅4倍"))


# 算術平均 --------------------------------------------------------------------

# 算術平均
mean(fish$length)


# 中央値・四分位点・パーセント点 ----------------------------------------------------------------

# 0から1000の等差数列
suuretu <- 0:1000

# 中身の確認
suuretu

# 長さの確認
length(suuretu)

# 中央値
median(suuretu)
quantile(suuretu, probs = c(0.5))

# 四分位点
quantile(suuretu, probs = c(0.25, 0.75))

# 95%区間
quantile(suuretu, probs = c(0.025, 0.975))



# 共分散とピアソンの積率相関係数 ---------------------------------------------------------

# CSVファイルを読み込む
birds <- read.csv("2-1-1-birds.csv")

# 体の大きさと羽の大きさの相関係数
cor(birds$body_length, birds$feather_length)



# 自己相関係数とコレログラム -----------------------------------------------------------

# ナイル川の流量データ
Nile

# 標本自己共分散
acf(
  Nile,                # 対象データ
  type = "covariance", # 自己共分散を計算(デフォルトは自己相関)
  plot = F,            # グラフは非表示(デフォルトはTRUE)
  lag.max = 5          # 5時点前までの自己共分散を計算する
)

# 標本自己相関
acf(
  Nile,                # 対象データ
  plot = F,            # グラフは非表示(デフォルトはTRUE)
  lag.max = 5          # 5時点前までの自己相関を計算する
)

# コレログラム
acf(Nile)




