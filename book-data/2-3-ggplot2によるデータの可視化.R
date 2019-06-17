
# ggplot2によるデータの可視化｜RとStanではじめる ベイズ統計モデリングによるデータ分析入門
# 馬場真哉


# ライブラリの読み込み
library(ggplot2)

# データの読み込み ----------------------------------------------------------------

# データの読み込み
fish <- read.csv("2-2-1-fish.csv")
head(fish, n = 3)


# ヒストグラムとカーネル密度推定 ---------------------------------------------------------

# ヒストグラム
ggplot(data = fish, mapping = aes(x = length)) + 
  geom_histogram(alpha = 0.5, bins = 20) +
  labs(title = "ヒストグラム")

# カーネル密度推定
ggplot(data = fish, mapping = aes(x = length)) +
  geom_density(size = 1.5) +
  labs(title = "カーネル密度推定")


# グラフの重ね合わせと一覧表示 ----------------------------------------------------------

# グラフの重ね合わせ
ggplot(data = fish, mapping = aes(x = length, y = ..density..)) + 
  geom_histogram(alpha = 0.5, bins = 20) +
  geom_density(size = 1.5) +
  labs(title = "グラフの重ね合わせ")



# グラフの一覧表示
library(gridExtra)

p_hist <- ggplot(data = fish, mapping = aes(x = length)) + 
  geom_histogram(alpha = 0.5, bins = 20) +
  labs(title = "ヒストグラム")

p_density <- ggplot(data = fish, mapping = aes(x = length)) +
  geom_density(size = 1.5) +
  labs(title = "カーネル密度推定")

grid.arrange(p_hist, p_density, ncol = 2)


# 箱ひげ図とバイオリンプロット ----------------------------------------------------------

# アヤメデータ
head(iris, n = 3)

# 箱ひげ図
p_box <- ggplot(data = iris, 
                mapping = aes(x = Species, y = Petal.Length)) + 
  geom_boxplot() +
  labs(title = "箱ひげ図")

# バイオリンプロット
p_violin <- ggplot(data = iris, 
                   mapping = aes(x = Species, y = Petal.Length)) + 
  geom_violin() +
  labs(title = "バイオリンプロット")


# グラフの表示
grid.arrange(p_box, p_violin, ncol = 2)


# 散布図 ---------------------------------------------------------------------

# 散布図
ggplot(iris,  aes(x = Petal.Width, y = Petal.Length)) + 
  geom_point() 

# 色分けした散布図
ggplot(iris, aes(x=Petal.Width, y=Petal.Length, color=Species)) + 
  geom_point() 



# 折れ線グラフ ------------------------------------------------------------------

# ナイル川流量データ
Nile

# data.frameに変換
nile_data_frame <- data.frame(
  year = 1871:1970,
  Nile = as.numeric(Nile)
)

head(nile_data_frame, n = 3)

# 折れ線グラフ
ggplot(nile_data_frame, aes(x = year, y = Nile)) + 
  geom_line()



# tsオブジェクトを楽に描画する方法
library(ggfortify)
autoplot(Nile)



# ggplot2まとめ --------------------------------------------------------------

# 疑似コード（動きません）

# ggplot(データ, aes(x = X変数名, y = Y変数名, color = 色分け対象)) + 
#   geom_xxxx(必要なら引数) +
#   labs(title = "グラフタイトル")




