data {
  int T;         // データ取得期間の長さ
  vector[T] y;   // 観測値
}

parameters {
  real<lower=0> s_w;  // 過程誤差の標準偏差
  real b_ar;          // 自己回帰項の係数
  real Intercept;     // 切片
}

model {
  for(i in 2:T) {
    y[i] ~ normal(Intercept + y[i-1]*b_ar, s_w);
  }
}
