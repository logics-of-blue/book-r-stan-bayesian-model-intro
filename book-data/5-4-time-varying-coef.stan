data {
  int T;        // データ取得期間の長さ
  vector[T] ex; // 説明変数
  vector[T] y;  // 観測値
}

parameters {
  vector[T] mu;       // 水準成分の推定値
  vector[T] b;        // 時変係数の推定値
  real<lower=0> s_w;  // 水準成分の過程誤差の標準偏差
  real<lower=0> s_t;  // 時変係数の変動の大きさを表す標準偏差
  real<lower=0> s_v;  // 観測誤差の標準偏差
}

transformed parameters {
  vector[T] alpha;        // 各成分の和として得られる状態推定値
  
  for(i in 1:T) {
    alpha[i] = mu[i] + b[i] * ex[i];
  }

}

model {
  // 状態方程式に従い、状態が遷移する
  for(i in 2:T) {
    mu[i] ~ normal(mu[i-1], s_w);
    b[i] ~ normal(b[i-1], s_t);
  }
  
  // 観測方程式に従い、観測値が得られる
  for(i in 1:T) {
    y[i] ~ normal(alpha[i], s_v);
  }

}
