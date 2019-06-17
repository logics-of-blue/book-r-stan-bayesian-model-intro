data {
  int T;        // データ取得期間の長さ
  vector[T] y;  // 観測値
}

parameters {
  vector[T] mu;       // 水準+ドリフト成分の推定値
  vector[T] delta;    // ドリフト成分の推定値
  real<lower=0> s_w;  // 水準成分の変動の大きさを表す標準偏差
  real<lower=0> s_z;  // ドリフト成分の変動の大きさを表す標準偏差
  real<lower=0> s_v;  // 観測誤差の標準偏差
}

model {
  // 弱情報事前分布
  s_w ~ normal(2, 2);
  s_z ~ normal(0.5, 0.5);
  s_v ~ normal(10, 5);
  
  // 状態方程式に従い、状態が遷移する
  for(i in 2:T) {
    mu[i] ~ normal(mu[i-1] + delta[i-1], s_w);
    delta[i] ~ normal(delta[i-1], s_z);
  }
  
  // 観測方程式に従い、観測値が得られる
  for(i in 1:T) {
    y[i] ~ normal(mu[i], s_v);
  }

}
