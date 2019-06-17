data {
  int T;        // データ取得期間の長さ
  vector[T] y;  // 観測値
}

parameters {
  vector[T] mu;       // 水準+ドリフト成分の推定値
  vector[T] gamma;    // 季節成分の推定値
  real<lower=0> s_z;  // ドリフト成分の変動の大きさを表す標準偏差
  real<lower=0> s_v;  // 観測誤差の標準偏差
  real<lower=0> s_s;  // 季節変動の大きさを表す標準偏差
}

transformed parameters {
  vector[T] alpha;        // 各成分の和として得られる状態推定値
  
  for(i in 1:T) {
    alpha[i] = mu[i] + gamma[i];
  }

}

model {
  // 水準+ドリフト成分
  for(i in 3:T) {
    mu[i] ~ normal(2 * mu[i-1] - mu[i-2], s_z);
  }
  
  // 季節成分
  for(i in 7:T){
    gamma[i] ~ normal(-sum(gamma[(i-6):(i-1)]), s_s);
  }
  
  // 観測方程式に従い、観測値が得られる
  for(i in 1:T) {
    y[i] ~ normal(alpha[i], s_v);
  }

}
