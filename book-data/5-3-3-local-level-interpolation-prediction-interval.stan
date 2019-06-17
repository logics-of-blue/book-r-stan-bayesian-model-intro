data {
  int T;               // データ取得期間の長さ
  int len_obs;         // 観測値が得られた個数
  vector[len_obs] y;   // 観測値
  int obs_no[len_obs]; // 観測値が得られた時点
}

parameters {
  vector[T] mu;       // 状態の推定値(水準成分)
  real<lower=0> s_w;  // 過程誤差の標準偏差
  real<lower=0> s_v;  // 観測誤差の標準偏差
}

model {
  // 状態方程式に従い、状態が遷移する
  for(i in 2:T) {
    mu[i] ~ normal(mu[i-1], s_w);
  }
  
  // 観測方程式に従い、観測値が得られる
  // ただし、「観測値が得られた時点」でのみ実行する
  for(i in 1:len_obs) {
    y[i] ~ normal(mu[obs_no[i]], s_v);
  }
}

generated quantities {
  vector[T] y_pred;       // 観測値の予測値
  
  for (i in 1:T) {
    y_pred[i] = normal_rng(mu[i], s_v);
  }
}
