data {
  int T;               // データ取得期間の長さ
  int len_obs;         // 観測値が得られた個数
  int y[len_obs];      // 観測値
  int obs_no[len_obs]; // 観測値が得られた時点
}

parameters {
  vector[T] mu;       // 状態の推定値
  real<lower=0> s_w;  // 過程誤差の標準偏差
}

model {
  // 弱情報事前分布
  s_w ~ student_t(3, 0, 10);
  
  // 状態方程式に従い、状態が遷移する
  for(i in 2:T) {
    mu[i] ~ normal(mu[i-1], s_w);
  }
  
  // 観測方程式に従い、観測値が得られる
  // ただし、「観測値が得られた時点」でのみ実行する
  for(i in 1:len_obs) {
    y[i] ~ bernoulli_logit(mu[obs_no[i]]);
  }
}

generated quantities{
  vector[T] probs;       // 推定された勝率
  
  probs = inv_logit(mu);
}
