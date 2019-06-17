data {
  int T;         // データ取得期間の長さ
  vector[T] y;   // 観測値
  int pred_term; // 予測期間の長さ
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
  for(i in 1:T) {
    y[i] ~ normal(mu[i], s_v);
  }
}

generated quantities{
  vector[T + pred_term] mu_pred; // 予測値も含めた状態の推定値
  
  // データ取得期間においては、状態推定値muと同じ
  mu_pred[1:T] = mu;
  
  // データ取得期間を超えた部分を予測
  for(i in 1:pred_term){
    mu_pred[T + i] = normal_rng(mu_pred[T + i - 1], s_w);
  }
}
