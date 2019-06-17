data {
  int T;        // データ取得期間の長さ
  vector[T] ex; // 説明変数
  int y[T];     // 観測値
}

parameters {
  vector[T] mu;       // 水準+ドリフト成分の推定値
  vector[T] r;        // ランダム効果
  real b;             // 係数の推定値
  real<lower=0> s_z;  // ドリフト成分の変動の大きさを表す標準偏差
  real<lower=0> s_r;  // ランダム効果の標準偏差
}

transformed parameters {
  vector[T] lambda;   // 観測値の期待値のlogをとった値
  
  for(i in 1:T) {
    lambda[i] = mu[i] + b * ex[i] + r[i];
  }

}

model {
  // 時点ごとに加わるランダム効果
  r ~ normal(0, s_r);
  
  // 状態方程式に従い、状態が遷移する
  for(i in 3:T) {
    mu[i] ~ normal(2 * mu[i-1] - mu[i-2], s_z);
  }
  
  // 観測方程式に従い、観測値が得られる
  for(i in 1:T) {
    y[i] ~ poisson_log(lambda[i]);
  }

}

generated quantities {
  // 状態推定値（EXP）
  vector[T] lambda_exp;
  // ランダム効果除外の状態推定値
  vector[T] lambda_smooth;
  // ランダム効果除外、説明変数固定の状態推定値
  vector[T] lambda_smooth_fix; 

  lambda_exp = exp(lambda);
  lambda_smooth = exp(mu + b * ex);
  lambda_smooth_fix = exp(mu + b * mean(ex));
}
