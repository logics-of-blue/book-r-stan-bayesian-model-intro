data {
  int N;                  // サンプルサイズ
  vector[N] sales;        // 売り上げデータ
  vector[N] temperature;  // 気温データ
  
  int N_pred;                        // 予測対象データの大きさ
  vector[N_pred] temperature_pred;   // 予測対象となる気温
}

parameters {
  real Intercept;         // 切片
  real beta;              // 係数
  real<lower=0> sigma;    // 標準偏差
}

model {
  // 平均Intercept + beta*temperature
  // 標準偏差sigmaの正規分布に従ってデータが得られたと仮定
  for (i in 1:N) {
    sales[i] ~ normal(Intercept + beta*temperature[i], sigma);
  }
}

generated quantities {
  vector[N_pred] mu_pred;           // ビールの売り上げの期待値
  vector[N_pred] sales_pred;        // ビールの売り上げの予測値

  for (i in 1:N_pred) {
    mu_pred[i] = Intercept + beta*temperature_pred[i];
    sales_pred[i] = normal_rng(mu_pred[i], sigma);
  }
}
