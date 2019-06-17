data {
  int N;                  // サンプルサイズ
  vector[N] sales;        // 売り上げデータ
  vector[N] temperature;  // 気温データ
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
