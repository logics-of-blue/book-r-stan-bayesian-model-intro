data {
  int N;                  // サンプルサイズ
  vector[N] sales;        // データ
}

parameters {
  real mu;                // 平均
  real<lower=0> sigma;    // 標準偏差
}

model {
  // 平均mu、標準偏差sigmaの正規分布に従ってデータが得られたと仮定
  for (i in 1:N) {
    sales[i] ~ normal(mu, sigma);
  }
}
