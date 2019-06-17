data {
  int N;                  // サンプルサイズ
  vector[N] sales_a;      // ビールAの売り上げデータ
  vector[N] sales_b;      // ビールBの売り上げデータ
}

parameters {
  real mu_a;                // ビールAの平均
  real<lower=0> sigma_a;    // ビールAの標準偏差
  real mu_b;                // ビールBの平均
  real<lower=0> sigma_b;    // ビールBの標準偏差
}

model {
  // 平均mu、標準偏差sigmaの正規分布に従ってデータが得られたと仮定
  sales_a ~ normal(mu_a, sigma_a);
  sales_b ~ normal(mu_b, sigma_b);
}

generated quantities {
  real diff;                // ビールAとBの売り上げ平均の差
  diff = mu_b - mu_a;
}
