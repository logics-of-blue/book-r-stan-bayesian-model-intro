data {
  int N;                  // サンプルサイズ
  vector[N] sales;        // データ
}

parameters {
  real mu;                // 平均
  real<lower=0> sigma;    // 標準偏差
}

model {
  // 事前分布の設定
  target += normal_lpdf(mu|0, 1000000);
  target += normal_lpdf(sigma|0, 1000000);
  
  // 平均mu、標準偏差sigmaの正規分布に従ってデータが得られたと仮定
  target += normal_lpdf(sales|mu, sigma);
}
