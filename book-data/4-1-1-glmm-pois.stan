data {
  int N;                   // サンプルサイズ
  int fish_num[N];         // 釣獲尾数
  vector[N] sunny;         // 晴れダミー変数
  vector[N] temp;          // 気温データ
}

parameters {
  real Intercept;          // 切片
  real b_temp;             // 係数(気温)
  real b_sunny;            // 係数(晴れの影響)
  vector[N] r;             // ランダム効果
  real<lower=0> sigma_r;   // ランダム効果の標準偏差
}

transformed parameters{
  vector[N] lambda = Intercept + b_sunny*sunny + b_temp*temp + r;
}

model {
  r ~ normal(0, sigma_r);
  fish_num ~ poisson_log(lambda);
}
