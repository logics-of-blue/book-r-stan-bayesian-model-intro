data {
  int N;                   // サンプルサイズ
  int fish_num[N];         // 釣獲尾数
  vector[N] temp;          // 気温データ
  vector[N] sunny;         // 晴れダミー変数
}

parameters {
  real Intercept;      // 切片
  real b_temp;         // 係数(気温)
  real b_sunny;        // 係数(晴れの影響)
}

model {
  vector[N] lambda = exp(Intercept + b_temp*temp + b_sunny*sunny);
  fish_num ~ poisson(lambda);
}
