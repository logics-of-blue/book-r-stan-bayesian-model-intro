data {
  int N;          // サンプルサイズ
  int animal_num[N];      // データ
}

parameters {
  real<lower=0> lambda;   // 強度
}

model {
  // 強度lambdaのポアソン分布
  animal_num ~ poisson(lambda);
}

generated quantities{
  // 事後予測分布を得る
  int pred[N];
  for (i in 1:N) {
    pred[i] = poisson_rng(lambda);
  }
}
