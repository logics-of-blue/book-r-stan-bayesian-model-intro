data {
  int N;                   // サンプルサイズ
  int germination[N];      // 発芽数
  int binom_size[N];       // 二項分布の試行回数
  vector[N] solar;         // 1：日照あり
  vector[N] nutrition;     // 栄養量
}

parameters {
  real Intercept;          // 切片
  real b_solar;            // 係数(日照の有無)
  real b_nutrition;        // 係数(栄養量)
}

model {
  vector[N] prob = Intercept + b_solar*solar + b_nutrition*nutrition;
  germination ~ binomial_logit(binom_size, prob);
}
