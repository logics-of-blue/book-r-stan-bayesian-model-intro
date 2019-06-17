data { 
  int N;                // サンプルサイズ
  int K;                // デザイン行列の列数(説明変数の数＋１)
  int Y[N];             // 応答変数(整数型)
  matrix[N, K] X;       // デザイン行列 
} 

parameters { 
  vector[K] b;          // 切片を含む係数ベクトル
} 

model { 
  vector[N] lambda = X * b;
  Y ~ poisson_log(lambda);
} 
