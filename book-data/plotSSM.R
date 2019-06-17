plotSSM <- function(mcmc_sample, time_vec, obs_vec = NULL,
                    state_name, graph_title, y_label,
                    date_labels = "%Y年%m月"){
  # 状態空間モデルを図示する関数
  #
  # Args:
  #   mcmc_sample : MCMCサンプル
  #   time_vec    : 時間軸(POSIXct)のベクトル
  #   obs_vec     : (必要なら)観測値のベクトル
  #   state_name  : 図示する状態の変数名
  #   graph_title : グラフタイトル
  #   y_label     : y軸のラベル
  #   date_labels : 日付の書式
  #
  # Returns:
  #   ggplot2により生成されたグラフ
  
  # すべての時点の状態の、95%区間と中央値
  result_df <- data.frame(t(apply(
    X = mcmc_sample[[state_name]],
    MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
  )))
  
  # 列名の変更
  colnames(result_df) <- c("lwr", "fit", "upr")
  
  # 時間軸の追加
  result_df$time <- time_vec
  
  # 観測値の追加
  if(!is.null(obs_vec)){
    result_df$obs <- obs_vec
  }
  
  # 図示
  p <- ggplot(data = result_df, aes(x = time)) + 
    labs(title = graph_title) +
    ylab(y_label) +
    geom_line(aes(y = fit), size = 1.2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + 
    scale_x_datetime(date_labels = date_labels)
  
  # 観測値をグラフに追加
  if(!is.null(obs_vec)){
    p <- p + geom_point(alpha = 0.6, size = 0.9, 
                        data = result_df, aes(x = time, y = obs))
  }
  
  # グラフを返す
  return(p)
}