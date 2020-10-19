library(tidyverse)
library(magrittr)
library(ROCR)
library(glue)

plot_score_histogram <- function(d, th, n_bins=90, opt_th=0.5, mode='count'){
  g_hist <- ggplot(d, aes(pred, fill=y, group=y))
  
  if(mode == 'count'){
    g_hist <- g_hist + 
      geom_histogram(bins=n_bins, position = 'identity', alpha=0.8) + 
      geom_vline(xintercept = th, linetype='dashed') + 
      geom_vline(xintercept = opt_th, linetype='dashed', colour='red')
    
    g_hist <- g_hist + 
      ggtitle('Prediction score histogram(count)') + 
      theme(legend.position = 'none', plot.background = element_blank()) 
  }else{
    g_hist <- g_hist + 
      geom_histogram(aes(y = ..density..), bins=n_bins, position = 'identity', alpha=0.8) + 
      geom_vline(xintercept = th, linetype='dashed') + 
      geom_vline(xintercept = opt_th, linetype='dashed', colour='red')
    
    g_hist <- g_hist + 
      coord_cartesian(ylim = c(0, 0.6)) + 
      ggtitle('Prediction score histogram(dens)') + 
      theme(legend.position = 'none', plot.background = element_blank())
  }
  
  g_hist <- g_hist + coord_cartesian(xlim = c(-5, 5))
  
  return(g_hist)
}

plot_roc <- function(d){
  rr_pred <- ROCR::prediction(d$pred, d$y)
  
  rr_perf_auc <- ROCR::performance(rr_pred, measure = 'auc')
  v_auc <- rr_perf_auc@y.values[[1]]
  
  rr_perf <- ROCR::performance(rr_pred, measure = 'tpr', x.measure = 'fpr')
  d_perf <- data.frame(rr_perf@x.values, rr_perf@y.values)
  colnames(d_perf) <- c('x', 'y')
  
  g_roc <- ggplot(d_perf, aes(x=x, y=y)) +
    geom_line(colour=4) +
    geom_line(data = data.frame(x=c(0, 1), y=c(0, 1)), linetype=2, colour='darkgray')
  g_roc <- g_roc +
    ggtitle('ROC curve', subtitle=glue('AUC={round(v_auc, 6)}')) +
    xlab('FPR') +
    ylab('TPR') + 
    theme(plot.background = element_blank())
  
  return(g_roc)
}

plot_pr <- function(d){
  rr_pred <- ROCR::prediction(d$pred, d$y)
  
  rr_perf_auc <- ROCR::performance(rr_pred, measure = 'aucpr')
  v_auc <- rr_perf_auc@y.values[[1]]
  
  rr_perf <- ROCR::performance(rr_pred, measure = 'prec', x.measure = 'rec')
  d_perf <- data.frame(rr_perf@x.values, rr_perf@y.values)
  colnames(d_perf) <- c('x', 'y')
  
  g_pr <- ggplot(d_perf, aes(x=x, y=y)) +
    geom_line(colour=4)
  g_pr <- g_pr +
    ggtitle('PR curve', subtitle = glue('AUC for PR-curve={round(v_auc, 6)}')) +
    xlab('Precision') +
    ylab('Recall') +
    theme(plot.background = element_blank())
  
  return(g_pr)
}

get_threshold_by_youden_index <- function(d){
  rr_pred <- ROCR::prediction(d$pred, d$y)
  rr_perf <- ROCR::performance(rr_pred, measure = 'tpr', x.measure = 'fpr')
  youden_index <- rr_perf@y.values[[1]] - rr_perf@x.values[[1]]
  th <- rr_pred@cutoffs[[1]][which.max(youden_index)]

  return(th)
}

get_threshold <- function(d, mode='youden'){
  if(mode=='youden'){
    ret <- get_threshold_by_youden_index(d)
  }
  return(ret)
}

get_confusion_matrix_plot <- function(d, th=0, str_title){
  d1 <- data.frame(y=c(T,T,F,F), y_pred_flag=c(T,F,T,F))
  
  d2 <- d %>% 
    mutate(y_pred_flag = case_when(pred >= th ~ 1, TRUE ~ 0))
  tmp_d <- d2 %>% 
    group_by(y, y_pred_flag) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    right_join(d1, by=c('y', 'y_pred_flag')) %>% 
    mutate(
      y=case_when(y==0~"FALSE", TRUE~"TRUE"),
      y_pred_flag=case_when(y_pred_flag==0~"FALSE", TRUE~"TRUE"),
      n = replace_na(n, 0)
    )
  
  correct_case <- tmp_d %>% filter(y_pred_flag == y) %>% summarise(x=sum(n)) %>% extract2('x')
  incorrect_case <- tmp_d %>% filter(y_pred_flag != y) %>% summarise(x=sum(n)) %>% extract2('x')
  
  g <- ggplot(tmp_d, aes(x=y, y=y_pred_flag, label=n, fill=n)) + 
    geom_tile() + 
    geom_text(colour='white', size=10)
  g <- g + 
    theme(legend.position = 'none', , plot.background = element_blank()) +
    ggtitle(str_title, subtitle = glue('correct:{correct_case}\nincorrect:{incorrect_case}')) + 
    xlab('Grand Truth') + 
    ylab('Prediction')
  
  return(g)
}

