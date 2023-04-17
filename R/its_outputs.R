its_outputs <- function( fitted.mod=mod1,niter=5000, set.vax.intro.date=vax.intro.date){
  
  model.output= fitted.mod$mod1
  mod.ds=fitted.mod$mod1$model
  form1=fitted.mod$form_its1
  ds=fitted.mod$ds
  
  
  mod.matrix <- model.matrix(form1, data=mod.ds) 
  
  mod.matrix.cf <- mod.matrix %>%
    as.data.frame() %>%
    mutate(spl1=0, spl2=0, spl3=0, spl4=0) %>%
    as.matrix()
  
  mod.matrix.cf <- mod.matrix.cf[, colnames(mod.matrix)] #in case any extra spl wer added , it removes them here
  
  #GENERATE PREDICTIONS
  
  pred.coefs.reg.mean <-
    mvrnorm(n = niter,
            mu = coef(model.output),
            Sigma = vcov(model.output))
  
  if(sum(grepl('offset', colnames(mod.ds)))>0 ){
  logoffset <- mod.ds[, grep('offset', colnames(mod.ds))]
  }else{
    logoffset = rep(0, nrow(ds))
  }
  
  preds.stage1.regmean <-
    exp( mod.matrix %*% t(pred.coefs.reg.mean) +logoffset)
  
  preds.q<-t(apply(preds.stage1.regmean,1,quantile, probs=c(0.025,0.5,0.975)))%>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median_pred=`50%`, lcl_pred=`2.5%`, ucl_pred=`97.5%`)
  
  #Then for counterfactual, set post-vax effects to 0.
  
  preds.stage1.regmean.cf <-    exp(mod.matrix.cf %*% t(pred.coefs.reg.mean)+ logoffset)
  
  preds.cf.q<-t(apply(preds.stage1.regmean.cf,1,quantile, probs=c(0.025,0.5,0.975)))%>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median_cf=`50%`, lcl_cf=`2.5%`, ucl_cf=`97.5%`)
  
  rr.t <- preds.stage1.regmean / preds.stage1.regmean.cf
  
  rr.q.t <- as.data.frame(t(apply(rr.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))) %>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`)
  
  prevented.post.t <-    preds.stage1.regmean.cf - preds.stage1.regmean
  
  
  #Cumulative cases
  cum.post.t <-  apply(prevented.post.t,2, function(x) cumsum(x)   )
  
  cum.post.t.q <-   as.data.frame(t(apply(cum.post.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))) %>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`)
  
  eval.period = which((mod.matrix[,'spl2'] >0 )) #period when evaluate
  preds.stage1.regmean.SUM <-    apply(preds.stage1.regmean[eval.period ,],2, sum )
  preds.stage1.regmean.cf.SUM <- apply(preds.stage1.regmean.cf[eval.period, ],2,sum)
  rr.post <- preds.stage1.regmean.SUM / preds.stage1.regmean.cf.SUM
  rr.q.post <- quantile(rr.post, probs = c(0.025, 0.5, 0.975))
  
  p.rr.trend <- rr.q.t %>% 
    ungroup() %>%
    ggplot( aes( x=date, y=median)) +
    geom_line() +
    theme_classic() +
    geom_ribbon(data=rr.q.t, aes(x=date, ymin=lcl, ymax=ucl), alpha=0.1) +
    ylab('Rate ratio') +
    geom_hline(yintercept=1, lty=2, col='red')+
    geom_vline(xintercept=as.numeric(set.vax.intro.date), lty=2, col='black')
  
  p.cum_prevented <- cum.post.t.q %>% 
    ungroup() %>%
    ggplot( aes( x=date, y=median)) +
    geom_line() +
    theme_classic() +
    geom_ribbon(data=cum.post.t.q, aes(x=date, ymin=lcl, ymax=ucl), alpha=0.1) +
    ylab('Deaths averted') +
    geom_hline(yintercept=1, lty=2, col='red')+
    geom_vline(xintercept=as.numeric(set.vax.intro.date), lty=2, col='black')
  
  
  all.preds <- preds.cf.q %>%
    left_join(preds.q, by='date') %>%
    cbind.data.frame('outcome'=model.output$y)
  
  p.preds <- all.preds %>%
    ggplot( aes( x=date, y=median_pred)) +
    geom_ribbon(data=all.preds, aes(x=date, ymin=lcl_cf, ymax=ucl_cf), alpha=0.1) +
    geom_line() +
    geom_point(data=all.preds, aes(x=date, y=outcome), color='red', alpha=0.3) +
    geom_line(data=all.preds, aes(x=date, y=median_cf), color='white', lty=2) +
    theme_classic() +
    ylab('Number of cases') +
    ylim(0,NA)+
    geom_vline(xintercept=as.numeric(set.vax.intro.date), lty=2, col='black')
  
  
  agg.pred <- all.preds %>%
    mutate(year=year(date)) %>%
    group_by(year) %>%
    summarize( across(c(median_pred,lcl_cf, ucl_cf, median_cf, outcome), sum )) 
  
  p.preds.agg <- agg.pred %>%
    ggplot( aes( x=year, y=median_pred)) +
    geom_ribbon(data=agg.pred, aes(x=year, ymin=lcl_cf, ymax=ucl_cf), alpha=0.1) +
    geom_line() +
    geom_point(data=agg.pred, aes(x=year, y=outcome), color='red', alpha=0.3) +
    geom_line(data=agg.pred, aes(x=year, y=median_cf), color='white', lty=2) +
    theme_classic() +
    ylab('Number of cases') +
    ylim(0,NA)+
    geom_vline(xintercept=as.numeric(year(set.vax.intro.date)), lty=2, col='black')
  
  
  rr.out <- list('rr.q.post' = rr.q.post, 
                 'aic1'=AIC(model.output),'outcome'=mod1$y,
                 'all.preds'=all.preds, 
                 'rr.q.t'=rr.q.t, 'dates'=ds$date, 'p.rr.trend'=p.rr.trend,
                 'p.preds.agg'=p.preds.agg, 'p.preds'=p.preds,
                 'p.cum_prevented'=p.cum_prevented)
  
  
  return(rr.out)
}