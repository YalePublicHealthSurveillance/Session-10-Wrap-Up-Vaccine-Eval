control_outputs <- function( fitted.mod=mod3,niter=10000, months.start=months_until_second_knot, set.vax.intro.date=vax.intro.date,set.months_until_second_knot=months_until_second_knot){
  

  model.output= fitted.mod$mod1
  mod.ds=fitted.mod$mod1$model
  form1=fitted.mod$form_its1
  ds=fitted.mod$ds
  Y= ds$J12_J18

  vax.intro.index = which(ds$date ==set.vax.intro.date   )
  eval.index = vax.intro.index + set.months_until_second_knot
  
  form2 <- as.formula(paste0('~',as.character(form1)[3]))
  mod.matrix <- model.matrix(form2, data=ds) 


    #GENERATE PREDICTIONS
  
  pred.coefs.reg.mean <-
    mvrnorm(n = niter,
            mu = coef(model.output),
            Sigma = vcov(model.output))
  
  if(sum(grepl('offset', colnames(ds)))>0 ){
  logoffset <- ds[, grep('offset', colnames(ds))]
  }else{
    logoffset = rep(0, nrow(ds))
  }
  
  preds.stage1.regmean <-
    exp( mod.matrix %*% t(pred.coefs.reg.mean) +logoffset)
  
  N.samples.stage2 = 1
  
  preds.stage2 <- rnbinom(n = length(preds.stage1.regmean) * N.samples.stage2,
                          size = model.output$theta, mu = preds.stage1.regmean)
  
  preds.stage2 <- matrix(preds.stage2,
                         nrow = nrow(preds.stage1.regmean),
                         ncol = ncol(preds.stage1.regmean) * N.samples.stage2)
  
  preds.q<-t(apply(preds.stage2,1,quantile, probs=c(0.025,0.5,0.975)))%>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median_pred=`50%`, lcl_pred=`2.5%`, ucl_pred=`97.5%`)
  
  
  rr.t <-  apply(preds.stage2,2, function(x) Y/x  )
  
  rr.q.t <- as.data.frame(t(apply(rr.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))) %>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`)
  
  eval.period = eval.index:nrow(mod.matrix) #period when evaluate
  
  preds.stage2.regmean.SUM <-    apply(preds.stage2[eval.period ,],2, sum )
  obs.sum.eval <- sum(Y[eval.period])
  
  rr.post <-    obs.sum.eval/preds.stage2.regmean.SUM 
  rr.q.post <- quantile(rr.post, probs = c(0.025, 0.5, 0.975))
  
  prevented.post.t <-    apply(preds.stage2,2, function(x) x -Y   )

  
  #Cumulative cases
  cum.post.t <-  apply(prevented.post.t,2, function(x) cumsum(x)   )

  cum.post.t.q <-   as.data.frame(t(apply(cum.post.t, 1, quantile, probs = c(0.025, 0.5, 0.975)))) %>% 
    cbind.data.frame(., 'date'=as.Date(ds$date))  %>%
    rename(median=`50%`, lcl=`2.5%`, ucl=`97.5%`)
    
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
  
  
  all.preds <- preds.q %>%
    cbind.data.frame('outcome'=Y)
  
  p.preds <- all.preds %>%
    ggplot( aes( x=date, y=median_pred)) +
    geom_ribbon(data=all.preds, aes(x=date, ymin=lcl_pred, ymax=ucl_pred), alpha=0.1) +
    geom_line() +
    geom_point(data=all.preds, aes(x=date, y=outcome), color='red', alpha=0.3) +
    theme_classic() +
    ylab('Number of cases') +
    ylim(0,NA) +
    geom_vline(xintercept=as.numeric(set.vax.intro.date), lty=2, col='black')
  
  agg.pred <- all.preds %>%
    mutate(year=year(date)) %>%
    group_by(year) %>%
    summarize( across(c(median_pred,lcl_pred, ucl_pred,  outcome), sum )) 
  
  p.preds.agg <- agg.pred %>%
    ggplot( aes( x=year, y=median_pred)) +
    geom_ribbon(data=agg.pred, aes(x=year, ymin=lcl_pred, ymax=ucl_pred), alpha=0.1) +
    geom_line() +
    geom_point(data=agg.pred, aes(x=year, y=outcome), color='red', alpha=0.3) +
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