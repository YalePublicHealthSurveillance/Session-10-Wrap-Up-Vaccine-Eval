
run_its <- function(set.vax.intro.date,months_until_second_knot=12,months_until_third_knot=999, set.form_its1){
  ds1 <- a2 %>%
    filter(grepl(ds,age_group)) %>%
    arrange(date) %>%
    #create the variables needed for ITS and trend and seasonal adjustments
    mutate( index=row_number(),
            vax.intro.index = which(date ==set.vax.intro.date   ),
            spl1 = ifelse(index>vax.intro.index, index- vax.intro.index , 0),
            spl2 = ifelse(index>(vax.intro.index+months_until_second_knot), index- vax.intro.index - months_until_second_knot , 0),
            spl3 = ifelse(index>(vax.intro.index+months_until_third_knot), index- vax.intro.index - months_until_third_knot , 0),
            month=as.factor(month(date)),
            J12_J18_pre = ifelse(date <set.vax.intro.date, J12_J18, NA_real_)) %>%
    #log and scale the covariates
    mutate(across(c( E40_E46_prim,E70_E90_prim,A30_49_prim, C00_D48_prim, K00_K99_prim, V01_X59_prim),
                  scale.fun),
           acm_noresp_nodiar=log(acm_noresp_nodiar+0.5)
    )

  #acm_noresp_nodiar 
  mod1 <- glm.nb(form_its1, data=ds1)
  
  out.list=list(mod1=mod1,'form_its1'=form_its1, 'ds'=ds1)
  
  return(out.list)
}