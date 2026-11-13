# FINAL_EMPIRICAL_DIAGNOSTICS.R
# Read-only analysis of the completed FULL run. No source() of an engine,
# model fitting, bootstrap generation, future simulation, or benchmark changes.
FULL_FOLDER <- file.path("MESE_EMPIRICAL_REVIEW_v2","tourism_monthly","full_offsets_0_B100_S20000")
OUTPUT_ROOT <- "MESE_EMPIRICAL_FINAL_AUDIT"
CHECK_ABS <- 1e-8
CHECK_REL <- 1e-8

mostra_audit_finale <- local({
  started<-Sys.time()
  must<-function(ok,msg)if(!isTRUE(ok))stop(msg,call.=FALSE)
  bind<-function(xs){xs<-Filter(function(x)!is.null(x)&&is.data.frame(x)&&nrow(x)>0L,xs);if(!length(xs))return(data.frame());z<-do.call(rbind,xs);rownames(z)<-NULL;z}
  key<-function(d,cols)if(!nrow(d))character() else do.call(paste,c(lapply(d[,cols,drop=FALSE],function(x)ifelse(is.na(x),"<NA>",as.character(x))),sep="\035"))
  groups<-function(d,cols)if(nrow(d))split(seq_len(nrow(d)),key(d,cols)) else list()
  need<-function(d,cols,label)must(all(cols%in%names(d)),paste(label,"missing columns",paste(setdiff(cols,names(d)),collapse=", ")))
  unique_key<-function(d,cols,label)must(!anyDuplicated(key(d,cols)),paste("Duplicate key in",label))
  parse_flag<-function(x,label){a<-toupper(trimws(as.character(x)));z<-rep(NA,length(a));z[a%in%c("TRUE","T","1")]<-TRUE;z[a%in%c("FALSE","F","0")]<-FALSE
    must(all(is.na(a)|a%in%c("","NA","TRUE","T","1","FALSE","F","0")),paste("Invalid logical:",label));z}
  yes<-function(x)x%in%TRUE
  near<-function(a,b){if(length(a)!=length(b)||!identical(is.na(a),is.na(b)))return(FALSE);ok<-!is.na(a)
    all(is.finite(a[ok]))&&all(is.finite(b[ok]))&&all(abs(a[ok]-b[ok])<=CHECK_ABS+CHECK_REL*pmax(abs(a[ok]),abs(b[ok])))}
  avg<-function(x)if(any(is.finite(x)))mean(x[is.finite(x)]) else NA_real_
  qval<-function(x,p)if(any(is.finite(x)))as.numeric(quantile(x[is.finite(x)],p,type=8,names=FALSE)) else NA_real_
  mean_stats<-function(a,b,tie_abs,tie_rel){
    must(length(a)==length(b)&&all(is.finite(a))&&all(is.finite(b)),"Invalid paired values")
    n<-length(a);d<-a-b;tol<-tie_abs+tie_rel*pmax(abs(a),abs(b));win<-d< -tol;loss<-d>tol;tie<-!win&!loss
    mu<-if(n)mean(d) else NA_real_;se<-if(n>1)sd(d)/sqrt(n) else NA_real_;lo<-hi<-pv<-pw<-NA_real_;test<-"fewer_than_two_pairs"
    if(n>1){if(is.finite(se)&&se>0){lo<-mu-qt(.975,n-1)*se;hi<-mu+qt(.975,n-1)*se;pv<-2*pt(-abs(mu/se),n-1);test<-"paired_series_t_approximation"}
      else{lo<-hi<-mu;test<-"zero_observed_variance_no_t_test"}
      if(any(d!=0))pw<-suppressWarnings(tryCatch(wilcox.test(d,mu=0,exact=FALSE,correct=TRUE)$p.value,error=function(e)NA_real_))}
    totalgain<--sum(d[win]);totalcost<-sum(d[loss]);tienet<-sum(d[tie])
    must(!n||near(mu,(totalcost-totalgain+tienet)/n),"Win/loss decomposition failed")
    loo<-if(n>1)(sum(d)-d)/(n-1) else NA_real_;den<-sum(abs(d));ord<-order(abs(d),decreasing=TRUE)
    data.frame(n_pairs=n,mean_focal=if(n)mean(a) else NA_real_,mean_reference=if(n)mean(b) else NA_real_,
      difference=mu,SE_series=se,lower95=lo,upper95=hi,p_mean=pv,p_Wilcoxon_approx=pw,test_status=test,
      relative_change_pct=if(n&&mean(b)>0)100*(mean(a)/mean(b)-1) else NA_real_,
      median_difference=if(n)median(d) else NA_real_,q05_difference=qval(d,.05),q95_difference=qval(d,.95),
      wins=sum(win),ties=sum(tie),losses=sum(loss),win_pct=if(n)100*mean(win) else NA_real_,
      average_gain_when_winning=if(any(win))-mean(d[win]) else NA_real_,average_cost_when_losing=if(any(loss))mean(d[loss]) else NA_real_,
      total_gain_winning_series=totalgain,total_cost_losing_series=totalcost,tie_net_difference=tienet,
      largest3_share_absolute_differences=if(den>0)sum(abs(d[ord[seq_len(min(3L,n))]]))/den else NA_real_,
      leave_one_out_mean_min=if(n>1)min(loo) else NA_real_,leave_one_out_mean_max=if(n>1)max(loo) else NA_real_,
      leave_one_out_sign_changes=if(n>1&&mu!=0)any(loo*mu<0) else FALSE,row.names=NULL)
  }
  score_mean<-function(Q){v<-Reduce(`+`,lapply(seq_len(nrow(Q)),function(b)Q[b,]))/nrow(Q);names(v)<-colnames(Q);v}
  perturbed_grid<-function(Q,replica,model,gap){
    must(is.matrix(Q)&&is.numeric(Q)&&all(is.finite(Q))&&!is.null(colnames(Q)),"Invalid saved IC matrix")
    must(length(replica)==length(model)&&length(model)==length(gap)&&all(is.finite(gap)&gap>0),"Invalid grid perturbations")
    must(all(replica>=1&replica<=nrow(Q)&replica==as.integer(replica))&&all(model%in%colnames(Q)),"Grid issue outside saved matrix")
    must(!anyDuplicated(paste(replica,model,sep="\035")),"Repeated replica/model issue: do not sum ambiguously")
    P<-Q;for(i in seq_along(replica))P[replica[i],model[i]]<-P[replica[i],model[i]]-2*gap[i]
    list(before=Q,after=P,mean_before=score_mean(Q),mean_after=score_mean(P))
  }
  # Deterministic tests: majority wins can coexist with a positive mean loss difference.
  test<-mean_stats(c(1,1,1,10),rep(2,4),1e-10,1e-8)
  must(test$wins==3&&test$losses==1&&test$difference==1.25&&test$median_difference== -1,"Loss-decomposition self-test failed")
  Q<-matrix(c(10,11,10,11),nrow=2,byrow=TRUE,dimnames=list(NULL,c("A","B")))
  testgrid<-perturbed_grid(Q,1L,"B",2)
  must(names(which.min(testgrid$mean_before))=="A"&&names(which.min(testgrid$mean_after))=="B","IC-perturbation self-test failed")
  must(mean_stats(numeric(),numeric(),1e-10,1e-8)$n_pairs==0L,"Empty-pair self-test failed")

  must(dir.exists(FULL_FOLDER),paste("Full folder missing:",FULL_FOLDER))
  folder<-normalizePath(FULL_FOLDER,winslash="/",mustWork=TRUE)
  cp<-file.path(folder,"configuration.rds");must(file.exists(cp),"Missing full configuration")
  cfg<-readRDS(cp)
  must(identical(cfg$stage,"full")&&identical(cfg$dataset,"tourism_monthly"),"This audit requires the full Tourism run")
  must(identical(as.integer(cfg$origin_offsets),0L)&&cfg$H==12L,"Only the single official origin is covered by this audit")
  H<-as.integer(cfg$H);levels<-as.numeric(cfg$levels)
  must(length(levels)>0&&all(is.finite(levels))&&all(levels>0&levels<1)&&!anyDuplicated(levels),"Invalid configured levels")
  tie_abs<-cfg$tie_abs;tie_rel<-cfg$tie_rel
  must(length(tie_abs)==1&&length(tie_rel)==1&&all(is.finite(c(tie_abs,tie_rel)))&&all(c(tie_abs,tie_rel)>=0),"Invalid saved tie tolerances")
  paths<-c(cp,list.files(folder,pattern="\\.csv$",full.names=TRUE));hash_before<-tools::md5sum(paths)
  read<-function(n){p<-file.path(folder,n);must(file.exists(p),paste("Missing stored output:",n,"-- no fitting was started"));read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)}
  tasks<-read("task_manifest.csv");st<-read("status.csv");ev<-read("point_events.csv");ie<-read("interval_events.csv")
  ni<-read("numerical_issues.csv");ss<-read("interval_S_sensitivity.csv");bc<-read("bagged_native_codes.csv")
  ks<-c("series_id","origin","method")
  need(tasks,c("task_id","series_id","origin"),"tasks");unique_key(tasks,c("series_id","origin"),"tasks")
  need(st,c(ks,"fit_success","intervals_planned","interval_success","numerical_anomaly","native_code"),"status")
  for(n in c("fit_success","intervals_planned","interval_success","numerical_anomaly"))st[[n]]<-parse_flag(st[[n]],n)
  unique_key(st,ks,"status");must(!anyNA(st[,ks]),"Missing status identifiers")
  expected<-merge(tasks[,c("series_id","origin"),drop=FALSE],data.frame(method=cfg$methods),by=NULL)
  must(setequal(key(st,ks),key(expected,ks)),"Incomplete or foreign status schedule")
  need(ev,c(ks,"horizon","actual","forecast","mase_scale"),"point events")
  unique_key(ev,c(ks,"horizon"),"point events")
  must(!anyNA(ev[,c(ks,"horizon")])&&all(ev$horizon%in%seq_len(H)),"Invalid point identifiers/horizons")
  must(all(is.finite(ev$actual))&&all(is.finite(ev$forecast)),"Nonfinite point input")
  j<-match(key(ev,ks),key(st,ks));must(!anyNA(j)&&all(yes(st$fit_success[j])),"Events without successful scheduled fit")
  for(ix in groups(ev,c("series_id","origin","horizon")))must(length(unique(ev$actual[ix]))==1,"Paired actual outcomes disagree")
  perpoint<-bind(lapply(groups(ev,ks),function(ix){x<-ev[ix,,drop=FALSE]
    must(nrow(x)==H&&setequal(x$horizon,seq_len(H)),"Incomplete point horizon set")
    must(length(unique(x$mase_scale))==1,"Changing MASE denominator within an origin")
    e<-x$actual-x$forecast;den<-abs(x$actual)+abs(x$forecast);scale<-x$mase_scale[1]
    ape<-ifelse(x$actual!=0,100*abs(e)/abs(x$actual),NA_real_)
    smape<-ifelse(den==0,0,200*abs(e)/den)
    cbind(x[1,ks,drop=FALSE],data.frame(MAE=mean(abs(e)),MSE=mean(e^2),RMSE=sqrt(mean(e^2)),MAPE=avg(ape),sMAPE=mean(smape),
      MASE=if(is.finite(scale)&&scale>0)mean(abs(e))/scale else NA_real_,mase_scale=scale,n_MAPE_events=sum(is.finite(ape))))
  }))
  expected_points<-st[yes(st$fit_success),ks,drop=FALSE]
  must(setequal(key(expected_points,ks),key(perpoint,ks)),"A successful fit has no complete point-event set")
  need(ie,c(ks,"level","horizon","lower","upper","covered","width","IS","scaled_IS","mase_scale","interval_construction","S"),"interval events")
  must(!anyNA(ie[,c(ks,"horizon","level","interval_construction")])&&all(ie$horizon%in%seq_len(H)),"Invalid interval identifiers")
  must(all(vapply(ie$level,function(l)any(abs(l-levels)<1e-10),logical(1))),"Unknown interval level")
  ie$level<-vapply(ie$level,function(l)levels[which.min(abs(levels-l))],numeric(1))
  unique_key(ie,c(ks,"level","horizon"),"interval events")
  must(all(is.finite(ie$lower))&&all(is.finite(ie$upper))&&all(ie$lower<=ie$upper),"Invalid interval bounds")
  j<-match(key(ie,ks),key(st,ks));must(!anyNA(j)&&all(yes(st$intervals_planned[j]))&&all(yes(st$interval_success[j])),"Foreign or unsuccessful interval events")
  target<-unique(ev[,c("series_id","origin","horizon","actual","mase_scale")]);unique_key(target,c("series_id","origin","horizon"),"actuals/scales")
  j<-match(key(ie,c("series_id","origin","horizon")),key(target,c("series_id","origin","horizon")))
  must(!anyNA(j)&&near(ie$mase_scale,target$mase_scale[j]),"Interval targets/scales cannot be matched")
  y<-target$actual[j];a<-1-ie$level;reIS<-ie$upper-ie$lower+(2/a)*(pmax(ie$lower-y,0)+pmax(y-ie$upper,0))
  must(near(ie$IS,reIS)&&near(ie$width,ie$upper-ie$lower)&&near(ie$covered,as.numeric(y>=ie$lower&y<=ie$upper)),"Stored interval scoring mismatch")
  reScaled<-ifelse(is.finite(ie$mase_scale)&ie$mase_scale>0,reIS/ie$mase_scale,NA_real_)
  must(near(ie$scaled_IS,reScaled),"Stored scaled interval score mismatch")
  perinterval<-bind(lapply(groups(ie,c(ks,"level")),function(ix){x<-ie[ix,,drop=FALSE]
    must(nrow(x)==H&&setequal(x$horizon,seq_len(H)),"Incomplete interval horizon set")
    must(length(unique(x$S))==1&&length(unique(x$interval_construction))==1,"Mixed interval budgets/constructions")
    if(x$method[1]=="ETS_standard")must(all(is.na(x$S)),"Unexpected native ETS S") else must(all(x$S==cfg$S),"Wrong simulated path budget")
    cbind(x[1,c(ks,"level"),drop=FALSE],data.frame(IS=mean(x$IS),scaled_IS=avg(x$scaled_IS),coverage=mean(x$covered),
      mean_width=mean(x$width),reference_S=x$S[1],interval_construction=x$interval_construction[1]))
  }))
  expected_intervals<-st[yes(st$intervals_planned)&yes(st$interval_success),ks,drop=FALSE]
  expected_intervals<-bind(lapply(levels,function(l){x<-expected_intervals;x$level<-l;x}))
  must(setequal(key(expected_intervals,c(ks,"level")),key(perinterval,c(ks,"level"))),"A successful interval status has no complete level/horizon events")
  need(ni,c("series_id","origin","stage","model","issue","gap","tolerance"),"numerical issues")
  must(!anyNA(ni[,c("series_id","origin","stage","model","issue")]),"Missing issue identifiers")
  must(all(key(ni,c("series_id","origin"))%in%key(tasks,c("series_id","origin"))),"Issue outside schedule")
  need(bc,c(ks,"member","code"),"Bagged native codes");unique_key(bc,c(ks,"member"),"Bagged codes")
  st$native_nonzero<-FALSE;st$native_diagnostic_incomplete<-FALSE
  ix<-which(st$method=="ETS_standard")
  st$native_nonzero[ix]<-is.finite(st$native_code[ix])&st$native_code[ix]!=0
  st$native_diagnostic_incomplete[ix]<-yes(st$fit_success[ix])&!is.finite(st$native_code[ix])
  for(ix in groups(st[st$method=="BaggedETS_original",,drop=FALSE],ks)){
    bagst<-st[st$method=="BaggedETS_original",,drop=FALSE][ix,,drop=FALSE]
    row<-match(key(bagst,ks),key(st,ks));cc<-bc[key(bc,ks)==key(bagst,ks),,drop=FALSE]
    st$native_nonzero[row]<-any(is.finite(cc$code)&cc$code!=0)
    st$native_diagnostic_incomplete[row]<-yes(bagst$fit_success)&(nrow(cc)!=cfg$B||any(!is.finite(cc$code)))
  }
  datasets<-bind(lapply(groups(st,c("series_id","origin")),function(ix){x<-st[ix,,drop=FALSE];iss<-ni[ni$series_id==x$series_id[1]&ni$origin==x$origin[1],,drop=FALSE]
    data.frame(series_id=x$series_id[1],origin=x$origin[1],n_issue_records=nrow(iss),has_source_issue=nrow(iss)>0||any(yes(x$numerical_anomaly)),
      has_grid_issue=any(grepl("^MEB_grid\\[",iss$stage)),has_native_nonzero=any(x$native_nonzero),
      diagnostics_incomplete=anyNA(x$numerical_anomaly)||any(x$native_diagnostic_incomplete))
  }))
  attach<-function(d){z<-merge(d,st[,c(ks,"numerical_anomaly","native_nonzero","native_diagnostic_incomplete"),drop=FALSE],by=ks,all.x=TRUE,sort=FALSE)
    merge(z,datasets,by=c("series_id","origin"),all.x=TRUE,sort=FALSE)}
  perpoint<-attach(perpoint);perinterval<-attach(perinterval)
  pairs<-sensitivity<-details<-list()
  addpairs<-function(d,metric,level=NA_real_){
    for(focal in cfg$focal_methods)for(ref in setdiff(cfg$methods,focal)){
      if(!is.na(level)&&ref=="BaggedETS_original")next
      plannedA<-st[st$method==focal,,drop=FALSE];plannedB<-st[st$method==ref,,drop=FALSE]
      if(!is.na(level)){plannedA<-plannedA[yes(plannedA$intervals_planned),,drop=FALSE];plannedB<-plannedB[yes(plannedB$intervals_planned),,drop=FALSE]}
      joint<-intersect(key(plannedA,c("series_id","origin")),key(plannedB,c("series_id","origin")))
      aa<-d[d$method==focal&is.finite(d[[metric]]),,drop=FALSE];bb<-d[d$method==ref&is.finite(d[[metric]]),,drop=FALSE]
      jointvalid<-sort(intersect(intersect(key(aa,c("series_id","origin")),key(bb,c("series_id","origin"))),joint))
      A<-aa[match(jointvalid,key(aa,c("series_id","origin"))),,drop=FALSE];B<-bb[match(jointvalid,key(bb,c("series_id","origin"))),,drop=FALSE]
      meta<-data.frame(focal=focal,reference=ref,metric=metric,level=level,n_jointly_scheduled=length(joint),n_unavailable_or_metric_undefined=length(joint)-length(jointvalid))
      main<-mean_stats(A[[metric]],B[[metric]],tie_abs,tie_rel);pairs[[length(pairs)+1L]]<<-cbind(meta,main)
      known<-!is.na(A$numerical_anomaly)&!is.na(B$numerical_anomaly)
      sourceflag<-yes(A$numerical_anomaly)|yes(B$numerical_anomaly)
      subsets<-list(either_method_source_flag=sourceflag,neither_method_source_flag_known=known&!sourceflag,
        dataset_source_issue=A$has_source_issue,no_dataset_source_issue_recorded=!A$has_source_issue&!A$diagnostics_incomplete,
        either_method_native_nonzero=A$native_nonzero|B$native_nonzero)
      for(sub in names(subsets)){take<-subsets[[sub]]%in%TRUE
        sensitivity[[length(sensitivity)+1L]]<<-cbind(meta,data.frame(subset=sub,analysis_role="descriptive_sensitivity_not_exclusion"),mean_stats(A[[metric]][take],B[[metric]][take],tie_abs,tie_rel))}
      if(length(jointvalid)){
        delta<-A[[metric]]-B[[metric]];tol<-tie_abs+tie_rel*pmax(abs(A[[metric]]),abs(B[[metric]]))
        details[[length(details)+1L]]<<-data.frame(series_id=A$series_id,origin=A$origin,focal=focal,reference=ref,metric=metric,level=level,
          focal_loss=A[[metric]],reference_loss=B[[metric]],difference=delta,outcome=ifelse(delta< -tol,"win",ifelse(delta>tol,"loss","tie")),
          focal_source_flag=A$numerical_anomaly,reference_source_flag=B$numerical_anomaly,
          dataset_source_issue=A$has_source_issue,grid_issue_dataset=A$has_grid_issue,
          focal_native_nonzero=A$native_nonzero,reference_native_nonzero=B$native_nonzero,
          signed_contribution_to_overall_mean=delta/length(delta),
          leave_one_out_mean=if(length(delta)>1)(sum(delta)-delta)/(length(delta)-1) else NA_real_)
      }
    }
  }
  for(v in c("MASE","MAE","MSE","RMSE","MAPE","sMAPE"))addpairs(perpoint,v)
  for(l in levels)for(v in c("IS","scaled_IS"))addpairs(perinterval[perinterval$level==l,,drop=FALSE],v,l)
  main<-bind(pairs);sens<-bind(sensitivity);detail<-bind(details)
  main$p_Holm_within_focal_metric_level<-NA_real_
  for(ix in groups(main,c("focal","metric","level"))){ok<-ix[is.finite(main$p_mean[ix])];if(length(ok))main$p_Holm_within_focal_metric_level[ok]<-p.adjust(main$p_mean[ok],"holm",n=length(ix))}
  # Reproduce original primary contrasts before interpreting added diagnostics.
  original_pairs<-read("paired_series_point.csv")
  compare<-main[is.na(main$level)&main$metric%in%unique(original_pairs$metric),,drop=FALSE]
  z<-merge(compare,original_pairs,by=c("focal","reference","metric"),suffixes=c("_audit","_source"),all=TRUE)
  must(nrow(z)==nrow(original_pairs)&&all(z$n_pairs==z$n_series)&&near(z$difference_audit,z$difference_source),"Original point contrasts not reproduced")
  original_ip<-read("paired_series_interval_scores.csv")
  z<-merge(main[!is.na(main$level),,drop=FALSE],original_ip,by=c("focal","reference","metric","level"),suffixes=c("_audit","_source"),all=TRUE)
  must(nrow(z)==nrow(original_ip)&&all(z$n_pairs==z$n_series)&&near(z$difference_audit,z$difference_source),"Original interval contrasts not reproduced")
  extremes<-bind(lapply(groups(detail,c("focal","reference","metric","level")),function(ix){x<-detail[ix,,drop=FALSE];n<-min(10L,nrow(x))
    good<-x[order(x$difference)[seq_len(n)],,drop=FALSE];good$extreme_type<-"largest_gains"
    bad<-x[order(x$difference,decreasing=TRUE)[seq_len(n)],,drop=FALSE];bad$extreme_type<-"largest_costs";rbind(good,bad)}))
  coverage_by_flags<-bind(lapply(groups(perinterval,c("method","level")),function(ix){x<-perinterval[ix,,drop=FALSE]
    bind(lapply(c("all_series","source_flag","no_source_flag_known"),function(label){
      take<-if(label=="all_series")rep(TRUE,nrow(x)) else if(label=="source_flag")yes(x$numerical_anomaly) else !is.na(x$numerical_anomaly)&!yes(x$numerical_anomaly)
      a<-x[take,,drop=FALSE]
      data.frame(method=x$method[1],level=x$level[1],subset=label,n_series=nrow(a),coverage=avg(a$coverage),
        mean_width=avg(a$mean_width),IS=avg(a$IS),scaled_IS=avg(a$scaled_IS))
    }))
  }))
  # Hypothetical IC perturbation in grid-flagged checkpoints only.
  grid_rows<-ni$issue=="finite_nonaccepted_candidate_beats_selected"&grepl("^MEB_grid\\[[0-9]+\\]/",ni$stage)
  grid_summary<-grid_candidates<-grid_members<-checkpoint_manifest<-list()
  for(ix in groups(ni[grid_rows,,drop=FALSE],c("series_id","origin"))){
    ng<-ni[grid_rows,,drop=FALSE][ix,,drop=FALSE];taskix<-match(key(ng[1,,drop=FALSE],c("series_id","origin")),key(tasks,c("series_id","origin")))
    must(!is.na(taskix),"Grid checkpoint absent from manifest")
    pf<-file.path(folder,paste0(tasks$task_id[taskix],"_fit.rds"))
    hb<-if(file.exists(pf))unname(tools::md5sum(pf)) else NA_character_
    ans<-tryCatch({
      must(file.exists(pf),"Checkpoint missing")
      saved<-readRDS(pf)
      must(identical(saved$protocol_hash,cfg$protocol_hash),"Foreign grid checkpoint")
      must(identical(as.character(saved$task$series_id),as.character(ng$series_id[1]))&&saved$task$origin==ng$origin[1],"Grid checkpoint identifier mismatch")
      Q<-saved$diagnostic$bootstrap_IC
      must(is.matrix(Q)&&nrow(Q)==cfg$B,"Saved B-by-K IC matrix unavailable")
      replica<-as.integer(sub("^MEB_grid\\[([0-9]+)\\]/.*$","\\1",ng$stage))
      model<-sub("^MEB_grid\\[[0-9]+\\]/","",ng$stage)
      must(all(model==ng$model),"Grid issue model/stage mismatch")
      g<-perturbed_grid(Q,replica,model,ng$gap)
      before<-names(which.min(g$mean_before));after<-names(which.min(g$mean_after))
      stored<-saved$status$selected_model[saved$status$method=="MEB_common"]
      mb<-colnames(Q)[apply(g$before,1L,which.min)];ma<-colnames(Q)[apply(g$after,1L,which.min)]
      ranked<-sort(g$mean_before);rankedp<-sort(g$mean_after)
      list(summary=data.frame(series_id=ng$series_id[1],origin=ng$origin[1],audit_status="hypothetical_sensitivity_completed",error="",n_issue_records=nrow(ng),
        saved_common_model=if(length(stored)==1)stored else NA_character_,recomputed_common_before=before,recomputed_common_after=after,
        saved_choice_reproduced=length(stored)==1&&identical(as.character(stored),as.character(before)),common_choice_changes=before!=after,
        margin_best_second_before=ranked[2]-ranked[1],margin_best_second_after=rankedp[2]-rankedp[1],n_replica_choices_changed=sum(mb!=ma)),
        candidates=data.frame(series_id=ng$series_id[1],origin=ng$origin[1],model=colnames(Q),mean_IC_before=as.numeric(g$mean_before),
          mean_IC_hypothetical=as.numeric(g$mean_after),mean_IC_shift=as.numeric(g$mean_after-g$mean_before)),
        members=data.frame(series_id=ng$series_id[1],origin=ng$origin[1],replica=seq_len(nrow(Q)),choice_before=mb,choice_hypothetical=ma,changed=mb!=ma))
    },error=function(e)list(summary=data.frame(series_id=ng$series_id[1],origin=ng$origin[1],audit_status="not_completed",error=conditionMessage(e),n_issue_records=nrow(ng),
      saved_common_model=NA_character_,recomputed_common_before=NA_character_,recomputed_common_after=NA_character_,saved_choice_reproduced=NA,
      common_choice_changes=NA,margin_best_second_before=NA_real_,margin_best_second_after=NA_real_,n_replica_choices_changed=NA_integer_)))
    if(file.exists(pf))must(identical(hb,unname(tools::md5sum(pf))),"Grid checkpoint changed during audit")
    checkpoint_manifest[[length(checkpoint_manifest)+1L]]<-data.frame(file=pf,md5=hb)
    grid_summary[[length(grid_summary)+1L]]<-ans$summary
    if(!is.null(ans$candidates))grid_candidates[[length(grid_candidates)+1L]]<-ans$candidates
    if(!is.null(ans$members))grid_members[[length(grid_members)+1L]]<-ans$members
  }
  grid_summary<-bind(grid_summary);grid_candidates<-bind(grid_candidates);grid_members<-bind(grid_members)
  # Full S sensitivity: every series, method, level and prefix, with explicit undefined ratios.
  need(ss,c(ks,"level","S","reference_S","max_bound_difference","max_bound_difference_over_training_SD"),"S sensitivity")
  unique_key(ss,c(ks,"level","S"),"S sensitivity")
  must(all(ss$reference_S==cfg$S)&&all(ss$S%in%cfg$S_grid[cfg$S_grid<cfg$S]),"Unexpected sensitivity budgets")
  expectedS<-perinterval[perinterval$method!="ETS_standard",c(ks,"level"),drop=FALSE]
  expectedS<-bind(lapply(cfg$S_grid[cfg$S_grid<cfg$S],function(s){d<-expectedS;d$S<-s;d}))
  must(setequal(key(ss,c(ks,"level","S")),key(expectedS,c(ks,"level","S"))),"Missing/extra S sensitivity records")
  must(all(is.finite(ss$max_bound_difference)&ss$max_bound_difference>=0)&&all(is.finite(ss$max_bound_difference_over_training_SD)),"Invalid saved sensitivity values")
  j<-match(key(ss,c(ks,"level")),key(perinterval,c(ks,"level")));must(!anyNA(j),"Reference width cannot be matched")
  ss$mean_reference_width<-perinterval$mean_width[j]
  ss$relative_width_defined<-is.finite(ss$mean_reference_width)&ss$mean_reference_width>0
  ss$max_bound_difference_pct_mean_width<-ifelse(ss$relative_width_defined,100*ss$max_bound_difference/ss$mean_reference_width,NA_real_)
  ss$max_bound_difference_pct_training_SD<-100*ss$max_bound_difference_over_training_SD
  Ssummary<-bind(lapply(groups(ss,c("method","level","S","reference_S")),function(ix){x<-ss[ix,,drop=FALSE]
    z<-x[1,c("method","level","S","reference_S"),drop=FALSE];z$n_series<-length(unique(x$series_id));z$n_records<-nrow(x)
    z$n_undefined_width_ratios<-sum(!x$relative_width_defined)
    for(v in c("max_bound_difference_pct_mean_width","max_bound_difference_pct_training_SD")){
      z[[paste0(v,"_median")]]<-qval(x[[v]],.5);z[[paste0(v,"_p90")]]<-qval(x[[v]],.9)
      z[[paste0(v,"_p95")]]<-qval(x[[v]],.95);z[[paste0(v,"_max")]]<-if(any(is.finite(x[[v]])))max(x[[v]][is.finite(x[[v]])]) else NA_real_
    };z
  }))
  Sworst<-bind(lapply(groups(ss,c("method","level","S")),function(ix){x<-ss[ix,,drop=FALSE];o<-order(x$max_bound_difference_pct_mean_width,decreasing=TRUE,na.last=TRUE)
    x[o[seq_len(min(5L,length(o)))],,drop=FALSE]}))
  native_summary<-data.frame(method="ETS_standard",n_codes=nrow(st[st$method=="ETS_standard",]),
    code0=sum(st$method=="ETS_standard"&st$native_code%in%0),code1=sum(st$method=="ETS_standard"&st$native_code%in%1),code10=sum(st$method=="ETS_standard"&st$native_code%in%10),
    unknown=sum(st$method=="ETS_standard"&!is.finite(st$native_code)),other=sum(st$method=="ETS_standard"&is.finite(st$native_code)&!(st$native_code%in%c(0,1,10))))
  native_summary<-rbind(native_summary,data.frame(method="BaggedETS_original_members",n_codes=nrow(bc),code0=sum(bc$code%in%0),code1=sum(bc$code%in%1),code10=sum(bc$code%in%10),unknown=sum(!is.finite(bc$code)),other=sum(is.finite(bc$code)&!(bc$code%in%c(0,1,10)))))
  checks<-data.frame(check=c("original_point_mean_contrasts_reproduced","original_interval_mean_contrasts_reproduced","all_S_prefix_records_present","source_inputs_unchanged"),passed=TRUE)
  must(identical(unname(hash_before),unname(tools::md5sum(paths))),"Source inputs changed during audit")
  dir.create(OUTPUT_ROOT,recursive=TRUE,showWarnings=FALSE)
  destination<-file.path(OUTPUT_ROOT,paste0("audit_",format(started,"%Y%m%d_%H%M%S"),"_",Sys.getpid()))
  must(!dir.exists(destination)&&dir.create(destination,recursive=TRUE),"Cannot create fresh audit directory")
  destination<-normalizePath(destination,winslash="/",mustWork=TRUE)
  outputs<-list(integrity_checks=checks,series_issue_index=datasets,point_metrics_recomputed=perpoint,interval_metrics_recomputed=perinterval,
    all_pairwise_effects_and_influence=main,descriptive_flag_sensitivity=sens,paired_series_details=detail,
    largest_gains_and_costs=extremes,coverage_by_method_source_flags=coverage_by_flags,numerical_issue_records=ni,
    grid_hypothetical_sensitivity=grid_summary,grid_candidate_IC_perturbations=grid_candidates,grid_member_choice_perturbations=grid_members,
    S_sensitivity_all_series=ss,S_sensitivity_full_summary=Ssummary,S_largest_relative_differences=Sworst,native_code_summary=native_summary)
  for(n in names(outputs))if(ncol(outputs[[n]]))write.csv(outputs[[n]],file.path(destination,paste0(n,".csv")),row.names=FALSE,na="NA")
  implementation<-c(
    "FINAL EMPIRICAL DIAGNOSTICS -- existing outputs only",
    "No engine sourced; no model refitted; no bootstrap, new future simulation, calibration or parameter modification.",
    "Main results retain all valid jointly available series. Source-flag and native-code subsets are descriptive sensitivities, not exclusion rules.",
    "An empirical series is the inferential unit. Series t tests and Wilcoxon assume conditions not guaranteed by shared tourism shocks.",
    "Mean-loss intervals concern a mean contrast. Wilcoxon is an approximate signed-rank test, not a generic test of equal means.",
    "Tie tolerances are inherited from the saved protocol; mean contrasts use raw, not tolerance-rounded, losses.",
    "Win/loss decomposition keeps losses in both directions and the signed contribution of ties. Majority wins do not imply a lower mean risk.",
    "Leave-one-out results report mean-sign stability, NOT leave-one-out significance or corrected confidence intervals.",
    "Holm sensitivities are within focal/metric/level reference families, not across every statistic in this audit.",
    "Likelihood source issues can indicate an inadequate numerical reference; they do not prove that MESE improves the maximum likelihood.",
    "Native ETS and Bagged ETS are unchanged. Native code 1/10 is neither an automatic missing forecast nor an automatic competitor defeat.",
    "Grid scenario: subtract 2*recorded_loglik_gap from the candidate IC in the same bootstrap replication, holding n and parameter count fixed.",
    "This uses recorded finite nonaccepted objectives as a hypothetical stress test, NOT as valid corrected or converged fits.",
    "Only flagged candidate cells are perturbed; unrecorded optimization error is not bounded. Stable choices under this scenario do not certify global optimality.",
    "No forecasts, coefficients, bootstrap ensembles or published comparison losses are replaced by hypothetical IC values.",
    "S sensitivity compares stored nested prefixes to the finite reference budget. It is not a quantile MCSE or proof of convergence.",
    "Relative endpoint differences divide the largest recorded bound shift by mean reference width over H, not width at the affected endpoint.",
    "Undefined width ratios are counted explicitly. No precision pass/fail threshold is imposed after viewing the results.",
    "Fixed B, empirical residual future innovations, plug-in versus mixture labels and actual S must be stated separately in the manuscript.",
    "This code was not executed in R by the assistant. Deterministic checks and source-consistency assertions run locally."
  )
  writeLines(implementation,file.path(destination,"IMPLEMENTATION_NOTE.txt"))
  writeLines(capture.output(dput(cfg)),file.path(destination,"executed_configuration.txt"))
  write.csv(data.frame(file=paths,md5=unname(hash_before)),file.path(destination,"source_manifest.csv"),row.names=FALSE)
  if(length(checkpoint_manifest))write.csv(bind(checkpoint_manifest),file.path(destination,"grid_checkpoint_manifest.csv"),row.names=FALSE)
  saveRDS(list(source=folder,configuration=cfg,started=started,finished=Sys.time(),session=sessionInfo()),file.path(destination,"audit_metadata.rds"))
  focus_main<-main[main$metric%in%c("MASE","scaled_IS"),,drop=FALSE]
  focus_sens<-sens[sens$metric%in%c("MASE","scaled_IS")&sens$subset%in%c("either_method_source_flag","neither_method_source_flag_known","no_dataset_source_issue_recorded"),,drop=FALSE]
  important_extremes<-extremes[extremes$focal=="MESE_original"&((extremes$metric=="scaled_IS"&extremes$level==.95)|extremes$metric=="MASE")&extremes$reference%in%c("ETS_standard","BaggedETS_original","MEB_original","MEB_per_replica"),,drop=FALSE]
  report_tables<-list(integrity_checks=checks,primary_loss_influence=focus_main,primary_source_flag_sensitivity=focus_sens,
    important_largest_gains_costs=important_extremes,grid_sensitivity=grid_summary,grid_candidate_scores=grid_candidates,
    S_full_summary=Ssummary,S_largest_differences=Sworst,native_codes=native_summary,coverage_by_flags=coverage_by_flags)
  report<-file.path(destination,"FINAL_EMPIRICAL_DIAGNOSTICS.txt");con<-file(report,open="wt",encoding="UTF-8")
  tryCatch({writeLines(c(implementation,"",capture.output(dput(cfg)),""),con)
    for(n in names(report_tables))if(ncol(report_tables[[n]])){
      writeLines(paste0("===== BEGIN ",n,".csv ====="),con)
      write.table(report_tables[[n]],con,sep=",",row.names=FALSE,col.names=TRUE,na="NA",qmethod="double")
      writeLines(c(paste0("===== END ",n,".csv ====="),""),con)
    }
  },finally=close(con))
  must(identical(unname(hash_before),unname(tools::md5sum(paths))),"Source files changed while writing output")
  cat("\nRead-only final diagnostics complete. No fitting or new simulation.\n")
  cat("Output:",destination,"\nReport:",report,"\n")
  cat("All original series remain in the primary available-pair analysis.\n")
  displays<-list(overview=list(checks=checks,source_issue_series=with(datasets,table(has_source_issue)),native_codes=native_summary),
    influence=list(primary_loss_influence=focus_main),flags=list(primary_source_flag_sensitivity=focus_sens),
    tails=list(largest_gains_and_costs=important_extremes),grid=list(hypothetical_sensitivity=grid_summary,candidate_scores=grid_candidates),
    S=list(full_S_summary=Ssummary,worst_S_differences=Sworst),implementation=list(implementation_note=implementation))
  function(section="overview"){
    must(length(section)==1&&section%in%names(displays),paste("Choose:",paste(names(displays),collapse=", ")))
    old<-options(width=190,max.print=1000000);on.exit(options(old),add=TRUE)
    cat("\n========== FINAL EMPIRICAL AUDIT:",section,"==========\n")
    for(n in names(displays[[section]])){
      cat("\n---",n,"---\n");d<-displays[[section]][[n]]
      if(is.data.frame(d))print(d,row.names=FALSE,digits=7,max=1000000) else print(d)
    };invisible(NULL)
  }
})

mostra_audit_finale("overview")


mostra_audit_finale("grid")
mostra_audit_finale("S")
mostra_audit_finale("influence")
mostra_audit_finale("flags")
