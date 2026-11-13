# AUDIT_MESE_v7_PER_REPLICA_FINAL.R
# Audit of stored results only. Does NOT correct fits or certify publication readiness.
FOLDER <- "MESE_v7_validation"
AUDIT_ROOT <- "MESE_v7_audit"
FOCAL_METHODS <- c("MESE_original", "MESE_MEB_selection")
SCAN_CHECKPOINTS <- TRUE
AUDIT_INTERVALS <- TRUE
HORIZONS_TO_AUDIT <- c(1L,6L,12L)
TIE_ABS <- 1e-10
TIE_REL <- 1e-8
CHECK_ABS <- 1e-8
CHECK_REL <- 1e-8

local({
  version <- "v7_audit_separated_diagnostics_1"
  started <- Sys.time()
  must <- function(ok,msg) if(!isTRUE(ok)) stop(msg,call.=FALSE)
  bind <- function(x) {x<-Filter(function(z)!is.null(z)&&nrow(z)>0L,x);if(!length(x)) return(data.frame());z<-do.call(rbind,x);rownames(z)<-NULL;z}
  key <- function(d,cols) {if(!nrow(d)) return(character());do.call(paste,c(lapply(d[,cols,drop=FALSE],function(x)ifelse(is.na(x),"<NA>",as.character(x))),sep="\035"))}
  groups <- function(d,cols) if(nrow(d)) split(seq_len(nrow(d)),key(d,cols)) else list()
  need <- function(d,cols,label) must(all(cols%in%names(d)),paste(label,"missing",paste(setdiff(cols,names(d)),collapse=", ")))
  unique_key <- function(d,cols,label) must(!anyDuplicated(key(d,cols)),paste("Duplicate key:",label))
  flag <- function(x,label) {
    a<-toupper(trimws(as.character(x)));z<-rep(NA,length(a));z[a%in%c("TRUE","T","1")]<-TRUE;z[a%in%c("FALSE","F","0")]<-FALSE
    must(all(is.na(a)|a%in%c("","NA","TRUE","T","1","FALSE","F","0")),paste("Invalid logical:",label));z
  }
  near <- function(a,b) length(a)==length(b)&&all(is.finite(a))&&all(is.finite(b))&&all(abs(a-b)<=CHECK_ABS+CHECK_REL*pmax(abs(a),abs(b)))
  wilson <- function(k,n) {
    if(!n) return(c(NA_real_,NA_real_));z<-qnorm(.975);p<-k/n;den<-1+z*z/n
    c(max(0,(p+z*z/(2*n)-z*sqrt(p*(1-p)/n+z*z/(4*n*n)))/den),min(1,(p+z*z/(2*n)+z*sqrt(p*(1-p)/n+z*z/(4*n*n)))/den))
  }
  pair_stats <- function(a,b) {
    must(length(a)==length(b)&&all(is.finite(a))&&all(is.finite(b)),"Invalid paired losses")
    n<-length(a);d<-a-b;tol<-TIE_ABS+TIE_REL*pmax(abs(a),abs(b));w<-sum(d< -tol);l<-sum(d>tol);ties<-n-w-l
    mu<-if(n)mean(d) else NA_real_;se<-if(n>1L)sd(d)/sqrt(n) else NA_real_;low<-high<-pt<-pw<-NA_real_
    status<-"fewer_than_two_pairs"
    if(n>1L) {
      if(is.finite(se)&&se>0) {low<-mu-qt(.975,n-1)*se;high<-mu+qt(.975,n-1)*se;pt<-2*stats::pt(-abs(mu/se),n-1);status<-"paired_t_MC_approximation"}
      else {low<-high<-mu;status<-"zero_observed_variance_no_t_test"}
      if(any(d!=0)) pw<-suppressWarnings(tryCatch(wilcox.test(d,mu=0,exact=FALSE,correct=TRUE)$p.value,error=function(e)NA_real_))
    }
    ci<-wilson(w,n);qs<-if(n)quantile(d,c(.05,.5,.95),type=8,names=FALSE) else rep(NA_real_,3)
    data.frame(n_pairs=n,mean_focal=if(n)mean(a) else NA_real_,mean_reference=if(n)mean(b) else NA_real_,
               mean_difference=mu,MCSE_difference=se,MC_lower95=low,MC_upper95=high,
               relative_mean_loss_change_pct=if(n&&mean(b)>0)100*(mean(a)/mean(b)-1) else NA_real_,
               median_difference=qs[2],q05_difference=qs[1],q95_difference=qs[3],wins=w,ties=ties,losses=l,
               win_pct=if(n)100*w/n else NA_real_,tie_pct=if(n)100*ties/n else NA_real_,loss_pct=if(n)100*l/n else NA_real_,
               half_tie_credit_pct=if(n)100*(w+ties/2)/n else NA_real_,win_pct_excluding_ties=if(w+l)100*w/(w+l) else NA_real_,
               win_Wilson_lower95_pct=100*ci[1],win_Wilson_upper95_pct=100*ci[2],
               p_mean_two_sided=pt,p_sign_excluding_ties_two_sided=if(w+l)binom.test(w,w+l,p=.5)$p.value else NA_real_,
               p_Wilcoxon_approx_unadjusted=pw,test_status=status,row.names=NULL)
  }
  pool <- function(d,id,values,H) bind(lapply(groups(d,id),function(ix){
    x<-d[ix,,drop=FALSE];z<-x[1,id,drop=FALSE]
    valid<-nrow(x)==H&&setequal(x$horizon,seq_len(H))&&all(vapply(x[,values,drop=FALSE],function(v)all(is.finite(v)),logical(1)))
    z$n_horizons<-nrow(x);z$valid_horizons<-valid
    for(v in values)z[[v]]<-if(valid)mean(x[[v]]) else NA_real_;z
  }))
  toy<-pair_stats(c(1,3,2,4),c(2,2,2,5));must(toy$wins==2&&toy$ties==1&&toy$losses==1&&abs(toy$mean_difference+.25)<1e-12,"Pairing test failed")
  must(pair_stats(numeric(),numeric())$n_pairs==0,"Empty test failed")
  must(is.na(pair_stats(rep(2,4),rep(2,4))$p_mean_two_sided),"Constant test failed")
  must(wilson(0,4)[2]>0&&wilson(4,4)[1]<1,"Wilson test failed")
  must(all(is.finite(c(TIE_ABS,TIE_REL,CHECK_ABS,CHECK_REL)))&&all(c(TIE_ABS,TIE_REL,CHECK_ABS,CHECK_REL)>=0),"Invalid tolerances")
  must(dir.exists(FOLDER),"Validation folder missing")
  folder<-normalizePath(FOLDER,winslash="/",mustWork=TRUE);cp<-file.path(folder,"configuration.rds")
  must(file.exists(cp),"configuration.rds missing");cfg<-readRDS(cp)
  must(identical(cfg$mode,"validation"),"Use validation, not precision results")
  H<-as.integer(cfg$H);must(length(H)==1&&is.finite(H)&&H>=1&&H==cfg$H,"Invalid H")
  must(length(cfg$cases)>0&&!anyDuplicated(cfg$cases)&&cfg$R>=1&&cfg$R==as.integer(cfg$R),"Invalid schedule")
  must(length(cfg$levels)>0&&all(is.finite(cfg$levels))&&all(cfg$levels>0&cfg$levels<1)&&!anyDuplicated(cfg$levels),"Invalid levels")
  must(all(HORIZONS_TO_AUDIT%in%seq_len(H)),"Invalid requested horizon")
  must(length(FOCAL_METHODS)>0&&!anyDuplicated(FOCAL_METHODS),"Invalid focal methods")
  source_paths<-c(cp,list.files(folder,pattern="\\.csv$",full.names=TRUE));hash_before<-tools::md5sum(source_paths)
  read_csv<-function(n,required=TRUE){p<-file.path(folder,n);if(!file.exists(p)){if(required)stop("Missing ",n,call.=FALSE);return(NULL)};read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)}
  st<-read_csv("status.csv");ev<-read_csv("point_events.csv");ni<-read_csv("numerical_issues.csv",FALSE);issues_available<-!is.null(ni)
  if(is.null(ni)||!nrow(ni))ni<-data.frame(case=character(),replication=integer(),stage=character(),model=character(),issue=character(),gap=double(),tolerance=double())
  ks<-c("case","replication","method")
  need(st,c(ks,"fit_success","numerical_anomaly","intervals_planned","interval_success"),"status")
  need(ev,c(ks,"horizon","actual","forecast","AE","SE"),"point events")
  need(ni,c("case","replication","stage","issue","gap","tolerance"),"issues")
  for(n in c("fit_success","numerical_anomaly","intervals_planned","interval_success"))st[[n]]<-flag(st[[n]],n)
  must(!anyNA(st[,ks])&&!anyNA(ev[,c(ks,"horizon")]),"Missing identifiers")
  unique_key(st,ks,"status");unique_key(ev,c(ks,"horizon"),"point events")
  must(all(ev$horizon%in%seq_len(H)),"Invalid event horizons")
  must(near(ev$AE,abs(ev$actual-ev$forecast))&&near(ev$SE,(ev$actual-ev$forecast)^2),"Stored losses inconsistent")
  must(all(vapply(groups(ev,c("case","replication","horizon")),function(ix)length(unique(ev$actual[ix]))==1,logical(1))),"Different actuals between methods")
  schedule<-function(r)c("ETS_standard","ETS_controlled","Akaike_controlled","MEB_original","MESE_original",
                         if(length(cfg$B_grid)>1)unlist(lapply(cfg$B_grid[cfg$B_grid<cfg$B],function(b)c(paste0("MEB_original_B",b),paste0("MESE_original_B",b)))),
                         if(r<=cfg$R_bag)"BaggedETS_original",if(r<=cfg$R_ablation)c("MEB_common","MEB_per_replica","MESE_MEB_selection","BLD_common_ablation"))
  expected<-bind(lapply(cfg$cases,function(ca)bind(lapply(seq_len(cfg$R),function(r)data.frame(case=ca,replication=r,method=schedule(r))))))
  must(all(key(st,ks)%in%key(expected,ks)),"Status outside schedule")
  st$status_present<-TRUE;st<-merge(expected,st,by=ks,all.x=TRUE,sort=FALSE);st$status_present<-st$status_present%in%TRUE
  ei<-match(key(ev,ks),key(st,ks));must(!anyNA(ei)&&all(st$fit_success[ei]%in%TRUE),"Events for unscheduled/unsuccessful fits")
  must(all(FOCAL_METHODS%in%st$method),"Unknown focal method")
  if(nrow(ni))must(!anyNA(ni[,c("case","replication","stage","issue")])&&all(key(ni,c("case","replication"))%in%key(expected,c("case","replication"))),"Invalid issue identifiers")
  dir.create(AUDIT_ROOT,recursive=TRUE,showWarnings=FALSE)
  destination<-file.path(AUDIT_ROOT,paste0("audit_final_",format(started,"%Y%m%d_%H%M%S"),"_",Sys.getpid()))
  must(!dir.exists(destination)&&dir.create(destination,recursive=TRUE),"Cannot create fresh audit folder")
  destination<-normalizePath(destination,winslash="/",mustWork=TRUE)
  write_table<-function(d,n)if(is.data.frame(d)&&ncol(d))write.csv(d,file.path(destination,n),row.names=FALSE,na="NA")
  st$native_diagnostic_applicable<-st$method%in%c("ETS_standard","BaggedETS_original")
  st$native_diagnostic_scanned<-FALSE;st$native_nonzero_count<-0L;st$native_unknown_count<-0L
  st$native_diagnostic_incomplete<-st$native_diagnostic_applicable&st$fit_success%in%TRUE
  checkpoints<-parameters<-list()
  codes_numeric<-function(x){if(is.null(x))return(numeric());if(!is.atomic(x))return(rep(NA_real_,length(x)));v<-suppressWarnings(as.numeric(x));v[!is.finite(v)|v!=round(v)]<-NA_real_;v}
  if(isTRUE(SCAN_CHECKPOINTS))for(ca in cfg$cases)for(r in seq_len(cfg$R)){
    pf<-file.path(folder,sprintf("%s_%05d_fit.rds",ca,r));exists<-file.exists(pf)
    hb<-if(exists)unname(tools::md5sum(pf)) else NA_character_
    z<-if(exists)tryCatch(readRDS(pf),error=function(e)e) else NULL
    readable<-!is.null(z)&&!inherits(z,"error");fatal<-readable&&isTRUE(z$fatal)
    if(exists)must(identical(hb,unname(tools::md5sum(pf))),paste("Checkpoint changed during read:",pf))
    if(readable)must(identical(as.character(z$case),as.character(ca))&&length(z$replication)==1&&z$replication==r,"Checkpoint identifier mismatch")
    nc<-bc<-numeric();members<-NA_integer_
    if(readable&&!fatal){
      nc<-codes_numeric(z$diagnostic$standard_code);bc<-codes_numeric(z$diagnostic$bagged_codes)
      members<-sum(vapply(z$methods,function(m)!is.null(m$members),logical(1)))
      pr<-z$diagnostic$primary
      if(!is.null(pr$bootstrap_coefficients)){
        C<-as.matrix(pr$bootstrap_coefficients)
        for(j in seq_len(nrow(C))){v<-as.numeric(C[j,]);v<-v[is.finite(v)]
        parameters[[length(parameters)+1L]]<-data.frame(case=ca,replication=r,model=pr$model,
                                                        parameter=if(!is.null(rownames(C)))rownames(C)[j] else paste0("parameter_",j),
                                                        n_bootstrap=ncol(C),n_finite=length(v),mean=if(length(v))mean(v) else NA_real_,
                                                        median=if(length(v))median(v) else NA_real_,sd=if(length(v)>1)sd(v) else NA_real_,
                                                        minimum=if(length(v))min(v) else NA_real_,maximum=if(length(v))max(v) else NA_real_)
        }
      }
    }
    for(nm in c("ETS_standard","BaggedETS_original")){
      ix<-which(st$case==ca&st$replication==r&st$method==nm)
      if(!length(ix))next
      cc<-if(nm=="ETS_standard")nc else bc
      expected_count<-if(nm=="ETS_standard")1L else as.integer(cfg$B)
      st$native_diagnostic_scanned[ix]<-readable&&!fatal
      st$native_nonzero_count[ix]<-sum(!is.na(cc)&cc!=0)
      st$native_unknown_count[ix]<-sum(is.na(cc))
      st$native_diagnostic_incomplete[ix]<-st$fit_success[ix]%in%TRUE&&(!readable||fatal||length(cc)!=expected_count||anyNA(cc))
    }
    checkpoints[[length(checkpoints)+1L]]<-data.frame(case=ca,replication=r,exists=exists,readable=readable,fatal=fatal,
                                                      standard_code=if(length(nc)==1)nc else NA_real_,bagged_code_count=length(bc),
                                                      bagged_members_nonzero=sum(!is.na(bc)&bc!=0),bagged_members_unknown=sum(is.na(bc)),
                                                      methods_with_retained_members=members,md5=hb)
  }
  st$native_nonzero<-st$native_nonzero_count>0L
  datasets<-bind(lapply(groups(st,c("case","replication")),function(ix){
    x<-st[ix,,drop=FALSE];iss<-ni[ni$case==x$case[1]&ni$replication==x$replication[1],,drop=FALSE]
    src<-nrow(iss)>0||any(x$numerical_anomaly%in%TRUE);native<-any(x$native_nonzero)
    incomplete<-!issues_available||any(!x$status_present)||anyNA(x$numerical_anomaly)||any(x$native_diagnostic_incomplete)
    challenged<-nrow(iss)>0&&any(grepl("^original/",iss$stage)|(iss$issue=="parameter_refit_beats_corresponding_free_fit"&iss$stage%in%c("MESE_original","MESE_MEB_selection")))
    data.frame(case=x$case[1],replication=x$replication[1],n_issue_records=nrow(iss),
               n_source_flagged_methods=sum(x$numerical_anomaly%in%TRUE),has_source_issue=src,has_native_nonzero=native,
               n_methods_native_nonzero=sum(x$native_nonzero),audit_information_incomplete=incomplete,
               original_reference_challenged=challenged,
               issue_origin=if(src&&native)"source_and_native" else if(src)"source_only" else if(native)"native_only" else "neither_recorded",
               max_recorded_gap=if(nrow(iss)&&any(is.finite(iss$gap)))max(iss$gap[is.finite(iss$gap)]) else NA_real_)
  }))
  attach_status<-function(d){
    f<-c(ks,"fit_success","numerical_anomaly","status_present","native_nonzero","native_nonzero_count","native_unknown_count","native_diagnostic_incomplete")
    z<-merge(d,st[,f,drop=FALSE],by=ks,all.x=TRUE,sort=FALSE)
    merge(z,datasets,by=c("case","replication"),all.x=TRUE,sort=FALSE)
  }
  pp<-attach_status(pool(ev,ks,c("AE","SE"),H));pp$valid_result<-pp$valid_horizons&pp$fit_success%in%TRUE
  availability<-merge(st,pp[,c(ks,"valid_horizons","n_horizons")],by=ks,all.x=TRUE,sort=FALSE)
  availability$point_integrity_problem<-availability$fit_success%in%TRUE&!(availability$valid_horizons%in%TRUE)
  point_summary<-bind(lapply(groups(pp,c("case","method")),function(ix){x<-pp[ix,,drop=FALSE];x<-x[x$valid_result,,drop=FALSE];if(!nrow(x))return(NULL)
  mse<-mean(x$SE);se<-if(nrow(x)>1)sd(x$SE)/sqrt(nrow(x)) else NA_real_
  data.frame(case=x$case[1],method=x$method[1],n=nrow(x),MAE=mean(x$AE),MCSE_MAE=if(nrow(x)>1)sd(x$AE)/sqrt(nrow(x)) else NA_real_,
             MSE=mse,MCSE_MSE=se,pooled_RMSE=sqrt(mse),MCSE_pooled_RMSE=if(mse>0)se/(2*sqrt(mse)) else NA_real_)
  }))
  summaries<-details<-list()
  pairs<-function(d,metric,scope="pooled_H",horizon=0L,level=NA_real_){
    d<-d[d$valid_result%in%TRUE&is.finite(d[[metric]]),,drop=FALSE]
    for(ca in cfg$cases)for(focal in FOCAL_METHODS)for(ref in setdiff(unique(st$method[st$case==ca]),focal)){
      scheduled<-st[st$case==ca,,drop=FALSE]
      if(metric=="IS")scheduled<-scheduled[scheduled$intervals_planned%in%TRUE,,drop=FALSE]
      eligible<-intersect(scheduled$replication[scheduled$method==focal],scheduled$replication[scheduled$method==ref]);if(!length(eligible))next
      aa<-d[d$case==ca&d$method==focal&d$replication%in%eligible,,drop=FALSE]
      bb<-d[d$case==ca&d$method==ref&d$replication%in%eligible,,drop=FALSE]
      must(!anyDuplicated(aa$replication)&&!anyDuplicated(bb$replication),"Duplicate pair replica")
      rr<-sort(intersect(aa$replication,bb$replication));a<-aa[match(rr,aa$replication),,drop=FALSE];b<-bb[match(rr,bb$replication),,drop=FALSE]
      ownsrc<-(a$numerical_anomaly%in%TRUE)|(b$numerical_anomaly%in%TRUE)
      ownnative<-a$native_nonzero|b$native_nonzero
      ownunknown<-is.na(a$numerical_anomaly)|is.na(b$numerical_anomaly)|a$native_diagnostic_incomplete|b$native_diagnostic_incomplete
      subsets<-list(all_available_pairs=rep(TRUE,length(rr)),either_method_source_flag=ownsrc,
                    neither_method_source_flag_observed=!ownsrc&!is.na(a$numerical_anomaly)&!is.na(b$numerical_anomaly),
                    either_method_native_nonzero=ownnative,reference_challenged_dataset=a$original_reference_challenged,
                    no_dataset_source_issue_recorded=!a$has_source_issue & !a$audit_information_incomplete)
      for(sub in names(subsets)){
        take<-subsets[[sub]]%in%TRUE
        meta<-data.frame(case=ca,focal=focal,reference=ref,metric=metric,scope=scope,horizon=horizon,level=level,subset=sub,
                         analysis_role=if(sub=="all_available_pairs")"primary_available_pairs" else "descriptive_sensitivity_only",
                         n_jointly_scheduled=length(eligible),n_focal_available=nrow(aa),n_reference_available=nrow(bb),n_both_available=length(rr),
                         n_not_both_available=length(eligible)-length(rr),n_pairs_source_flag=sum(ownsrc),n_pairs_native_nonzero=sum(ownnative),n_pairs_diagnostic_incomplete=sum(ownunknown))
        summaries[[length(summaries)+1L]]<<-cbind(meta,pair_stats(a[[metric]][take],b[[metric]][take]))
      }
      if(length(rr)&&scope=="pooled_H"){
        delta<-a[[metric]]-b[[metric]];tol<-TIE_ABS+TIE_REL*pmax(abs(a[[metric]]),abs(b[[metric]]))
        details[[length(details)+1L]]<<-data.frame(case=ca,replication=rr,focal=focal,reference=ref,metric=metric,level=level,
                                                   focal_loss=a[[metric]],reference_loss=b[[metric]],difference=delta,
                                                   outcome=ifelse(delta< -tol,"win",ifelse(delta>tol,"loss","tie")),tie_tolerance=tol,
                                                   focal_source_flag=a$numerical_anomaly,reference_source_flag=b$numerical_anomaly,
                                                   focal_native_nonzero=a$native_nonzero,reference_native_nonzero=b$native_nonzero,
                                                   pair_diagnostic_incomplete=ownunknown,dataset_issue_origin=a$issue_origin,original_reference_challenged=a$original_reference_challenged,
                                                   signed_contribution_to_mean=delta/length(rr),leave_one_out_mean=if(length(rr)>1)(sum(delta)-delta)/(length(rr)-1) else NA_real_)
      }
    }
  }
  pairs(pp,"AE");pairs(pp,"SE")
  for(h in HORIZONS_TO_AUDIT){d<-attach_status(ev[ev$horizon==h,,drop=FALSE]);d$valid_result<-d$fit_success%in%TRUE;pairs(d,"AE","specific_horizon",h);pairs(d,"SE","specific_horizon",h)}
  interval_pool<-interval_summary<-budgets<-interval_availability<-data.frame()
  if(isTRUE(AUDIT_INTERVALS)){
    ie<-read_csv("interval_events.csv");need(ie,c(ks,"level","horizon","S","lower","upper","covered","width","IS","interval_construction"),"interval events")
    must(!anyNA(ie[,c(ks,"level","horizon","interval_construction")]),"Missing interval identifiers")
    must(all(ie$horizon%in%seq_len(H))&&all(vapply(ie$level,function(l)any(abs(l-cfg$levels)<1e-10),logical(1))),"Invalid interval horizon/level")
    ie$level<-vapply(ie$level,function(l)cfg$levels[which.min(abs(cfg$levels-l))],numeric(1))
    unique_key(ie,c(ks,"level","horizon"),"interval events")
    must(all(is.finite(ie$lower))&&all(is.finite(ie$upper))&&all(ie$lower<=ie$upper),"Invalid bounds")
    target<-unique(ev[,c("case","replication","horizon","actual")]);ix<-match(key(ie,c("case","replication","horizon")),key(target,c("case","replication","horizon")))
    must(!anyNA(ix),"Missing interval actuals");y<-target$actual[ix];alpha<-1-ie$level
    must(near(ie$width,ie$upper-ie$lower)&&near(ie$covered,as.numeric(y>=ie$lower&y<=ie$upper))&&
           near(ie$IS,ie$upper-ie$lower+(2/alpha)*(pmax(ie$lower-y,0)+pmax(y-ie$upper,0))),"Interval score inconsistency")
    ix<-match(key(ie,ks),key(st,ks));must(!anyNA(ix)&&all(st$intervals_planned[ix]%in%TRUE)&all(st$interval_success[ix]%in%TRUE),"Unscheduled/unsuccessful interval events")
    budgets<-unique(ie[,c("method","interval_construction","S")]);budgets$is_native<-budgets$method=="ETS_standard"
    budgets$matches_config<-ifelse(budgets$is_native,is.na(budgets$S),!is.na(budgets$S)&budgets$S==cfg$S)
    must(all(budgets$matches_config),"Interval budgets do not match configuration/native convention")
    for(g in groups(ie,c(ks,"level")))must(length(unique(ie$interval_construction[g]))==1,"Mixed construction within one result")
    interval_pool<-attach_status(pool(ie,c(ks,"level","interval_construction"),c("covered","width","IS"),H));interval_pool$valid_result<-interval_pool$valid_horizons&interval_pool$fit_success%in%TRUE
    for(l in cfg$levels)pairs(interval_pool[interval_pool$level==l,,drop=FALSE],"IS",level=l)
    interval_summary<-bind(lapply(groups(interval_pool,c("case","method","level","interval_construction")),function(ix){
      x<-interval_pool[ix,,drop=FALSE];x<-x[x$valid_result,,drop=FALSE];if(!nrow(x))return(NULL)
      z<-x[1,c("case","method","level","interval_construction"),drop=FALSE];z$n<-nrow(x)
      for(v in c("covered","width","IS")){z[[v]]<-mean(x[[v]]);z[[paste0("MCSE_",v)]]<-if(nrow(x)>1)sd(x[[v]])/sqrt(nrow(x)) else NA_real_}
      z$coverage_gap_pp<-100*(z$covered-z$level);z
    }))
    expected_i<-st[st$intervals_planned%in%TRUE,c(ks,"interval_success"),drop=FALSE]
    interval_availability<-bind(lapply(cfg$levels,function(l){d<-expected_i;d$level<-l;d}))
    ix<-match(key(interval_availability,c(ks,"level")),key(interval_pool,c(ks,"level")))
    interval_availability$complete_events<-!is.na(ix)&(interval_pool$valid_result[ix]%in%TRUE)
  }
  contrasts<-bind(summaries);detail<-bind(details)
  for(pn in c("p_mean_two_sided","p_sign_excluding_ties_two_sided")){
    new<-paste0(pn,"_Holm_across_DGP");contrasts[[new]]<-NA_real_
    primary<-which(contrasts$subset=="all_available_pairs")
    fam<-contrasts[primary,c("focal","reference","metric","scope","horizon","level"),drop=FALSE]
    for(ix in groups(fam,names(fam))){rows<-primary[ix];rows<-rows[is.finite(contrasts[[pn]][rows])]
    if(length(rows))contrasts[[new]][rows]<-p.adjust(contrasts[[pn]][rows],method="holm",n=length(cfg$cases))}
  }
  integrity<-data.frame(check=c("all_scheduled_status_rows_present","all_successful_points_complete","all_successful_intervals_complete"),
                        passed=c(all(st$status_present),!any(availability$point_integrity_problem),
                                 if(isTRUE(AUDIT_INTERVALS))!any(interval_availability$interval_success%in%TRUE&!interval_availability$complete_events) else NA))
  old<-read_csv("point_summary.csv",FALSE)
  if(!is.null(old)){
    a<-old[old$horizon==0,c("case","method","R_complete","AE","SE"),drop=FALSE]
    m<-merge(a,point_summary,by=c("case","method"),all=TRUE)
    ok<-nrow(m)==nrow(a)&&!anyNA(m[,c("AE","SE","MAE","MSE","R_complete","n")])&&all(m$R_complete==m$n)&&near(m$AE,m$MAE)&&near(m$SE,m$MSE)
    integrity<-rbind(integrity,data.frame(check="point_summary_reproduced",passed=ok))
  }
  must(identical(unname(hash_before),unname(tools::md5sum(source_paths))),"Source CSV/configuration changed during audit")
  integrity<-rbind(integrity,data.frame(check="source_hashes_unchanged",passed=TRUE))
  diagnostic_counts<-as.data.frame(table(case=datasets$case,issue_origin=datasets$issue_origin),stringsAsFactors=FALSE)
  method_diagnostics<-bind(lapply(groups(st,c("case","method")),function(ix){x<-st[ix,,drop=FALSE]
  data.frame(case=x$case[1],method=x$method[1],n_scheduled=nrow(x),fit_success=sum(x$fit_success%in%TRUE),fit_failed=sum(x$fit_success%in%FALSE),
             fit_unknown=sum(is.na(x$fit_success)),source_flag=sum(x$numerical_anomaly%in%TRUE),source_flag_unknown=sum(is.na(x$numerical_anomaly)),
             native_nonzero_datasets=sum(x$native_nonzero),native_nonzero_member_codes=sum(x$native_nonzero_count),native_diagnostic_incomplete=sum(x$native_diagnostic_incomplete))}))
  main<-contrasts[contrasts$subset=="all_available_pairs"&contrasts$scope=="pooled_H",,drop=FALSE]
  outputs<-list(integrity_checks=integrity,method_availability=availability,method_diagnostics=method_diagnostics,
                dataset_issue_index=datasets,dataset_issue_origin_counts=diagnostic_counts,checkpoint_audit=bind(checkpoints),
                bootstrap_parameter_diagnostics=bind(parameters),numerical_issues_unmodified=ni,point_losses_per_replica=pp,
                method_point_summary=point_summary,point_pairwise_main=main[main$metric%in%c("AE","SE"),,drop=FALSE],
                point_pairwise_specific_horizons=contrasts[contrasts$scope=="specific_horizon"&contrasts$subset=="all_available_pairs",,drop=FALSE],
                pairwise_descriptive_sensitivity=contrasts[contrasts$subset!="all_available_pairs",,drop=FALSE],
                paired_losses_and_influence=detail,actual_interval_budgets=budgets,interval_losses_per_replica=interval_pool,
                interval_method_summary=interval_summary,interval_IS_pairwise_main=main[main$metric=="IS",,drop=FALSE])
  for(n in names(outputs))write_table(outputs[[n]],paste0(n,".csv"))
  write_table(data.frame(file=basename(source_paths),md5=unname(hash_before)),"input_manifest.csv")
  notes<-c("AUDIT OF THE EXECUTED PROCEDURE; NOT AN OPTIMIZATION REPAIR OR A PUBLICATION CERTIFICATE.",
           "Primary results use all valid jointly available pairs. Failures never count as competitor losses.",
           "Source likelihood/flag diagnostics, native nonzero codes and missing diagnostic information are separate.",
           "A dataset-level signal may concern a different method. It is not an automatic exclusion criterion.",
           "Native codes: standard_code is one stored final native fit; bagged_codes are stored codes of ensemble members.",
           "The audit does not infer the meaning of a nonzero backend code or treat it as a harmless intermediate retry.",
           "No-recorded flags do not prove optimization adequacy. Sensitivity subsets are descriptive, not replacement analyses.",
           "Strict win rates include tolerance-defined ties in their denominator. Sign tests exclude such ties.",
           "Raw loss differences, not tolerance-rounded differences, are used for means, t intervals and Wilcoxon.",
           "Wilcoxon is an approximate signed-rank test, not a general test of equal means.",
           "Holm across DGP is exploratory sensitivity, separately per focal/reference/metric/horizon/level.",
           "Confidence intervals are pointwise Monte Carlo intervals; no simultaneous coverage is claimed.",
           "Coverage averages marginal events across horizons, not simultaneous path coverage.",
           "RMSE=sqrt(mean per-replica MSE); inference in pair tables concerns MAE or MSE, not mean replica RMSE.",
           "There is no claim of statistical equivalence from a nonsignificant result.",
           "Input S and original approval note are preserved exactly; a note cannot override the actual configuration.",
           "Mean/median coefficient diagnostics do not establish that median aggregation improves forecasting.",
           "The assistant did not execute this script in R; startup and integrity checks run in the user's R session.")
  writeLines(notes,file.path(destination,"READ_ME.txt"))
  meta<-list(audit_version=version,source_folder=folder,source_configuration=cfg,started=started,finished=Sys.time(),
             tie_abs=TIE_ABS,tie_rel=TIE_REL,check_abs=CHECK_ABS,check_rel=CHECK_REL,scan_checkpoints=SCAN_CHECKPOINTS,session=sessionInfo())
  saveRDS(meta,file.path(destination,"audit_configuration.rds"))
  con<-file(file.path(destination,"AUDIT_MESE_v7_REPORT.txt"),open="wt",encoding="UTF-8")
  tryCatch({writeLines(c(notes,"",capture.output(dput(meta)),""),con)
    for(n in c("integrity_checks","method_diagnostics","dataset_issue_origin_counts","method_point_summary","point_pairwise_main","actual_interval_budgets","interval_method_summary","interval_IS_pairwise_main")){
      p<-file.path(destination,paste0(n,".csv"));if(file.exists(p))writeLines(c(paste0("===== BEGIN ",n,".csv ====="),readLines(p,warn=FALSE),paste0("===== END ",n,".csv ====="),""),con)
    }
  },finally=close(con))
  cat("\n--- Integrity checks ---\n");print(integrity,row.names=FALSE)
  cat("\n--- Dataset issue origins: not a failure classification ---\n");print(with(datasets,table(case,issue_origin)))
  cat("\n--- Incomplete diagnostic information (independent flag) ---\n");print(with(datasets,table(case,audit_information_incomplete)))
  cat("\n--- Native diagnostics by method ---\n");print(method_diagnostics[method_diagnostics$method%in%c("ETS_standard","BaggedETS_original"),],row.names=FALSE)
  show<-main[main$metric=="AE"&main$reference%in%c("ETS_standard","ETS_controlled","Akaike_controlled","MEB_original","BaggedETS_original"),]
  cat("\n--- Primary paired MAE ---\n");print(show[,c("case","focal","reference","n_pairs","wins","ties","losses","mean_difference","MC_lower95","MC_upper95")],row.names=FALSE,digits=6,max=100000)
  cat("\nAudit folder:",destination,"\nShare AUDIT_MESE_v7_REPORT.txt. No original results were overwritten.\n")
  if(any(integrity$passed%in%FALSE))warning("Input integrity checks failed: do not use affected summaries for paper inference.")
})






local({
  folder <- "MESE_v7_validation"
  
  cfg <- readRDS(
    file.path(folder, "configuration.rds")
  )
  
  for (ca in cfg$cases) {
    checkpoint <- file.path(
      folder,
      sprintf("%s_%05d_fit.rds", ca, 1L)
    )
    
    z <- readRDS(checkpoint)
    
    cat(
      "\n====================================\n",
      "Scenario: ", ca, " — replica 1\n",
      sep = ""
    )
    
    cat("\nCodice ETS standard:\n")
    print(z$diagnostic$standard_code)
    
    cat("\nCodici dei membri Bagged ETS:\n")
    
    codes <- z$diagnostic$bagged_codes
    
    if (is.null(codes)) {
      cat("Codici non presenti nel checkpoint.\n")
    } else {
      print(table(codes, useNA = "ifany"))
    }
  }
})
