# MESE_EMPIRICAL_REVIEW_ENGINE.R -- functions only.
# Requires the user's v7 corrected core via ep_load_core(). No model runs on source().
ep_bind <- function(x) {
  x<-Filter(function(z)!is.null(z)&&is.data.frame(z)&&nrow(z)>0L,x)
  if(!length(x))return(data.frame());z<-do.call(rbind,x);rownames(z)<-NULL;z
}
ep_assert <- function(x,msg) if(!isTRUE(x))stop(msg,call.=FALSE)
ep_md5 <- function(x) {p<-tempfile();on.exit(unlink(p));saveRDS(x,p,version=3);unname(tools::md5sum(p))}
ep_save <- function(x,file) {
  dir.create(dirname(file),recursive=TRUE,showWarnings=FALSE)
  tmp<-tempfile(tmpdir=dirname(file));on.exit(unlink(tmp),add=TRUE);saveRDS(x,tmp,version=3)
  backup<-NULL
  if(file.exists(file)){backup<-tempfile(tmpdir=dirname(file));ep_assert(file.rename(file,backup),"Cannot back up checkpoint")}
  if(!file.rename(tmp,file)){
    if(!is.null(backup))file.rename(backup,file)
    stop("Cannot commit checkpoint; previous version restored where possible")
  }
  if(!is.null(backup))unlink(backup)
  invisible(file)
}
ep_load_core <- function(file) {
  ep_assert(file.exists(file),paste("Missing corrected v7 core:",file))
  parse(file=file,keep.source=FALSE)
  env<-new.env(parent=globalenv());sys.source(file,envir=env,keep.source=FALSE)
  required<-c("ms_config","ms_backend_check","ms_audit","ms_grid","ms_fit","ms_fit_grid","ms_scores","ms_pick","ms_common",
    "ms_method","ms_standard","ms_parameter","ms_id","ms_code","ms_intervals","ms_self_tests")
  ep_assert(all(vapply(required,function(n)exists(n,env,inherits=FALSE)&&is.function(get(n,env)),logical(1))),"Incompatible v7 core")
  assign("ep_core",env,envir=environment(ep_load_core));invisible(TRUE)
}
ep_software <- function(dataset) {
  pkgs<-c("forecast","meboot",if(dataset=="tourism_monthly")"Tcomp" else "Mcomp")
  for(p in pkgs)ep_assert(requireNamespace(p,quietly=TRUE),paste("Install explicitly before running:",p))
  c(R=as.character(getRversion()),setNames(vapply(pkgs,function(p)as.character(utils::packageVersion(p)),character(1)),pkgs))
}
ep_config <- function(dataset,stage,workers,offsets,engine_file,core_file) {
  ep_assert(dataset%in%c("tourism_monthly","m3_monthly"),"Unknown dataset")
  z<-ep_core$ms_config("precision",workers=as.integer(workers))
  z$engine<-"MESE_empirical_review_1";z$mode<-"empirical";z$stage<-stage;z$dataset<-dataset
  z$n<-NULL;z$R<-NULL;z$R_bag<-NULL;z$R_ablation<-NULL;z$cases<-NULL
  z$B<-if(stage=="pilot")10L else 100L;z$B_grid<-z$B
  z$S<-if(stage=="pilot")500L else 20000L
  z$S_grid<-if(stage=="pilot")500L else c(2000L,10000L,20000L)
  z$H<-12L;z$m<-12L;z$min_train<-48L;z$seed<-197654321L
  z$origin_offsets<-sort(unique(as.integer(offsets)),decreasing=TRUE)
  ep_assert(length(offsets)>0&&all(is.finite(offsets))&&all(offsets==as.integer(offsets))&&all(offsets>=0)&&0L%in%offsets&&length(z$origin_offsets)<=8L,"Invalid origin offsets")
  z$design<-if(identical(z$origin_offsets,0L))"official_split_first_12_holdout" else "preset_expanding_origins_including_official_split"
  z$methods<-c("ETS_standard","ETS_controlled","Akaike_controlled","MEB_original","MESE_original",
    "MEB_common","MESE_MEB_selection","MEB_per_replica","BaggedETS_original")
  z$focal_methods<-c("MESE_original","MESE_MEB_selection")
  z$keep_components<-TRUE;z$compute_intervals<-TRUE;z$keep_full_optimizer_log<-FALSE
  z$primary_metric<-"MASE";z$secondary_metrics<-c("MAE","RMSE","MAPE","sMAPE")
  z$tie_abs<-1e-10;z$tie_rel<-1e-8;z$bootstrap_CI_reps<-999L
  z$software<-ep_software(dataset)
  z$module_md5<-unname(tools::md5sum(engine_file));z$core_md5<-unname(tools::md5sum(core_file))
  z$protocol_note<-paste("Same v7 parameter/forecast definitions; both selectors reported.",
    "No manual shift; controlled lambda=NULL. Native benchmark defaults retained.",
    "No adaptive ETS/ARIMA selector. No claim that this design reproduces old manuscript tables.",
    "Holdout outcomes never choose candidates, coefficients, trimming, pilot IDs or hyperparameters.")
  ep_core$ms_backend_check(z);z
}
ep_data <- function(cfg) {
  pkg<-if(cfg$dataset=="tourism_monthly")"Tcomp" else "Mcomp"
  object<-if(pkg=="Tcomp")"tourism" else "M3"
  e<-new.env(parent=baseenv());utils::data(list=object,package=pkg,envir=e)
  ep_assert(exists(object,e,inherits=FALSE),paste("Cannot load",object));all<-get(object,e)
  ix<-which(vapply(all,function(s)is.ts(s$x)&&stats::frequency(s$x)==12,logical(1)))
  expected<-if(pkg=="Tcomp")366L else 1428L
  ep_assert(length(ix)==expected,"Unexpected monthly population; inspect dataset rather than silently subsetting")
  series<-vector("list",length(ix));manifest<-list()
  for(j in seq_along(ix)){
    s<-all[[ix[j]]];id<-if(length(s$st)==1L)as.character(s$st) else names(all)[ix[j]]
    if(is.na(id)||!nzchar(id))id<-paste0("series_",ix[j])
    ep_assert(is.ts(s$xx)&&frequency(s$xx)==12,"Test values must be monthly ts")
    ep_assert(all(is.finite(s$x))&&all(is.finite(s$xx)),paste("Nonfinite source data:",id))
    ep_assert(abs(tsp(s$xx)[1]-(tsp(s$x)[2]+1/12))<1e-7,paste("Noncontiguous official split:",id))
    series[[j]]<-list(id=id,index=j,source_index=ix[j],train=s$x,test=s$xx)
    manifest[[j]]<-data.frame(series_id=id,series_index=j,source_index=ix[j],n_train=length(s$x),n_test=length(s$xx),
      train_start=tsp(s$x)[1],train_end=tsp(s$x)[2],test_start=tsp(s$xx)[1],test_end=tsp(s$xx)[2],
      train_min=min(s$x),train_max=max(s$x),train_sd=sd(as.numeric(s$x)),train_nonpositive=sum(s$x<=0),
      dataset=pkg,description=if(length(s$description)==1L)as.character(s$description) else "")
  }
  manifest<-ep_bind(manifest);ep_assert(!anyDuplicated(manifest$series_id),"Duplicate series IDs")
  list(series=series,manifest=manifest,data_md5=ep_md5(series))
}
ep_tasks <- function(data,cfg) {
  ids<-seq_along(data$series)
  if(cfg$stage=="pilot"){
    ord<-order(data$manifest$n_train,data$manifest$series_id)
    ids<-unique(ord[round(seq(1,length(ord),length.out=6))])
    nonpositive<-which(data$manifest$train_nonpositive>0)
    if(length(nonpositive)&&!any(ids%in%nonpositive))ids[length(ids)]<-nonpositive[1L]
  }
  tasks<-list();rows<-list()
  for(j in ids)for(o in seq_along(cfg$origin_offsets)){
    s<-data$series[[j]];offset<-cfg$origin_offsets[o];n<-length(s$train)-offset
    allvalues<-c(as.numeric(s$train),as.numeric(s$test))
    eligible<-n>=cfg$min_train&&length(allvalues)>=n+cfg$H
    reason<-if(n<cfg$min_train)"training_below_prespecified_minimum" else if(length(allvalues)<n+cfg$H)"insufficient_future_observations" else ""
    task_id<-sprintf("%04d_o%02d",j,o)
    y<-if(n>0)ts(as.numeric(s$train)[seq_len(n)],start=start(s$train),frequency=12) else numeric()
    actual<-if(eligible)allvalues[n+seq_len(cfg$H)] else rep(NA_real_,cfg$H)
    task<-list(task_id=task_id,series_id=s$id,series_index=j,origin=o,offset=offset,train=y,actual=actual,
      eligible=eligible,reason=reason,origin_time=if(n>0)tsp(y)[2] else NA_real_,root_seed=cfg$seed+j*10000L+o*1000L)
    tasks[[length(tasks)+1L]]<-task
    rows[[length(rows)+1L]]<-data.frame(task_id=task_id,series_id=s$id,origin=o,offset=offset,n_train=max(0,n),
      origin_time=task$origin_time,eligible=eligible,eligibility_reason=reason,root_seed=task$root_seed)
  }
  list(tasks=tasks,manifest=ep_bind(rows))
}
ep_module <- function(stage,audit,fun) {
  audit$stage<-stage;t<-proc.time()[["elapsed"]];ws<-character()
  value<-tryCatch(withCallingHandlers(fun(),warning=function(w){ws<<-c(ws,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)e)
  audit$times[[length(audit$times)+1L]]<-data.frame(stage=stage,seconds=proc.time()[["elapsed"]]-t)
  if(inherits(value,"error"))list(ok=FALSE,error=conditionMessage(value),stage=stage,warnings=unique(ws)) else list(ok=TRUE,value=value,error="",stage="",warnings=unique(ws))
}
ep_build <- function(task,cfg,audit) {
  do.call(RNGkind,as.list(cfg$rng_kind));set.seed(task$root_seed)
  y<-task$train;cfg$training_scale<-sd(as.numeric(y));g<-ep_core$ms_grid(all(y>0))
  methods<-list();status<-list();diagnostic<-list();selection_members<-list();parameters<-list()
  add<-function(nm,q,selected=NA_character_,field=NULL,intervals=TRUE,native_code=NA_integer_){
    value<-if(q$ok){if(is.null(field))q$value else q$value[[field]]} else NULL
    status[[length(status)+1L]]<<-data.frame(method=nm,fit_success=q$ok,fit_error=q$error,failed_stage=q$stage,
      fit_warnings=paste(q$warnings,collapse=" | "),selected_model=selected,intervals_planned=cfg$compute_intervals&&intervals,
      native_code=native_code,numerical_anomaly=FALSE)
    if(q$ok)methods[[nm]]<<-value
  }
  bad<-function(q,label)list(ok=FALSE,error=paste(label,q$error),stage=q$stage,warnings=q$warnings)
  weight_models<-function(nm,models,weights){
    ids<-vapply(models,ep_core$ms_id,character(1));d<-aggregate(weights,list(model=ids),sum);names(d)[2]<-"weight";d$method<-nm;d
  }
  standard<-ep_module("ETS_standard",audit,function()ep_core$ms_standard(y,task$root_seed,cfg))
  add("ETS_standard",standard,if(standard$ok)standard$value$selected else NA_character_,"method",native_code=if(standard$ok)standard$value$code else NA_integer_)
  orig<-ep_module("controlled_original",audit,function(){
    ep_assert(is.finite(cfg$training_scale)&&cfg$training_scale>0,"Degenerate training scale for controlled fits")
    ep_core$ms_fit_grid(y,g,cfg,audit,"original")
  })
  fs<-if(orig$ok)orig$value else NULL;ib<-if(orig$ok)ep_core$ms_pick(fs,cfg) else NULL
  if(orig$ok){
    diagnostic$original_scores<-data.frame(model=g$id,IC=ep_core$ms_scores(fs,cfg))
    q<-ep_module("ETS_controlled_packing",audit,function()ep_core$ms_method(list(fs[[ib]]),1,cfg));add("ETS_controlled",q,g$id[ib])
    q<-ep_module("Akaike_packing",audit,function(){s<-ep_core$ms_scores(fs,cfg);w<-exp(-.5*(s-min(s)));ep_assert(all(is.finite(w))&&sum(w)>0,"Invalid IC weights");
      diagnostic$akaike_weights<<-data.frame(model=g$id,weight=w/sum(w));ep_core$ms_method(fs,w,cfg)})
    add("Akaike_controlled",q)
  }else{add("ETS_controlled",orig);add("Akaike_controlled",orig)}
  paths<-ep_module("MEB_generation",audit,function(){
    set.seed(task$root_seed+1L);trim<-if(all(y>0))list(trim=.1,xmin=0,xmax=NULL) else .1
    P<-as.matrix(meboot::meboot(y,reps=cfg$B,trim=trim,reachbnd=TRUE,expand.sd=FALSE,force.clt=FALSE,scl.adjustment=FALSE,sym=FALSE,elaps=FALSE)$ens)
    ep_assert(nrow(P)==length(y)&&ncol(P)==cfg$B&&all(is.finite(P))&&(!all(y>0)||all(P>0)),"Invalid MEB ensemble/domain")
    P
  })
  bank<-if(!orig$ok)bad(orig,"Original selection unavailable:") else if(!paths$ok)bad(paths,"MEB generation failed:") else
    ep_module("primary_MEB_fits",audit,function()lapply(seq_len(cfg$B),function(b)
      ep_core$ms_fit(ts(paths$value[,b],start=start(y),frequency=12),as.list(g[ib,]),cfg,audit,paste0("primary_MEB[",b,"]/",g$id[ib]))))
  if(bank$ok){
    q<-ep_module("MEB_original_packing",audit,function()ep_core$ms_method(bank$value,rep(1,cfg$B),cfg));add("MEB_original",q,g$id[ib])
    pm<-ep_module("MESE_original_refit",audit,function()ep_core$ms_parameter(y,bank$value,as.list(g[ib,]),fs[[ib]],cfg,audit,"MESE_original"))
    add("MESE_original",pm,g$id[ib],"method")
    if(pm$ok)parameters$MESE_original<-list(model=g$id[ib],mean=pm$value$mean,coefficients=pm$value$coefficients,gain=pm$value$gain)
  }else{add("MEB_original",bank);add("MESE_original",bank)}
  grids<-if(!paths$ok)bad(paths,"MEB generation failed:") else ep_module("MEB_full_grid",audit,function(){
    ep_assert(is.finite(cfg$training_scale)&&cfg$training_scale>0,"Degenerate training scale")
    lapply(seq_len(cfg$B),function(b)ep_core$ms_fit_grid(ts(paths$value[,b],start=start(y),frequency=12),g,cfg,audit,
      paste0("MEB_grid[",b,"]"),cache_index=if(bank$ok)ib else NULL,cache_fit=if(bank$ok)bank$value[[b]] else NULL))
  })
  if(grids$ok){
    gs<-grids$value;im<-ep_core$ms_common(gs,cfg);common<-lapply(gs,function(f)f[[im]])
    individual<-lapply(gs,function(f)f[[ep_core$ms_pick(f,cfg)]])
    diagnostic$bootstrap_IC<-do.call(rbind,lapply(gs,ep_core$ms_scores,cfg=cfg));colnames(diagnostic$bootstrap_IC)<-g$id
    if(!is.null(ib)&&im==ib&&!is.null(methods$MEB_original)){
      q<-list(ok=TRUE,value=methods$MEB_original,error="",stage="",warnings=character());q$value$alias_of<-"MEB_original"
    }else q<-ep_module("MEB_common_packing",audit,function()ep_core$ms_method(common,rep(1,cfg$B),cfg))
    add("MEB_common",q,g$id[im])
    if(!is.null(ib)&&im==ib&&!is.null(methods$MESE_original)){
      q<-list(ok=TRUE,value=methods$MESE_original,error="",stage="",warnings=character());q$value$alias_of<-"MESE_original"
      add("MESE_MEB_selection",q,g$id[im]);parameters$MESE_MEB_selection<-parameters$MESE_original
    }else{
      pm<-ep_module("MESE_common_refit",audit,function(){ref<-if(orig$ok)fs[[im]] else ep_core$ms_fit(y,as.list(g[im,]),cfg,audit,"common_reference")
        ep_core$ms_parameter(y,common,as.list(g[im,]),ref,cfg,audit,"MESE_MEB_selection")})
      add("MESE_MEB_selection",pm,g$id[im],"method")
      if(pm$ok)parameters$MESE_MEB_selection<-list(model=g$id[im],mean=pm$value$mean,coefficients=pm$value$coefficients,gain=pm$value$gain)
    }
    ids<-vapply(individual,ep_core$ms_id,character(1))
    if(!is.null(ib)&&all(ids==g$id[ib])&&!is.null(methods$MEB_original)){
      q<-list(ok=TRUE,value=methods$MEB_original,error="",stage="",warnings=character());q$value$alias_of<-"MEB_original"
    }else q<-ep_module("MEB_per_replica_packing",audit,function()ep_core$ms_method(individual,rep(1,cfg$B),cfg))
    add("MEB_per_replica",q);selection_members$MEB_per_replica<-weight_models("MEB_per_replica",individual,rep(1/cfg$B,cfg$B))
  }else for(nm in c("MEB_common","MESE_MEB_selection","MEB_per_replica"))add(nm,grids)
  bag<-ep_module("BaggedETS_original",audit,function(){
    set.seed(task$root_seed+2L);P<-forecast::bld.mbb.bootstrap(y,num=cfg$B)
    f<-forecast::baggedETS(y,bootstrapped_series=P)
    point<-as.numeric(forecast::forecast(f,h=cfg$H)$mean)
    list(method=ep_core$ms_method(f$models,rep(1,length(f$models)),cfg,override=point,intervals=FALSE),
      codes=vapply(f$models,ep_core$ms_code,integer(1)),model_weights=weight_models("BaggedETS_original",f$models,rep(1/length(f$models),length(f$models))))
  })
  add("BaggedETS_original",bag,field="method",intervals=FALSE)
  if(bag$ok){diagnostic$bagged_codes<-bag$value$codes;selection_members$BaggedETS_original<-bag$value$model_weights}
  st<-ep_bind(status);ep_assert(setequal(st$method,cfg$methods)&&!anyDuplicated(st$method),"Missing method status")
  issues<-ep_bind(audit$issues)
  if(nrow(issues)){
    oi<-any(grepl("^original/",issues$stage));pi<-any(grepl("^primary_MEB",issues$stage));gi<-any(grepl("^MEB_grid",issues$stage))
    st$numerical_anomaly<-(st$method%in%c("ETS_controlled","Akaike_controlled","MEB_original","MESE_original")&oi)|
      (st$method%in%c("MEB_original","MESE_original","MEB_common","MESE_MEB_selection","MEB_per_replica")&pi)|
      (st$method%in%c("MEB_common","MESE_MEB_selection","MEB_per_replica")&gi)
    for(nm in st$method)if(any(issues$stage==nm))st$numerical_anomaly[st$method==nm]<-TRUE
    for(nm in names(methods))if(!is.null(methods[[nm]]$alias_of))st$numerical_anomaly[st$method==nm]<-
      any(st$numerical_anomaly[st$method%in%c(nm,methods[[nm]]$alias_of)])
  }
  scale<-if(length(y)>12)mean(abs(as.numeric(y)[13:length(y)]-as.numeric(y)[seq_len(length(y)-12)])) else NA_real_
  list(methods=methods,status=st,training_scale=cfg$training_scale,mase_scale=scale,train=as.numeric(y),actual=task$actual,
    selection_members=selection_members,parameters=parameters,diagnostic=diagnostic)
}
ep_worker <- function(task,cfg,folder,retry_fits=FALSE,retry_intervals=FALSE) {
  ep_assert(identical(ep_software(cfg$dataset),cfg$software),"Worker software mismatch")
  file<-file.path(folder,paste0(task$task_id,"_fit.rds"))
  z<-if(file.exists(file))readRDS(file) else NULL
  if(!is.null(z))ep_assert(identical(z$protocol_hash,cfg$protocol_hash),"Checkpoint protocol mismatch")
  rerun<-is.null(z)||(retry_fits&&(isTRUE(z$fatal)||any(!z$status$fit_success)))
  if(rerun){
    if(!is.null(z)){
      for(nm in cfg$methods){ip<-file.path(folder,paste0(task$task_id,"__",nm,"_interval.rds"));if(file.exists(ip))unlink(ip)}
    }
    audit<-ep_core$ms_audit();t<-proc.time()[["elapsed"]]
    if(!task$eligible){z<-list(fatal=TRUE,error=task$reason)}else
      z<-tryCatch(ep_build(task,cfg,audit),error=function(e)list(fatal=TRUE,error=conditionMessage(e)))
    z$task<-task[c("task_id","series_id","series_index","origin","offset","origin_time","root_seed","eligible","reason")]
    z$protocol_hash<-cfg$protocol_hash;z$fit_wall_seconds<-proc.time()[["elapsed"]]-t
    z$timing<-ep_bind(audit$times);z$numerical_issues<-ep_bind(audit$issues)
    attempts<-ep_bind(audit$rows)
    z$attempt_counts<-if(nrow(attempts))data.frame(calls=sum(attempts$attempt==1),retries=sum(attempts$attempt>1),nonzero_or_unknown=sum(is.na(attempts$code)|attempts$code!=0)) else data.frame()
    z$optimizer_attempts<-if(nrow(attempts))attempts[attempts$attempt>1|is.na(attempts$code)|attempts$code!=0|nzchar(attempts$warnings)|nzchar(attempts$error),,drop=FALSE] else attempts
    z$unresolved<-audit$unresolved;ep_save(z,file)
  }
  if(isTRUE(z$fatal))return(data.frame(task_id=task$task_id,state="fatal",seconds=z$fit_wall_seconds))
  for(nm in names(z$methods)){
    method<-z$methods[[nm]];if(!method$intervals_planned)next
    ip<-file.path(folder,paste0(task$task_id,"__",nm,"_interval.rds"));q<-if(file.exists(ip))readRDS(ip) else NULL
    if(!is.null(q))ep_assert(identical(q$protocol_hash,cfg$protocol_hash),"Interval protocol mismatch")
    if(is.null(q)||(retry_intervals&&!isTRUE(q$success))){
      q<-NULL;t<-proc.time()[["elapsed"]];ws<-character()
      if(!is.null(method$alias_of)){
        source<-file.path(folder,paste0(task$task_id,"__",method$alias_of,"_interval.rds"))
        if(file.exists(source)){sq<-readRDS(source);if(isTRUE(sq$success)&&identical(sq$protocol_hash,cfg$protocol_hash)){
          q<-sq;q$events$method<-nm;if(nrow(q$S_sensitivity))q$S_sensitivity$method<-nm;q$reused_identical_method<-method$alias_of}}
      }
      if(is.null(q))q<-tryCatch(withCallingHandlers(ep_core$ms_intervals(method,nm,z$actual,task$series_id,task$origin,
        task$root_seed,z$training_scale,cfg),warning=function(w){ws<<-c(ws,conditionMessage(w));invokeRestart("muffleWarning")}),error=function(e)list(error=conditionMessage(e)))
      q$success<-is.null(q$error);q$warnings<-unique(ws);q$seconds<-proc.time()[["elapsed"]]-t;q$protocol_hash<-cfg$protocol_hash
      ep_save(q,ip)
    }
  }
  data.frame(task_id=task$task_id,state=if(all(z$status$fit_success))"fits_completed" else "partial",seconds=z$fit_wall_seconds)
}
ep_run <- function(cfg,data,tasks,folder,retry_fits=FALSE,retry_intervals=FALSE) {
  dir.create(folder,recursive=TRUE,showWarnings=FALSE);folder<-normalizePath(folder,winslash="/",mustWork=TRUE)
  identity<-cfg;identity$workers<-NULL
  cp<-file.path(folder,"configuration.rds")
  if(file.exists(cp))ep_assert(identical(readRDS(cp),identity),"Source/data/configuration mismatch. Use a new OUTPUT_ROOT or restore the exact protocol.") else ep_save(identity,cp)
  ep_save(data,file.path(folder,"data_snapshot.rds"));ep_save(tasks$manifest,file.path(folder,"task_manifest.rds"))
  write.csv(data$manifest,file.path(folder,"data_manifest.csv"),row.names=FALSE)
  write.csv(tasks$manifest,file.path(folder,"task_manifest.csv"),row.names=FALSE)
  ep_save(sessionInfo(),file.path(folder,"session_info.rds"))
  t<-proc.time()[["elapsed"]]
  if(cfg$workers==1L)report<-lapply(tasks$tasks,ep_worker,cfg=cfg,folder=folder,retry_fits=retry_fits,retry_intervals=retry_intervals) else {
    cl<-parallel::makePSOCKcluster(cfg$workers);on.exit(parallel::stopCluster(cl),add=TRUE)
    env<-environment(ep_worker);nm<-ls(env,pattern="^ep_");parallel::clusterExport(cl,nm,envir=env)
    parallel::clusterCall(cl,function(config){Sys.setenv(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1");
      if(!identical(ep_software(config$dataset),config$software))stop("Worker software mismatch");ep_core$ms_backend_check(config);NULL},cfg)
    report<-parallel::parLapplyLB(cl,tasks$tasks,ep_worker,cfg=cfg,folder=folder,retry_fits=retry_fits,retry_intervals=retry_intervals)
  }
  report<-ep_bind(report);elapsed<-proc.time()[["elapsed"]]-t
  write.csv(report,file.path(folder,"task_report.csv"),row.names=FALSE)
  ep_save(list(wall_seconds=elapsed,workers=cfg$workers,timestamp=Sys.time(),resumed_checkpoints_possible=TRUE),file.path(folder,"execution_latest.rds"))
  cat("Wall seconds (this invocation, possibly resumed):",elapsed,"\n");invisible(report)
}
ep_metrics <- function(actual,pred,scale) {
  err<-actual-pred;den<-abs(actual)+abs(pred)
  data.frame(AE=abs(err),SE=err^2,APE=ifelse(actual!=0,100*abs(err)/abs(actual),NA_real_),
    sAPE=ifelse(den==0,0,200*abs(err)/den),scaled_AE=if(is.finite(scale)&&scale>0)abs(err)/scale else NA_real_)
}
ep_series_metrics <- function(d,H) {
  if(!nrow(d))return(data.frame())
  a<-d$actual;p<-d$forecast;scale<-d$mase_scale;m<-ep_metrics(a,p,scale[1])
  # For rolling origins, the MASE denominator is origin-specific.
  m$scaled_AE<-ifelse(is.finite(scale)&scale>0,m$AE/scale,NA_real_)
  val<-function(x)if(any(is.finite(x)))mean(x[is.finite(x)]) else NA_real_
  data.frame(MAE=mean(m$AE),RMSE=sqrt(mean(m$SE)),MAPE=val(m$APE),sMAPE=mean(m$sAPE),MASE=val(m$scaled_AE),
    n_events=nrow(d),n_origins=length(unique(d$origin)),n_MAPE_events=sum(is.finite(m$APE)),n_MASE_events=sum(is.finite(m$scaled_AE)))
}
ep_pair_stats <- function(a,b,cfg) {
  n<-length(a);ep_assert(n==length(b)&&all(is.finite(a))&&all(is.finite(b)),"Invalid paired values")
  d<-a-b;tol<-cfg$tie_abs+cfg$tie_rel*pmax(abs(a),abs(b));w<-sum(d< -tol);l<-sum(d>tol);tt<-n-w-l
  mu<-if(n)mean(d) else NA_real_;se<-if(n>1)sd(d)/sqrt(n) else NA_real_;lo<-hi<-pv<-pw<-NA_real_
  if(n>1&&is.finite(se)&&se>0){lo<-mu-qt(.975,n-1)*se;hi<-mu+qt(.975,n-1)*se;pv<-2*pt(-abs(mu/se),n-1)}
  if(n>1&&any(d!=0))pw<-suppressWarnings(tryCatch(wilcox.test(d,mu=0,exact=FALSE,correct=TRUE)$p.value,error=function(e)NA_real_))
  data.frame(n_series=n,mean_focal=if(n)mean(a) else NA_real_,mean_reference=if(n)mean(b) else NA_real_,
    difference=mu,SE_series=se,lower95=lo,upper95=hi,p_mean=pv,p_Wilcoxon_approx=pw,
    wins=w,ties=tt,losses=l,win_pct=if(n)100*w/n else NA_real_,
    median_difference=if(n)median(d) else NA_real_,relative_change_pct=if(n&&mean(b)>0)100*(mean(a)/mean(b)-1) else NA_real_)
}
ep_common_origin_support <- function(ev,methods,H) {
  ep_assert(all(c("series_id","origin","method","horizon")%in%names(ev)),"Missing common-support identifiers")
  ep_assert(length(methods)>0L&&!anyDuplicated(methods),"Invalid method list")
  ep_assert(length(H)==1L&&is.finite(H)&&H>=1L&&H==as.integer(H),"Invalid horizon count")
  if(!nrow(ev))return(ev[FALSE,c("series_id","origin"),drop=FALSE])
  ep_assert(!anyNA(ev[,c("series_id","origin","method","horizon")]),"Missing support identifiers")
  ep_assert(!anyDuplicated(paste(ev$series_id,ev$origin,ev$method,ev$horizon,sep="\035")),"Duplicate support events")
  blocks<-split(seq_len(nrow(ev)),paste(ev$series_id,ev$origin,sep="\035"))
  ans<-ep_bind(lapply(blocks,function(ix){
    x<-ev[ix,,drop=FALSE]
    valid<-nrow(x)==length(methods)*H&&setequal(x$method,methods)&&
      all(vapply(methods,function(m)setequal(x$horizon[x$method==m],seq_len(H)),logical(1)))
    if(valid)x[1L,c("series_id","origin"),drop=FALSE] else NULL
  }))
  if(!nrow(ans))return(ev[FALSE,c("series_id","origin"),drop=FALSE])
  ans
}

ep_collect <- function(folder) {
  cfg<-readRDS(file.path(folder,"configuration.rds"));tasks<-readRDS(file.path(folder,"task_manifest.rds"))
  status<-points<-intervals<-times<-future<-issues<-scores<-members<-params<-sens<-codes<-akaike<-domain<-list()
  for(k in seq_len(nrow(tasks))){
    task<-tasks[k,];pf<-file.path(folder,paste0(task$task_id,"_fit.rds"));z<-if(file.exists(pf))readRDS(pf) else NULL
    if(!is.null(z))ep_assert(identical(z$protocol_hash,cfg$protocol_hash),"Foreign fit checkpoint")
    if(is.null(z)||isTRUE(z$fatal)){
      st<-data.frame(method=cfg$methods,fit_success=FALSE,fit_error=if(is.null(z))"not_run" else z$error,failed_stage="task",
        fit_warnings="",selected_model=NA_character_,intervals_planned=cfg$methods!="BaggedETS_original",native_code=NA_integer_,numerical_anomaly=NA)
    }else{
      st<-z$status
      ep_assert(setequal(st$method,cfg$methods)&&!anyDuplicated(st$method),"Incomplete status schema")
      for(nm in names(z$methods)){
        method<-z$methods[[nm]]
        p<-data.frame(series_id=task$series_id,origin=task$origin,origin_time=task$origin_time,method=nm,horizon=seq_len(cfg$H),
          actual=z$actual,forecast=method$point,mase_scale=z$mase_scale)
        points[[length(points)+1L]]<-cbind(p,ep_metrics(z$actual,method$point,z$mase_scale))
      }
      if(!is.null(z$diagnostic$original_scores)){q<-z$diagnostic$original_scores;q$series_id<-task$series_id;q$origin<-task$origin;scores[[length(scores)+1L]]<-q}
      if(!is.null(z$diagnostic$akaike_weights)){q<-z$diagnostic$akaike_weights;q$series_id<-task$series_id;q$origin<-task$origin;akaike[[length(akaike)+1L]]<-q}
      for(nm in names(z$selection_members)){q<-z$selection_members[[nm]];q$series_id<-task$series_id;q$origin<-task$origin;members[[length(members)+1L]]<-q}
      for(nm in names(z$parameters)){
        a<-z$parameters[[nm]];C<-a$coefficients
        for(j in seq_len(nrow(C)))params[[length(params)+1L]]<-data.frame(series_id=task$series_id,origin=task$origin,method=nm,model=a$model,
          parameter=rownames(C)[j],mean=a$mean[j],median=median(C[j,]),sd=if(ncol(C)>1)sd(C[j,]) else NA_real_,
          min=min(C[j,]),max=max(C[j,]),parameter_minus_free_loglik=a$gain)
      }
      bc<-z$diagnostic$bagged_codes
      if(!is.null(bc))codes[[length(codes)+1L]]<-data.frame(series_id=task$series_id,origin=task$origin,method="BaggedETS_original",member=seq_along(bc),code=bc)
    }
    st$interval_success<-NA;st$interval_error<-"";st$interval_seconds<-NA_real_
    for(j in seq_len(nrow(st)))if(st$fit_success[j]&&st$intervals_planned[j]){
      nm<-st$method[j];ip<-file.path(folder,paste0(task$task_id,"__",nm,"_interval.rds"));q<-if(file.exists(ip))readRDS(ip) else NULL
      if(!is.null(q))ep_assert(identical(q$protocol_hash,cfg$protocol_hash),"Foreign interval checkpoint")
      st$interval_success[j]<-!is.null(q)&&isTRUE(q$success);st$interval_error[j]<-if(is.null(q))"not_run" else if(!isTRUE(q$success))q$error else ""
      if(!is.null(q)){
        st$interval_seconds[j]<-q$seconds;future[[length(future)+1L]]<-data.frame(series_id=task$series_id,origin=task$origin,method=nm,seconds=q$seconds,reused=!is.null(q$reused_identical_method))
        if(isTRUE(q$success)){
          a<-q$events;a$series_id<-task$series_id;a$origin<-task$origin;a$case<-NULL;a$replication<-NULL
          a$mase_scale<-z$mase_scale;a$scaled_IS<-if(is.finite(z$mase_scale)&&z$mase_scale>0)a$IS/z$mase_scale else NA_real_
          intervals[[length(intervals)+1L]]<-a
          domain[[length(domain)+1L]]<-data.frame(series_id=task$series_id,origin=task$origin,method=nm,
            innovation_below_minus_one_count=if(length(q$clipped)==1L)q$clipped else NA_integer_,
            simulated_paths=if(nm=="ETS_standard")NA_integer_ else cfg$S,
            reused_identical_method=!is.null(q$reused_identical_method))
          if(nrow(q$S_sensitivity)){a<-q$S_sensitivity;a$series_id<-task$series_id;a$origin<-task$origin;a$case<-NULL;a$replication<-NULL;sens[[length(sens)+1L]]<-a}
        }
      }
    }
    st$series_id<-task$series_id;st$origin<-task$origin;st$task_id<-task$task_id
    st$eligible<-task$eligible;st$offset<-task$offset;st$origin_time<-task$origin_time
    st$eligibility_reason<-task$eligibility_reason;status[[length(status)+1L]]<-st
    if(!is.null(z)){
      if(nrow(z$timing)){q<-z$timing;q$series_id<-task$series_id;q$origin<-task$origin;times[[length(times)+1L]]<-q}
      if(nrow(z$numerical_issues)){q<-z$numerical_issues;q$series_id<-task$series_id;q$origin<-task$origin;issues[[length(issues)+1L]]<-q}
    }
  }
  st<-ep_bind(status);ev<-ep_bind(points);ie<-ep_bind(intervals)
  write<-function(x,n)if(is.data.frame(x)&&ncol(x))write.csv(x,file.path(folder,paste0(n,".csv")),row.names=FALSE)
  tables<-list(status=st,point_events=ev,interval_events=ie,fit_timing=ep_bind(times),future_timing=ep_bind(future),numerical_issues=ep_bind(issues),
    original_model_scores=ep_bind(scores),ensemble_model_weights=ep_bind(members),bootstrap_parameter_diagnostics=ep_bind(params),
    interval_S_sensitivity=ep_bind(sens),bagged_native_codes=ep_bind(codes),
    akaike_model_weights=ep_bind(akaike),interval_innovation_domain_counter=ep_bind(domain))
  tables$method_availability<-ep_bind(lapply(cfg$methods,function(nm){
    x<-st[st$method==nm,,drop=FALSE]
    data.frame(method=nm,n_series_planned=length(unique(tasks$series_id)),n_origins_planned=nrow(x),
      n_eligible=sum(x$eligible%in%TRUE),n_ineligible=sum(!(x$eligible%in%TRUE)),
      n_fit_success=sum(x$fit_success%in%TRUE),n_not_run=sum(x$fit_error%in%"not_run"),
      n_fit_failed=sum(x$fit_success%in%FALSE&!(x$fit_error%in%"not_run")&x$eligible%in%TRUE),
      n_interval_planned=sum(x$intervals_planned%in%TRUE),n_interval_success=sum(x$interval_success%in%TRUE),
      n_interval_not_successful_after_fit=sum(x$fit_success%in%TRUE&x$intervals_planned%in%TRUE&!(x$interval_success%in%TRUE)),
      n_numerical_flags=sum(x$numerical_anomaly%in%TRUE),n_native_nonzero=sum(is.finite(x$native_code)&x$native_code!=0))
  }))
  for(n in c("data_manifest","task_manifest")){
    p<-file.path(folder,paste0(n,".csv"));if(file.exists(p))tables[[n]]<-read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)
  }
  ep_assert(!anyDuplicated(paste(st$series_id,st$origin,st$method)),"Duplicate status keys")
  if(nrow(ev)){
    ep_assert(!anyDuplicated(paste(ev$series_id,ev$origin,ev$method,ev$horizon)),"Duplicate point events")
    n<-aggregate(horizon~series_id+origin+method,ev,length);ep_assert(all(n$horizon==cfg$H),"Incomplete point horizons")
    ep_assert(!anyNA(ev[,c("series_id","origin","method","horizon")])&&all(ev$horizon%in%seq_len(cfg$H)),"Invalid point identifiers or horizons")
    ep_assert(all(is.finite(ev$actual))&&all(is.finite(ev$forecast)),"Nonfinite point outcomes or forecasts")
    hh<-split(ev$horizon,paste(ev$series_id,ev$origin,ev$method,sep="\035"))
    ep_assert(all(vapply(hh,function(h)setequal(h,seq_len(cfg$H)),logical(1))),"Incorrect point horizon set")
    ss<-lapply(split(ev,paste(ev$series_id,ev$method,sep="\035")),function(x)cbind(data.frame(series_id=x$series_id[1],method=x$method[1]),ep_series_metrics(x,cfg$H)))
    series_metrics<-ep_bind(ss);tables$series_metrics_available<-series_metrics
    metrics<-c("MASE","MAE","RMSE","MAPE","sMAPE")
    summaries<-list()
    for(nm in cfg$methods)for(v in metrics){q<-series_metrics[series_metrics$method==nm,v];q<-q[is.finite(q)]
      summaries[[length(summaries)+1L]]<-data.frame(method=nm,metric=v,n_series=length(q),n_series_planned=length(unique(tasks$series_id)),
        mean=if(length(q))mean(q) else NA_real_,SE_series=if(length(q)>1)sd(q)/sqrt(length(q)) else NA_real_,median=if(length(q))median(q) else NA_real_)}
    tables$method_summary_available<-ep_bind(summaries)
    pairs<-details<-list()
    for(focal in cfg$focal_methods)for(ref in setdiff(cfg$methods,focal)){
      a<-ev[ev$method==focal,];b<-ev[ev$method==ref,]
      d<-merge(a,b,by=c("series_id","origin","horizon"),suffixes=c("_focal","_reference"))
      if(nrow(d))ep_assert(all(d$actual_focal==d$actual_reference)&&all(d$mase_scale_focal==d$mase_scale_reference|is.na(d$mase_scale_focal)&is.na(d$mase_scale_reference)),"Paired targets/scales differ")
      byseries<-lapply(split(d,d$series_id),function(x){
        aa<-data.frame(origin=x$origin,actual=x$actual_focal,forecast=x$forecast_focal,mase_scale=x$mase_scale_focal)
        bb<-data.frame(origin=x$origin,actual=x$actual_reference,forecast=x$forecast_reference,mase_scale=x$mase_scale_reference)
        ma<-ep_series_metrics(aa,cfg$H);mb<-ep_series_metrics(bb,cfg$H)
        data.frame(series_id=x$series_id[1],metric=metrics,focal_loss=as.numeric(unlist(ma[1,metrics],use.names=FALSE)),reference_loss=as.numeric(unlist(mb[1,metrics],use.names=FALSE)),n_joint_origins=length(unique(x$origin)))
      })
      d0<-ep_bind(byseries)
      for(v in metrics){x<-if(nrow(d0))d0[d0$metric==v&is.finite(d0$focal_loss)&is.finite(d0$reference_loss),] else data.frame()
        q<-ep_pair_stats(if(nrow(x))x$focal_loss else numeric(),if(nrow(x))x$reference_loss else numeric(),cfg)
        pairs[[length(pairs)+1L]]<-cbind(data.frame(focal=focal,reference=ref,metric=v,n_series_planned=length(unique(tasks$series_id))),q)
        if(nrow(x)){x$focal<-focal;x$reference<-ref;x$difference<-x$focal_loss-x$reference_loss;details[[length(details)+1L]]<-x}
      }
    }
    pairs<-ep_bind(pairs)
    pairs$p_Holm_within_focal_metric<-NA_real_
    for(f in cfg$focal_methods)for(v in metrics){ix<-which(pairs$focal==f&pairs$metric==v);ok<-ix[is.finite(pairs$p_mean[ix])];if(length(ok))pairs$p_Holm_within_focal_metric[ok]<-p.adjust(pairs$p_mean[ok],"holm",n=length(ix))}
    tables$paired_series_point<-pairs;tables$paired_series_point_details<-ep_bind(details)
    # All-method summaries use exactly the same origin support within each series.
    count<-ep_common_origin_support(ev,cfg$methods,cfg$H)
    d<-merge(ev,count,by=c("series_id","origin"))
    if(nrow(d)){
      cm<-ep_bind(lapply(split(d,paste(d$series_id,d$method,sep="\035")),function(x)cbind(data.frame(series_id=x$series_id[1],method=x$method[1]),ep_series_metrics(x,cfg$H))))
      rows<-list()
      for(v in metrics){ids<-unique(cm$series_id);ids<-ids[vapply(ids,function(id){q<-cm[cm$series_id==id,];nrow(q)==length(cfg$methods)&&all(is.finite(q[[v]]))},logical(1))]
        for(nm in cfg$methods){q<-cm[cm$method==nm&cm$series_id%in%ids,v];rows[[length(rows)+1L]]<-data.frame(method=nm,metric=v,n_common_series=length(q),n_planned_series=length(unique(tasks$series_id)),mean=if(length(q))mean(q) else NA_real_)}
      }
      tables$method_summary_common_support<-ep_bind(rows)
    }
  }
  if(nrow(ie)){
    ep_assert(!anyDuplicated(paste(ie$series_id,ie$origin,ie$method,ie$level,ie$horizon)),"Duplicate intervals")
    n<-aggregate(horizon~series_id+origin+method+level,ie,length);ep_assert(all(n$horizon==cfg$H),"Incomplete interval horizons")
    vals<-c("covered","width","IS","scaled_IS")
    rows<-lapply(split(ie,paste(ie$series_id,ie$method,ie$level,sep="\035")),function(x){
      d<-data.frame(series_id=x$series_id[1],method=x$method[1],level=x$level[1],n_events=nrow(x),n_origins=length(unique(x$origin)))
      for(v in vals)d[[v]]<-if(any(is.finite(x[[v]])))mean(x[[v]][is.finite(x[[v]])]) else NA_real_;d})
    si<-ep_bind(rows);tables$series_interval_metrics<-si
    rows<-list();set.seed(cfg$seed+900L)
    for(nm in cfg$methods)for(l in cfg$levels){x<-si[si$method==nm&si$level==l,];if(!nrow(x))next
      rates<-x$covered;boot<-replicate(cfg$bootstrap_CI_reps,mean(sample(rates,length(rates),replace=TRUE)))
      ci<-quantile(boot,c(.025,.975),names=FALSE,type=8)
      d<-data.frame(method=nm,level=l,n_series=nrow(x),coverage_equal_series=mean(rates),coverage_gap_pp=100*(mean(rates)-l),
        series_bootstrap_lower95=ci[1],series_bootstrap_upper95=ci[2],coverage_SE_series=if(nrow(x)>1)sd(rates)/sqrt(nrow(x)) else NA_real_)
      for(v in c("width","IS","scaled_IS")){q<-x[[v]];q<-q[is.finite(q)];d[[v]]<-if(length(q))mean(q) else NA_real_;d[[paste0("n_",v)]]<-length(q)}
      rows[[length(rows)+1L]]<-d
    }
    tables$interval_summary_equal_series<-ep_bind(rows)
    # IS contrasts use common interval events BEFORE series aggregation.
    pairs<-list()
    for(focal in cfg$focal_methods)for(ref in setdiff(cfg$methods,c(focal,"BaggedETS_original")))for(l in cfg$levels){
      a<-ie[ie$method==focal&ie$level==l,];b<-ie[ie$method==ref&ie$level==l,]
      d<-merge(a,b,by=c("series_id","origin","horizon"),suffixes=c("_focal","_reference"))
      for(v in c("IS","scaled_IS")){
        rr<-lapply(split(d,d$series_id),function(x){aa<-x[[paste0(v,"_focal")]];bb<-x[[paste0(v,"_reference")]];ok<-is.finite(aa)&is.finite(bb)
          if(!any(ok))return(NULL);data.frame(a=mean(aa[ok]),b=mean(bb[ok]))})
        z<-ep_bind(rr);q<-ep_pair_stats(if(nrow(z))z$a else numeric(),if(nrow(z))z$b else numeric(),cfg)
        pairs[[length(pairs)+1L]]<-cbind(data.frame(focal=focal,reference=ref,level=l,metric=v),q)
      }
    }
    pairs<-ep_bind(pairs);pairs$p_Holm_within_focal_level_metric<-NA_real_
    for(f in cfg$focal_methods)for(l in cfg$levels)for(v in c("IS","scaled_IS")){
      ix<-which(pairs$focal==f&pairs$level==l&pairs$metric==v);ok<-ix[is.finite(pairs$p_mean[ix])]
      if(length(ok))pairs$p_Holm_within_focal_level_metric[ok]<-p.adjust(pairs$p_mean[ok],"holm",n=length(ix))}
    tables$paired_series_interval_scores<-pairs
    tables$coverage_by_horizon<-aggregate(covered~method+level+horizon,ie,mean)
    hh<-split(ie$horizon,paste(ie$series_id,ie$origin,ie$method,ie$level,sep="\035"))
    ep_assert(all(vapply(hh,function(h)setequal(h,seq_len(cfg$H)),logical(1))),"Incorrect interval horizon set")
    budgets<-unique(ie[,c("method","interval_construction","S"),drop=FALSE])
    budgets$matches_config<-ifelse(budgets$method=="ETS_standard",is.na(budgets$S),!is.na(budgets$S)&budgets$S==cfg$S)
    ep_assert(all(budgets$matches_config),"Stored interval budget disagrees with protocol")
    tables$actual_interval_budgets<-budgets
  }
  selections<-st[st$fit_success&!is.na(st$selected_model),c("series_id","origin","method","selected_model")]
  tables$selection_by_origin<-selections
  if(nrow(selections)){q<-aggregate(rep(1,nrow(selections)),selections[,c("method","selected_model")],sum);names(q)[3]<-"origins";tables$selection_frequencies<-q}
  agreements<-list()
  for(target in c("MEB_common","MESE_MEB_selection","MEB_per_replica","BaggedETS_original"))for(ref in c("ETS_standard","ETS_controlled")){
    a<-selections[selections$method==target,c("series_id","origin","selected_model"),drop=FALSE]
    b<-selections[selections$method==ref,c("series_id","origin","selected_model"),drop=FALSE]
    d<-merge(a,b,by=c("series_id","origin"),suffixes=c("_target","_reference"))
    if(nrow(d))agreements[[length(agreements)+1L]]<-data.frame(target=target,reference=ref,n_joint_origins=nrow(d),
      agreement_exact=mean(d$selected_model_target==d$selected_model_reference),
      agreement_ETS_family=mean(gsub("d","",d$selected_model_target,fixed=TRUE)==gsub("d","",d$selected_model_reference,fixed=TRUE)))
  }
  tables$selection_agreement<-ep_bind(agreements)
  # Per-replica and Bagged ensembles do not have a single selected structure;
  # their distributions are reported in ensemble_model_weights instead.
  for(n in names(tables))write(tables[[n]],n)
  notes<-c("EMPIRICAL REVIEW STUDY -- read before using tables",
    paste("Dataset:",cfg$dataset,"Design:",cfg$design),
    "tourism_monthly is the 366-series Tourism Forecasting Competition, NOT the M3 competition.",
    "m3_monthly is the genuine 1428-series M3 monthly population. Do not interchange labels or results.",
    "Default offsets=0 gives one official training/test split and first 12 test values, NOT rolling-origin.",
    "Multiple preset offsets give expanding training prefixes; all preprocessing and model fitting are repeated on each prefix.",
    "There is no adaptive ETS/ARIMA selector and no MEB-ARIMA in this study.",
    "No manual shift is used. Candidate positivity is determined only from the training prefix. Controlled lambda=NULL.",
    "Native ETS and Bagged ETS defaults are retained; native codes are diagnostics, not automatic failures or exclusions.",
    "BLD transforms belong to the native Bagged ETS method, not a common shift of every method.",
    "The controlled candidate grid is the restricted v7 grid; native automatic ETS searches its own default class.",
    "Parameter aggregation and original-series state refitting are jointly assessed; no effect of averaging alone is identified.",
    "MESE_original and MESE_MEB_selection use different selectors and are not interchangeable.",
    "A complete controlled grid is required; candidate failures are reported, not silently removed.",
    "Numerical anomaly flags are conservative dependency diagnostics; native stopping codes are kept separately.",
    "No true model or true coefficients are known here; selection frequencies do not measure recovery, bias or estimator efficiency.",
    "Available-support method means are descriptive. Use common-support summaries or paired contrasts for rankings.",
    "Paired comparisons match origins/horizons before averaging within each series. The inferential unit is series, not forecast event.",
    "Mean RMSE is the mean of per-series root mean squared errors; not root of a globally pooled MSE.",
    "MASE uses the original training prefix's seasonal naive denominator (lag 12). It is undefined when the denominator is zero.",
    "MAPE excludes exactly zero actuals, with event counts reported. sMAPE is zero when actual and prediction are both zero.",
    "No missing/failed competitor automatically becomes a MESE win. Ties use the saved tolerances.",
    "Series t intervals and tests assume independent series. Cross-series dependence may remain; these are not Diebold-Mariano tests.",
    "Bootstrap coverage intervals resample whole series, are bounded, and do not establish calibration from non-rejection.",
    "Coverage is marginal across forecast horizons, not simultaneous trajectory coverage.",
    "Native Bagged min-max bands are not evaluated as nominal prediction intervals.",
    "Parameter plug-in and forecast-mixture residual intervals are reported separately. Residual resampling does not model GARCH volatility.",
    "Holm is applied within the stated focal/metric families; p-values for other metrics remain secondary/exploratory.",
    "Optimizer convergence and source likelihood challenges are not repaired by this collector.",
    "Selection agreement is descriptive on jointly available single-structure fits; ensemble structure distributions are separate.",
    "Innovation-domain counters record source flags below -1, not a verified count of backend clipping operations.",
    "Collector v2 fixes counting unique methods rather than horizon-event rows when constructing all-method common support.",
    "This module was statically reviewed, not executed in R by the assistant; local unit tests run before fitting.",
    "Keep both favorable and unfavorable results; do not reuse old manuscript tables without verifying this new protocol.")
  writeLines(notes,file.path(folder,"READ_ME_RESULTS.txt"))
  report<-file(file.path(folder,"EMPIRICAL_REVIEW_RESULTS.txt"),open="wt",encoding="UTF-8")
  tryCatch({writeLines(c(notes,"",capture.output(dput(cfg)),""),report)
    selected<-c("data_manifest","task_manifest","method_availability","method_summary_common_support","method_summary_available",
      "paired_series_point","interval_summary_equal_series","paired_series_interval_scores","actual_interval_budgets","interval_S_sensitivity",
      "selection_frequencies","selection_agreement","ensemble_model_weights","akaike_model_weights","numerical_issues",
      "bagged_native_codes","interval_innovation_domain_counter","fit_timing","future_timing")
    for(n in selected)if(n%in%names(tables)&&ncol(tables[[n]]))writeLines(c(paste0("===== BEGIN ",n,".csv ====="),
      readLines(file.path(folder,paste0(n,".csv")),warn=FALSE),paste0("===== END ",n,".csv ====="),""),report)
  },finally=close(report))
  ep_save(list(collector_version="empirical_review_collector_v2",collection_time=Sys.time(),
    fitting_module_md5=cfg$module_md5,software=sessionInfo()),file.path(folder,"collector_metadata.rds"))
  ep_save(list(status=st,method_summary=if("method_summary_common_support"%in%names(tables))tables$method_summary_common_support else NULL,
    numerical_issues=tables$numerical_issues),file.path(folder,"collection_result.rds"))
  cat("Method-origin statuses:",nrow(st),"; failed/not-run:",sum(!st$fit_success),"; numerical flags:",sum(st$numerical_anomaly%in%TRUE),"\n")
  cat("Report: EMPIRICAL_REVIEW_RESULTS.txt\n")
  invisible(tables)
}
ep_self_tests <- function() {
  y<-c(0,2,4);p<-c(0,1,6);q<-ep_metrics(y,p,2)
  ep_assert(identical(q$AE,c(0,1,2))&&is.na(q$APE[1])&&q$sAPE[1]==0&&all(q$scaled_AE==c(0,.5,1)),"Metric tests failed")
  cfg<-list(tie_abs=1e-10,tie_rel=1e-8)
  z<-ep_pair_stats(c(1,2,3),c(2,2,1),cfg)
  ep_assert(z$wins==1&&z$ties==1&&z$losses==1&&abs(z$difference-1/3)<1e-12,"Pair test failed")
  x<-data.frame(origin=c(1,1,2,2),actual=c(1,3,2,4),forecast=c(0,2,0,2),mase_scale=c(1,1,2,2))
  a<-ep_series_metrics(x,2)
  ep_assert(abs(a$MAE-1.5)<1e-12&&abs(a$MASE-1)<1e-12&&abs(a$RMSE-sqrt(2.5))<1e-12,"Series aggregation failed")
  toy<-expand.grid(series_id=c("a","b"),origin=1L,method=c("m1","m2"),horizon=1:12,stringsAsFactors=FALSE)
  support<-ep_common_origin_support(toy,c("m1","m2"),12L)
  ep_assert(nrow(support)==2L,"Common-support regression test failed")
  bad<-toy[-which(toy$series_id=="a"&toy$method=="m1"&toy$horizon==1L),,drop=FALSE]
  support<-ep_common_origin_support(bad,c("m1","m2"),12L)
  ep_assert(nrow(support)==1L&&support$series_id=="b","Incomplete common-support test failed")
  invisible(TRUE)
}
