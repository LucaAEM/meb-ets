# MESE v7: frozen-candidate independent validation protocol.
# No parent-study dependency. Functions only; use the separate launcher.
# Native benchmark fitting is untouched; controlled fits use the v5 multiscale rule.
for(p in c("forecast","meboot")) if(!requireNamespace(p,quietly=TRUE)) stop("Install package: ",p)

ms_config <- function(mode="timing",workers=2L) {
  z<-switch(mode,
    timing=list(R=1L,R_bag=1L,R_ablation=1L,B=10L,S=200L,seed=4651230L,B_grid=10L,S_grid=200L),
    precision=list(R=2L,R_bag=0L,R_ablation=0L,B=200L,S=4000L,seed=5651230L,
      B_grid=c(50L,100L,200L),S_grid=c(1000L,2000L,4000L)),
    validation=list(R=200L,R_bag=100L,R_ablation=50L,B=100L,S=2000L,seed=7651230L,
      B_grid=100L,S_grid=2000L),stop("Unknown mode"))
  c(list(engine="MESE_v7_frozen_original_selection_1",mode=mode,n=240L,H=12L,m=12L,
    cases=c("ETS_AAA","ETS_MAM","NONLINEAR","GARCH","BREAK","SWITCH"),
    ic="aicc",bounds="both",retry_budgets=c(10000L,30000L),polish_rounds=1L,
    scale_multipliers=c(.1,1,10),ll_tol_abs=1e-5,ll_tol_rel=1e-8,
    innovation="residual",levels=c(.8,.95),rng_kind=c("Mersenne-Twister","Inversion","Rejection"),compute_intervals=TRUE,
    keep_components=FALSE,keep_full_optimizer_log=mode!="validation",
    workers=as.integer(workers),protocol_frozen=FALSE,precision_checked=FALSE,
    software=c(R=as.character(getRversion()),forecast=as.character(utils::packageVersion("forecast")),
      meboot=as.character(utils::packageVersion("meboot")))),z)
}
ms_method_ids <- function() c("ETS_standard","ETS_controlled","Akaike_controlled",
  "MESE_original","MEB_original","BaggedETS_original","MESE_MEB_selection",
  "MEB_common","MEB_per_replica","BLD_common_ablation")
ms_code <- function(f) {
  if(inherits(f,"ms_scaled_ets")) f<-f$native
  if(is.null(f$fit$fail)) NA_integer_ else as.integer(f$fit$fail)
}
ms_id <- function(f) {
  if(inherits(f,"ms_scaled_ets")) f<-f$native
  paste0(f$components[1],f$components[2],if(isTRUE(as.logical(f$components[4]))) "d" else "",f$components[3])
}
ms_point <- function(f,H) {
  a<-if(inherits(f,"ms_scaled_ets")) f$response_scale else 1
  z<-if(inherits(f,"ms_scaled_ets")) f$native else f
  p<-as.numeric(forecast::forecast(z,h=H,PI=FALSE)$mean)*a
  if(length(p)!=H||any(!is.finite(p))) stop("Invalid point forecast")
  p
}
ms_audit <- function() {
  a<-new.env(parent=emptyenv());a$rows<-list();a$times<-list();a$issues<-list()
  a$unresolved<-list();a$counter<-0L;a$stage<-"start";a
}
ms_backend_check <- function(cfg) {
  # The budget forwarding/code audit was empirically checked on this installed version.
  if(as.character(utils::packageVersion("forecast"))!="8.23.0")
    stop("This release requires forecast 8.23.0. Review optimizer forwarding before using another version.")
  if(as.character(utils::packageVersion("meboot"))!="1.4.9.4")
    stop("This release reproduces meboot 1.4.9.4; review other versions explicitly.")
  invisible(TRUE)
}
ms_retry <- function(args,id,stage,cfg,audit,a) {
  audit$counter<-audit$counter+1L;cid<-audit$counter;hist<-list();last<-NULL;best<- -Inf
  for(j in seq_along(cfg$retry_budgets)) {
    aa<-args;aa$maxit<-cfg$retry_budgets[j];ws<-character();err<-"";t<-proc.time()[["elapsed"]]
    f<-tryCatch(withCallingHandlers(do.call(forecast::ets,aa),warning=function(w){
      ws<<-c(ws,conditionMessage(w));invokeRestart("muffleWarning")}),
      error=function(e){err<<-conditionMessage(e);NULL})
    finite<-!is.null(f)&&inherits(f,"ets")&&length(f$loglik)==1&&is.finite(f$loglik)&&
      length(f$residuals)==length(args$y)&&all(is.finite(f$residuals))&&all(is.finite(f$states))
    code<-if(is.null(f)) NA_integer_ else ms_code(f)
    ll<-if(finite) f$loglik-length(args$y)*log(a) else NA_real_
    if(finite) best<-max(best,ll)
    row<-data.frame(call_id=cid,stage=stage,model=id,response_scale=a,attempt=j,maxit=aa$maxit,
      code=code,evaluations=if(is.null(f$fit$fncount)) NA_real_ else f$fit$fncount,
      finite_fit=finite,loglik_original_scale=ll,seconds=proc.time()[["elapsed"]]-t,
      warnings=paste(unique(ws),collapse=" | "),error=err)
    hist[[j]]<-row;audit$rows[[length(audit$rows)+1L]]<-row;last<-f
    if(finite&&!is.na(code)&&code==0L) return(list(ok=TRUE,fit=f,ll=ll,best_finite_ll=best))
    if(!is.na(code)&&code==10L) break
  }
  list(ok=FALSE,fit=last,ll=NA_real_,best_finite_ll=best,history=do.call(rbind,hist))
}
ms_fit <- function(y,spec,cfg,audit,stage,fixed=NULL) {
  c0<-cfg$training_scale
  if(is.null(c0)||!is.finite(c0)||c0<=0) stop("Invalid original-training scale")
  y<-ts(as.numeric(y),start=start(y),frequency=frequency(y));bestfinite<- -Inf;candidates<-list();records<-list()
  for(j in seq_along(cfg$scale_multipliers)) {
    a<-c0*cfg$scale_multipliers[j]
    args<-list(y=y/a,model=spec$model,damped=spec$damped,lambda=NULL,opt.crit="lik",
      bounds=cfg$bounds,ic=cfg$ic,restrict=TRUE,allow.multiplicative.trend=FALSE)
    if(!is.null(fixed)) args<-c(args,as.list(fixed))
    prefix<-paste0(stage,"/scale_",j)
    fr<-ms_retry(args,spec$id,paste0(prefix,"/",if(is.null(fixed)) "free" else "fixed"),cfg,audit,a)
    bestfinite<-max(bestfinite,fr$best_finite_ll);records[[j]]<-fr
    if(!fr$ok) next
    f<-fr$fit;ll<-fr$ll
    if(is.null(fixed)) for(i in seq_len(cfg$polish_rounds)) {
      th<-f$par[intersect(c("alpha","beta","gamma","phi"),names(f$par))]
      ar<-args;for(nm in names(th)) ar[[nm]]<-unname(th[nm])
      gr<-ms_retry(ar,spec$id,paste0(prefix,"/state_refinement_",i),cfg,audit,a)
      bestfinite<-max(bestfinite,gr$best_finite_ll)
      if(gr$ok&&gr$ll>ll) {f<-gr$fit;ll<-gr$ll}
    }
    if(!is.null(fixed)) {
      v<-f$par[names(fixed)]
      if(any(!is.finite(v))||max(abs(v-fixed))>1e-8) stop("Fixed coefficients changed")
    }
    candidates[[length(candidates)+1L]]<-list(native=f,response_scale=a,loglik=ll)
  }
  if(!length(candidates)) {
    audit$unresolved[[length(audit$unresolved)+1L]]<-list(y=y,spec=spec,stage=stage,fixed=fixed,records=records)
    stop("No converged scale: ",stage)
  }
  ans<-candidates[[which.max(vapply(candidates,function(z) z$loglik,numeric(1)))]]
  ns<-1L+as.integer(substr(spec$model,2,2)!="N")+(if(substr(spec$model,3,3)!="N") frequency(y)-1L else 0L)
  nd<-1L+as.integer(substr(spec$model,2,2)!="N")+as.integer(substr(spec$model,3,3)!="N")+as.integer(spec$damped)
  k<-ns+nd+1L;n<-length(y);ans$k<-k
  ans$aic<- -2*ans$loglik+2*k;ans$aicc<-if(n>k+1) ans$aic+2*k*(k+1)/(n-k-1) else Inf
  ans$bic<- -2*ans$loglik+log(n)*k
  ans$par<-ans$native$par[intersect(c("alpha","beta","gamma","phi"),names(ans$native$par))]
  ans$components<-ans$native$components
  tol<-cfg$ll_tol_abs+cfg$ll_tol_rel*abs(ans$loglik);gap<-max(0,bestfinite-ans$loglik)
  ans$numerical_flag<-gap>tol
  if(ans$numerical_flag) audit$issues[[length(audit$issues)+1L]]<-data.frame(stage=stage,model=spec$id,
    issue="finite_nonaccepted_candidate_beats_selected",gap=gap,tolerance=tol)
  class(ans)<-"ms_scaled_ets";ans
}
ms_fit_grid <- function(y,g,cfg,audit,stage,cache_index=NULL,cache_fit=NULL) {
  z<-lapply(seq_len(nrow(g)),function(k) {
    if(!is.null(cache_index)&&k==cache_index&&!is.null(cache_fit)) return(cache_fit)
    ms_fit(y,as.list(g[k,]),cfg,audit,paste0(stage,"/",g$id[k]))
  });names(z)<-g$id;z
}
ms_scores <- function(fs,cfg) vapply(fs,function(f) f[[cfg$ic]],numeric(1))
ms_pick <- function(fs,cfg) which.min(ms_scores(fs,cfg))
ms_common <- function(gs,cfg) which.min(Reduce(`+`,lapply(gs,ms_scores,cfg=cfg))/length(gs))
ms_pack <- function(f,H) {
  a<-if(inherits(f,"ms_scaled_ets")) f$response_scale else 1
  n<-if(inherits(f,"ms_scaled_ets")) f$native else f
  if(!is.null(n$lambda)) stop("Transformed native fitted objects need a separate packing rule")
  last<-as.numeric(tail(n$states,1L))
  z<-structure(list(x=ts(tail(as.numeric(n$x),1),start=tsp(n$x)[2],frequency=frequency(n$x)),m=n$m,
    states=rbind(last,last),components=n$components,par=n$par[intersect(c("alpha","beta","gamma","phi"),names(n$par))],
    sigma2=n$sigma2,residuals=as.numeric(n$residuals),lambda=NULL),class="ets")
  p<-as.numeric(stats::simulate(n,nsim=H,future=TRUE,innov=rep(0,H)))
  q<-as.numeric(stats::simulate(z,nsim=H,future=TRUE,innov=rep(0,H)))
  if(any(!is.finite(p))||any(!is.finite(q))||max(abs(p-q))>1e-7*max(1,max(abs(p)))) stop("Packed replay mismatch")
  if(any(!is.finite(z$residuals))) stop("Invalid residual pool")
  attr(z,"response_scale")<-a;z
}
ms_method <- function(fits,weights,cfg,override=NULL,intervals=TRUE) {
  if(length(weights)!=length(fits)||any(!is.finite(weights))||any(weights<0)||sum(weights)<=0) stop("Invalid weights")
  w<-weights/sum(weights)
  p<-if(is.null(override)) as.numeric(vapply(fits,ms_point,numeric(cfg$H),H=cfg$H)%*%w) else as.numeric(override)
  if(length(p)!=cfg$H||any(!is.finite(p))) stop("Invalid method point")
  members<-NULL;err<-""
  if(intervals&&cfg$compute_intervals) members<-tryCatch(lapply(fits,ms_pack,H=cfg$H),error=function(e){err<<-conditionMessage(e);NULL})
  list(point=p,members=members,weights=w,pack_error=err,intervals_planned=intervals&&cfg$compute_intervals)
}
ms_standard <- function(y,root,cfg) {
  f<-forecast::ets(y);mm<-ms_method(list(f),1,cfg,intervals=FALSE)
  mm$intervals_planned<-cfg$compute_intervals;mm$interval_engine<-"forecast_ets_native"
  mm$native_intervals<-NULL;mm$native_interval_error<-""
  if(cfg$compute_intervals) {
    set.seed(root+3L);q<-tryCatch(forecast::forecast(f,h=cfg$H,level=100*cfg$levels,PI=TRUE),error=function(e)e)
    if(inherits(q,"error")) mm$native_interval_error<-conditionMessage(q) else
      mm$native_intervals<-list(level=as.numeric(q$level),lower=as.matrix(q$lower),upper=as.matrix(q$upper))
  }
  list(method=mm,selected=ms_id(f),code=ms_code(f))
}
ms_parameter <- function(y,fs,spec,reference,cfg,audit,stage) {
  needed<-c("alpha",if(substr(spec$model,2,2)!="N") "beta",if(substr(spec$model,3,3)!="N") "gamma",if(spec$damped) "phi")
  C<-vapply(fs,function(f) as.numeric(f$par[needed]),numeric(length(needed)))
  if(is.null(dim(C))) C<-matrix(C,nrow=length(needed));rownames(C)<-needed
  meanpar<-rowMeans(C);f<-ms_fit(y,spec,cfg,audit,stage,fixed=meanpar)
  gain<-f$loglik-reference$loglik;tol<-cfg$ll_tol_abs+cfg$ll_tol_rel*abs(reference$loglik)
  if(gain>tol) audit$issues[[length(audit$issues)+1L]]<-data.frame(stage=stage,model=spec$id,
    issue="parameter_refit_beats_corresponding_free_fit",gap=gain,tolerance=tol)
  list(method=ms_method(list(f),1,cfg),fit=f,coefficients=C,mean=meanpar,gain=gain)
}
ms_events <- function(y,p,sc,case,r,name) {
  e<-y-p;data.frame(case=case,replication=r,method=name,horizon=seq_along(y),actual=y,forecast=p,
    AE=abs(e),SE=e^2,scaled_AE=if(sc>0) abs(e)/sc else NA_real_)
}

ms_grid <- function(positive) {
  g<-data.frame(id=c("ANN","AAN","AAdN","ANA","AAA","AAdA"),
    model=c("ANN","AAN","AAN","ANA","AAA","AAA"),damped=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE))
  if(positive) g<-rbind(g,data.frame(id=c("MNM","MAM","MAdM"),model=c("MNM","MAM","MAM"),damped=c(FALSE,FALSE,TRUE)))
  g
}

ms_time <- function(stage,audit,expr) {
  audit$stage<-stage;t<-proc.time()[["elapsed"]]
  on.exit({audit$times[[length(audit$times)+1L]]<-data.frame(stage=stage,seconds=proc.time()[["elapsed"]]-t)},add=TRUE)
  force(expr)
}

ms_module <- function(fun,audit) {
  ws<-character()
  ans<-tryCatch(withCallingHandlers(fun(),warning=function(w){ws<<-c(ws,conditionMessage(w));invokeRestart("muffleWarning")}),
    error=function(e)e)
  if(inherits(ans,"error")) list(ok=FALSE,error=conditionMessage(ans),stage=audit$stage,warnings=unique(ws))
  else list(ok=TRUE,value=ans,error="",stage="",warnings=unique(ws))
}

ms_dgp <- function(case,cfg) {
  n<-cfg$n;m<-cfg$m;N<-n+cfg$H;tt<-seq_len(N)
  if(case%in%c("ETS_AAA","ETS_MAM")) {
    mult<-case=="ETS_MAM";s<-if(mult) 1+.15*sin(2*pi*seq_len(m)/m) else 2*sin(2*pi*seq_len(m)/m)
    f<-structure(list(x=NULL,m=m,states=matrix(c(100,if(mult) 1 else .05,rev(s)),nrow=1),
      components=c(if(mult) "M" else "A","A",if(mult) "M" else "A","FALSE"),
      par=c(alpha=.3,beta=if(mult) .005 else .04,gamma=.08),
      sigma2=if(mult) .02^2 else 1,residuals=0,lambda=NULL),class="ets")
    e<-if(mult) runif(N,-sqrt(3)*.02,sqrt(3)*.02) else rnorm(N)
    y<-as.numeric(stats::simulate(f,nsim=N,future=FALSE,innov=e))
    if(mult&&any(y<=0)) stop("Multiplicative DGP left positive domain")
  } else if(case=="NONLINEAR") {
    v<-numeric(N+500L);for(t in 2:length(v)) v[t]<-.6*v[t-1]+.3*tanh(v[t-1]^2)+rnorm(1);y<-tail(v,N)
  } else if(case=="GARCH") {
    v<-numeric(N+500L);sv<-.5
    for(t in seq_along(v)) {sv<-.1+.3*(if(t>1) v[t-1]^2 else 0)+.5*sv;v[t]<-sqrt(sv)*rnorm(1)}
    y<-tail(v,N)
  } else if(case=="BREAK") {
    s<-sin(2*pi*tt/m);y<-ifelse(tt<=n-m,1+s,2+1.5*s)+rnorm(N)
  } else if(case=="SWITCH") {
    st<-sample.int(2,1,prob=c(.6,.4));y<-numeric(N);drift<-c(.2,.9)
    for(t in seq_len(N)) {y[t]<-(if(t>1) y[t-1] else 0)+drift[st]+rnorm(1)
      if(runif(1)<(if(st==1L) .1 else .15)) st<-3L-st}
  } else stop("Unknown DGP")
  if(any(!is.finite(y))) stop("Nonfinite DGP")
  list(train=ts(y[seq_len(n)],frequency=m),test=y[n+seq_len(cfg$H)],positive=case=="ETS_MAM",
    truth=if(case=="ETS_AAA") "AAA" else if(case=="ETS_MAM") "MAM" else NA_character_)
}

ms_root_seed <- function(case,r,cfg) {
  ids<-c("ETS_AAA","ETS_MAM","NONLINEAR","GARCH","BREAK","SWITCH")
  cfg$seed+match(case,ids)*100000L+r*100L
}


ms_build <- function(case,r,cfg,audit) {
  do.call(RNGkind,as.list(cfg$rng_kind))
  root<-ms_root_seed(case,r,cfg);set.seed(root)
  d<-ms_time("dgp",audit,ms_dgp(case,cfg));y<-d$train;g<-ms_grid(d$positive)
  cfg$training_scale<-sd(as.numeric(y));if(!is.finite(cfg$training_scale)||cfg$training_scale<=0) stop("Degenerate training")
  methods<-list();status<-list();selection<-list();diag<-list();original<-NULL;ib<-NULL
  add<-function(name,module,selected=NA_character_,interval_plan=cfg$compute_intervals) {
    status[[length(status)+1L]]<<-data.frame(case=case,replication=r,method=name,fit_success=module$ok,
      fit_error=module$error,failed_stage=module$stage,fit_warnings=paste(module$warnings,collapse=" | "),
      intervals_planned=interval_plan)
    if(module$ok) {
      methods[[name]]<<-module$value
      selection[[length(selection)+1L]]<<-data.frame(case=case,replication=r,method=name,selected_model=selected,truth=d$truth)
    }
  }
  unwrap<-function(x,field="method") {if(x$ok) x$value<-x$value[[field]];x}
  dependency<-function(x,label) list(ok=FALSE,error=paste0(label,": ",x$error),stage=x$stage,warnings=x$warnings)
  native<-ms_module(function() ms_time("ETS_standard",audit,ms_standard(y,root,cfg)),audit)
  if(native$ok) diag$standard_code<-native$value$code
  add("ETS_standard",unwrap(native),if(native$ok) native$value$selected else NA_character_)
  orig<-ms_module(function() ms_time("controlled_original",audit,ms_fit_grid(y,g,cfg,audit,"original")),audit)
  if(orig$ok) {
    original<-orig$value;ib<-ms_pick(original,cfg)
    q<-ms_module(function() ms_time("ETS_controlled_packing",audit,ms_method(list(original[[ib]]),1,cfg)),audit)
    add("ETS_controlled",q,g$id[ib])
    q<-ms_module(function() ms_time("Akaike_packing",audit,{
      s<-ms_scores(original,cfg);w<-exp(-.5*(s-min(s)));ms_method(original,w/sum(w),cfg)
    }),audit);add("Akaike_controlled",q)
    diag$original_scores<-data.frame(model=g$id,AIC=vapply(original,function(f) f$aic,numeric(1)),
      AICc=vapply(original,function(f) f$aicc,numeric(1)))
  } else {add("ETS_controlled",orig);add("Akaike_controlled",orig)}
  need_ab<-r<=cfg$R_ablation;need_bag<-r<=cfg$R_bag
  paths<-list(ok=FALSE,error="MEB not required",stage="not_scheduled",warnings=character())
  if(orig$ok||need_ab) paths<-ms_module(function() ms_time("MEB_generation",audit,{
    set.seed(root+1L);trim<-if(d$positive) list(trim=.1,xmin=0,xmax=NULL) else .1
    P<-as.matrix(meboot::meboot(y,reps=cfg$B,trim=trim,reachbnd=TRUE,expand.sd=FALSE,
      force.clt=FALSE,scl.adjustment=FALSE,sym=FALSE,elaps=FALSE)$ens)
    if(nrow(P)!=cfg$n||ncol(P)!=cfg$B||any(!is.finite(P))||(d$positive&&any(P<=0))) stop("Invalid MEB ensemble/domain")
    P
  }),audit)
  bank<-list(ok=FALSE,error="Original grid unavailable",stage="controlled_original",warnings=orig$warnings)
  if(orig$ok&&paths$ok) bank<-ms_module(function() ms_time("primary_MEB_fits",audit,
    lapply(seq_len(cfg$B),function(b) ms_fit(ts(paths$value[,b],frequency=cfg$m),as.list(g[ib,]),cfg,audit,
      paste0("primary_MEB[",b,"]/",g$id[ib])))),audit)
  else if(orig$ok&&!paths$ok) bank<-dependency(paths,"MEB generation failed")
  if(bank$ok) {
    q<-ms_module(function() ms_time("MEB_original_packing",audit,ms_method(bank$value,rep(1/cfg$B,cfg$B),cfg)),audit)
    add("MEB_original",q,g$id[ib])
    pm<-ms_module(function() ms_time("MESE_original_refit",audit,
      ms_parameter(y,bank$value,as.list(g[ib,]),original[[ib]],cfg,audit,"MESE_original")),audit)
    add("MESE_original",unwrap(pm),g$id[ib])
    if(pm$ok) diag$primary<-list(model=g$id[ib],mean=pm$value$mean,
      bootstrap_coefficients=pm$value$coefficients,parameter_minus_free_loglik=pm$value$gain)
    # Prefix sensitivity uses the SAME paths and fitted members, not new bootstrap runs.
    if(length(cfg$B_grid)>1L) {
      prec<-list()
      for(B in cfg$B_grid[cfg$B_grid<cfg$B]) {
        fs<-bank$value[seq_len(B)]
        nm<-paste0("MEB_original_B",B)
        q<-ms_module(function() ms_method(fs,rep(1/B,B),cfg),audit);add(nm,q,g$id[ib])
        nm<-paste0("MESE_original_B",B)
        pmb<-ms_module(function() ms_parameter(y,fs,as.list(g[ib,]),original[[ib]],cfg,audit,nm),audit)
        add(nm,unwrap(pmb),g$id[ib])
        if(pmb$ok) prec[[as.character(B)]]<-list(mean=pmb$value$mean,coefficients=pmb$value$coefficients)
      }
      diag$prefix_coefficients<-prec
    }
  } else {
    add("MEB_original",bank);add("MESE_original",bank)
    if(length(cfg$B_grid)>1L) for(B in cfg$B_grid[cfg$B_grid<cfg$B]) {
      add(paste0("MEB_original_B",B),bank);add(paste0("MESE_original_B",B),bank)
    }
  }
  if(need_ab) {
    ab<-if(!paths$ok) dependency(paths,"MEB ablation generation failed") else ms_module(function() {
      gs<-ms_time("ablation_MEB_grid",audit,lapply(seq_len(cfg$B),function(b) {
        cached<-if(bank$ok) bank$value[[b]] else NULL
        ms_fit_grid(ts(paths$value[,b],frequency=cfg$m),g,cfg,audit,paste0("ablation_MEB[",b,"]"),
          cache_index=if(bank$ok) ib else NULL,cache_fit=cached)
      }))
      im<-ms_common(gs,cfg);common<-lapply(gs,function(z) z[[im]])
      individual<-lapply(gs,function(z) z[[ms_pick(z,cfg)]])
      Q<-do.call(rbind,lapply(gs,ms_scores,cfg=cfg));colnames(Q)<-g$id
      C<-array(NA_real_,c(cfg$B,nrow(g),4L),dimnames=list(NULL,g$id,c("alpha","beta","gamma","phi")))
      for(b in seq_len(cfg$B)) for(k in seq_len(nrow(g))) C[b,k,names(gs[[b]][[k]]$par)]<-gs[[b]][[k]]$par
      list(im=im,common=common,individual=individual,Q=Q,C=C,
        models=vapply(individual,ms_id,character(1)))
    },audit)
    if(ab$ok) {
      a<-ab$value;im<-a$im
      diag$ablation<-list(common_model=g$id[im],IC=a$Q,coefficients=a$C,per_replica_models=a$models)
      if(!is.null(ib)&&im==ib&&!is.null(methods$MEB_original)) {
        q<-list(ok=TRUE,value=methods$MEB_original,error="",stage="",warnings=character())
        q$value$alias_of<-"MEB_original"
      } else q<-ms_module(function() ms_method(a$common,rep(1/cfg$B,cfg$B),cfg),audit)
      add("MEB_common",q,g$id[im])
      if(!is.null(ib)&&all(a$models==g$id[ib])&&!is.null(methods$MEB_original)) {
        q<-list(ok=TRUE,value=methods$MEB_original,error="",stage="",warnings=character())
        q$value$alias_of<-"MEB_original"
      } else q<-ms_module(function() ms_method(a$individual,rep(1/cfg$B,cfg$B),cfg),audit)
      add("MEB_per_replica",q)
      if(!is.null(ib)&&im==ib&&!is.null(methods$MESE_original)) {
        q<-list(ok=TRUE,value=methods$MESE_original,error="",stage="",warnings=character())
        q$value$alias_of<-"MESE_original";add("MESE_MEB_selection",q,g$id[im])
      } else {
        pm<-ms_module(function() {
          ref<-if(!is.null(original)) original[[im]] else ms_fit(y,as.list(g[im,]),cfg,audit,"ablation_reference")
          ms_time("ablation_parameter_refit",audit,ms_parameter(y,a$common,as.list(g[im,]),ref,cfg,audit,"MESE_MEB_selection"))
        },audit)
        add("MESE_MEB_selection",unwrap(pm),g$id[im])
      }
    } else for(nm in c("MEB_common","MEB_per_replica","MESE_MEB_selection")) add(nm,ab)
  }
  if(need_bag||need_ab) {
    boot<-ms_module(function() ms_time("BLD_generation",audit,{
      set.seed(root+2L);P<-forecast::bld.mbb.bootstrap(y,num=cfg$B)
      if(!is.list(P)||length(P)!=cfg$B||any(!vapply(P,function(v) length(v)==cfg$n&&all(is.finite(v)),logical(1)))) stop("Invalid native BLD ensemble")
      P
    }),audit)
    if(need_bag) {
      bag<-if(!boot$ok) boot else ms_module(function() ms_time("BaggedETS_original",audit,{
        f<-forecast::baggedETS(y,bootstrapped_series=boot$value)
        p<-as.numeric(forecast::forecast(f,h=cfg$H)$mean)
        if(length(p)!=cfg$H||any(!is.finite(p))) stop("Native Bagged ETS failed")
        list(method=ms_method(f$models,rep(1/length(f$models),length(f$models)),cfg,override=p,intervals=FALSE),
          codes=vapply(f$models,ms_code,integer(1)))
      }),audit)
      if(bag$ok) diag$bagged_codes<-bag$value$codes
      add("BaggedETS_original",unwrap(bag),interval_plan=FALSE)
    }
    if(need_ab) {
      bc<-if(!boot$ok) boot else ms_module(function() {
        if(d$positive&&any(vapply(boot$value,function(v) any(v<=0),logical(1)))) stop("BLD controlled positivity violation")
        gs<-ms_time("BLD_grid",audit,lapply(seq_len(cfg$B),function(b) {
          if(!is.null(original)&&isTRUE(all.equal(as.numeric(boot$value[[b]]),as.numeric(y),tolerance=0))) original else
            ms_fit_grid(boot$value[[b]],g,cfg,audit,paste0("BLD[",b,"]"))
        }))
        ic<-ms_common(gs,cfg);Q<-do.call(rbind,lapply(gs,ms_scores,cfg=cfg));colnames(Q)<-g$id
        list(method=ms_method(lapply(gs,function(z) z[[ic]]),rep(1/cfg$B,cfg$B),cfg),selected=g$id[ic],Q=Q)
      },audit)
      if(bc$ok) diag$BLD_IC<-bc$value$Q
      add("BLD_common_ablation",unwrap(bc),if(bc$ok) bc$value$selected else NA_character_)
    }
  }
  st<-do.call(rbind,status);st$numerical_anomaly<-FALSE
  if(length(audit$issues)) {
    issues<-do.call(rbind,audit$issues)
    oi<-any(grepl("^original/",issues$stage));pi<-any(grepl("^primary_MEB\\[",issues$stage))
    mi<-any(grepl("^ablation_MEB\\[",issues$stage));bi<-any(grepl("^BLD\\[",issues$stage))
    base_method<-sub("_B[0-9]+$","",st$method)
    st$numerical_anomaly<- (base_method%in%c("ETS_controlled","Akaike_controlled","MESE_original","MEB_original")&oi) |
      (base_method%in%c("MESE_original","MEB_original")&pi) |
      (st$method%in%c("MESE_MEB_selection","MEB_common","MEB_per_replica")&mi) |
      (st$method=="BLD_common_ablation"&bi)
    for(nm in st$method) if(any(grepl(paste0("^",nm,"(/|$)"),issues$stage))) st$numerical_anomaly[st$method==nm]<-TRUE
    # Alias methods inherit any relevant numerical issue of the identical source method.
    for(nm in names(methods)) if(!is.null(methods[[nm]]$alias_of)) {
      source<-methods[[nm]]$alias_of
      st$numerical_anomaly[st$method==nm]<-st$numerical_anomaly[st$method==nm]||any(st$numerical_anomaly[st$method==source])
    }
  }
  sc<-mean(abs(as.numeric(y)[(cfg$m+1L):cfg$n]-as.numeric(y)[seq_len(cfg$n-cfg$m)]))
  ev<-if(length(methods)) do.call(rbind,lapply(names(methods),function(nm) ms_events(d$test,methods[[nm]]$point,sc,case,r,nm))) else data.frame()
  if(length(cfg$B_grid)>1L) {
    bp<-list()
    for(base in c("MESE_original","MEB_original")) if(!is.null(methods[[base]])) for(B in cfg$B_grid[cfg$B_grid<cfg$B]) {
      nm<-paste0(base,"_B",B)
      if(!is.null(methods[[nm]])) {
        delta<-methods[[nm]]$point-methods[[base]]$point
        bp[[length(bp)+1L]]<-data.frame(case=case,replication=r,method=base,B=B,reference_B=cfg$B,
          max_difference=max(abs(delta)),RMS_difference=sqrt(mean(delta^2)),
          max_difference_over_training_SD=max(abs(delta))/cfg$training_scale)
      }
    }
    diag$B_sensitivity<-if(length(bp)) do.call(rbind,bp) else data.frame()
  }
  list(case=case,replication=r,root_seed=root,training_scale=cfg$training_scale,train=as.numeric(y),test=d$test,
    methods=methods,status=st,point_events=ev,selection=if(length(selection)) do.call(rbind,selection) else data.frame(),diagnostic=diag)
}


ms_interval_events <- function(lo,hi,y,case,r,name,level,S,label) {
  # Native forecast bounds can retain ts attributes after matrix subsetting.
  # Scoring must operate on plain numeric values, not time-series arithmetic.
  lo<-as.numeric(lo);hi<-as.numeric(hi);y<-as.numeric(y)
  level<-as.numeric(level)
  if(length(level)!=1L||!is.finite(level)||level<=0||level>=1) stop("Invalid nominal interval level")
  if(!length(y)||length(lo)!=length(y)||length(hi)!=length(y)||
     any(!is.finite(y))||any(!is.finite(lo))||any(!is.finite(hi))||any(lo>hi))
    stop("Invalid interval bounds or outcomes")
  a<-1-level
  miss_lower<-pmax(lo-y,0)
  miss_upper<-pmax(y-hi,0)
  data.frame(case=as.character(case),replication=as.integer(r),method=as.character(name),
    interval_construction=as.character(label),level=level,horizon=seq_along(y),
    S=as.integer(S),lower=lo,upper=hi,covered=as.numeric(y>=lo&y<=hi),
    width=hi-lo,IS=hi-lo+(2/a)*(miss_lower+miss_upper),stringsAsFactors=FALSE)
}

ms_intervals <- function(method,name,truth,case,r,root,training_scale,cfg) {
  if(name=="ETS_standard") {
    if(nzchar(method$native_interval_error)) stop(method$native_interval_error)
    z<-method$native_intervals;if(is.null(z)) stop("Missing native intervals")
    ev<-do.call(rbind,lapply(cfg$levels,function(level) {
      j<-which(abs(z$level-100*level)<1e-8);if(length(j)!=1) stop("Native level mismatch")
      ms_interval_events(z$lower[,j],z$upper[,j],truth,case,r,name,level,NA_integer_,"forecast_ETS_native_default")
    }))
    return(list(events=ev,clipped=NA_integer_,S_sensitivity=data.frame()))
  }
  if(nzchar(method$pack_error)) stop(method$pack_error)
  if(is.null(method$members)) stop("Simulation components missing")
  canonical<-if(is.null(method$alias_of)) name else method$alias_of
  canonical<-sub("_B[0-9]+$","",canonical)
  id<-match(canonical,ms_method_ids());if(is.na(id)) stop("No deterministic future seed for method")
  do.call(RNGkind,as.list(cfg$rng_kind));set.seed(root+10L+id)
  S<-cfg$S;H<-cfg$H;weights<-method$weights/sum(method$weights)
  ix<-sample.int(length(method$members),S,replace=TRUE,prob=weights)
  pools<-lapply(method$members,function(f) {e<-as.numeric(f$residuals);e-mean(e)})
  draws<-matrix(NA_real_,S,H);clip<-0L
  for(i in seq_len(S)) {
    b<-ix[i];f<-method$members[[b]]
    e<-if(cfg$innovation=="residual") sample(pools[[b]],H,replace=TRUE) else rnorm(H,sd=sqrt(f$sigma2))
    if(f$components[1]=="M") clip<-clip+sum(e< -1)
    a<-attr(f,"response_scale");if(is.null(a)) a<-1
    draws[i,]<-as.numeric(stats::simulate(f,nsim=H,future=TRUE,bootstrap=FALSE,innov=e))*a
  }
  if(any(!is.finite(draws))) stop("Nonfinite simulated future: no deletion/regeneration")
  label<-paste0(cfg$innovation,if(length(method$members)==1) "_plugin" else "_mixture")
  ev<-list();ss<-list()
  for(level in cfg$levels) {
    a<-1-level;lo<-apply(draws,2,quantile,probs=a/2,type=8,names=FALSE)
    hi<-apply(draws,2,quantile,probs=1-a/2,type=8,names=FALSE)
    ev[[length(ev)+1L]]<-ms_interval_events(lo,hi,truth,case,r,name,level,S,label)
    for(s in cfg$S_grid[cfg$S_grid<S]) {
      lower<-apply(draws[seq_len(s),,drop=FALSE],2,quantile,probs=a/2,type=8,names=FALSE)
      upper<-apply(draws[seq_len(s),,drop=FALSE],2,quantile,probs=1-a/2,type=8,names=FALSE)
      ss[[length(ss)+1L]]<-data.frame(case=case,replication=r,method=name,level=level,S=s,reference_S=S,
        max_bound_difference=max(abs(c(lower-lo,upper-hi))),
        max_bound_difference_over_training_SD=max(abs(c(lower-lo,upper-hi)))/training_scale)
    }
  }
  list(events=do.call(rbind,ev),clipped=clip,S_sensitivity=if(length(ss)) do.call(rbind,ss) else data.frame())
}
ms_save <- function(x,file) {
  tmp<-paste0(file,".tmp");saveRDS(x,tmp)
  if(file.exists(file)&&!file.remove(file)) stop("Cannot replace checkpoint")
  if(!file.rename(tmp,file)) stop("Cannot commit checkpoint")
}
ms_worker <- function(task,cfg,folder,retry_failed=FALSE) {
  ca<-task$case;r<-task$r;stem<-file.path(folder,sprintf("%s_%05d",ca,r));pf<-paste0(stem,"_fit.rds")
  z<-if(file.exists(pf)) readRDS(pf) else NULL
  if(is.null(z)||(retry_failed&&(isTRUE(z$fatal)||any(!z$status$fit_success)))) {
    old<-list.files(folder,pattern=paste0("^",ca,"_",sprintf("%05d",r),"__.*_interval.rds$"),full.names=TRUE)
    if(length(old)) unlink(old)
    audit<-ms_audit();t<-proc.time()[["elapsed"]]
    z<-tryCatch(ms_build(ca,r,cfg,audit),error=function(e) list(fatal=TRUE,error=conditionMessage(e),failed_stage=audit$stage))
    z$case<-ca;z$replication<-r;z$fit_wall_seconds<-proc.time()[["elapsed"]]-t
    z$timing<-if(length(audit$times)) do.call(rbind,audit$times) else data.frame()
    measured<-if(nrow(z$timing)) sum(z$timing$seconds) else 0
    overhead<-max(0,z$fit_wall_seconds-measured)
    z$timing<-rbind(z$timing,data.frame(stage="fit_overhead",seconds=overhead))
    at<-if(length(audit$rows)) do.call(rbind,audit$rows) else data.frame()
    if(nrow(at)) {at$case<-ca;at$replication<-r}
    z$attempt_summary<-if(nrow(at)) data.frame(calls=sum(at$attempt==1L),retries=sum(at$attempt>1L),
      unknown_codes=sum(is.na(at$code)),optimizer_seconds=sum(at$seconds)) else data.frame()
    z$optimizer_attempts<-if(cfg$keep_full_optimizer_log||isTRUE(z$fatal)) at else
      at[at$attempt>1L|is.na(at$code)|at$code!=0|nzchar(at$warnings)|nzchar(at$error),,drop=FALSE]
    z$numerical_issues<-if(length(audit$issues)) do.call(rbind,audit$issues) else data.frame()
    z$unresolved<-audit$unresolved;ms_save(z,pf)
  }
  if(isTRUE(z$fatal)) return(data.frame(case=ca,replication=r,state="fatal",fit_seconds=z$fit_wall_seconds))
  if(cfg$compute_intervals) for(nm in names(z$methods)) {
    method<-z$methods[[nm]];if(!method$intervals_planned) next
    ip<-paste0(stem,"__",nm,"_interval.rds");q<-if(file.exists(ip)) readRDS(ip) else NULL
    if(is.null(q)||(retry_failed&&!isTRUE(q$success))) {
      t<-proc.time()[["elapsed"]];ws<-character();q<-NULL
      if(!is.null(method$alias_of)) {
        source<-paste0(stem,"__",method$alias_of,"_interval.rds")
        if(file.exists(source)) {
          sq<-readRDS(source)
          if(isTRUE(sq$success)) {q<-sq;q$events$method<-nm
            if(nrow(q$S_sensitivity)) q$S_sensitivity$method<-nm
            q$reused_identical_method<-method$alias_of}
        }
      }
      if(is.null(q)) q<-tryCatch(withCallingHandlers(ms_intervals(method,nm,z$test,ca,r,z$root_seed,z$training_scale,cfg),
        warning=function(w){ws<<-c(ws,conditionMessage(w));invokeRestart("muffleWarning")}),
        error=function(e) list(error=conditionMessage(e)))
      q$success<-is.null(q$error);q$warnings<-unique(ws);q$seconds<-proc.time()[["elapsed"]]-t
      q$case<-ca;q$replication<-r;q$method<-nm;ms_save(q,ip)
    }
    if(isTRUE(q$success)&&!cfg$keep_components&&!is.null(z$methods[[nm]]$members)) {
      z$methods[[nm]]$members<-NULL;ms_save(z,pf)
    }
  }
  data.frame(case=ca,replication=r,state=if(all(z$status$fit_success)) "fit_completed" else "partial",fit_seconds=z$fit_wall_seconds)
}
ms_fingerprint <- function() {
  env<-environment(ms_worker);nm<-sort(ls(env,pattern="^ms_"));nm<-nm[vapply(nm,function(n) is.function(get(n,env)),logical(1))]
  txt<-unlist(lapply(nm,function(n) {f<-get(n,env);c(n,deparse(formals(f)),deparse(body(f),width.cutoff=500))}))
  tmp<-tempfile();on.exit(unlink(tmp));writeLines(txt,tmp);unname(tools::md5sum(tmp))
}
ms_run <- function(cfg,folder,confirm_validation=FALSE,retry_failed=FALSE) {
  if(cfg$mode=="validation"&&(!confirm_validation||!isTRUE(cfg$protocol_frozen)||!isTRUE(cfg$precision_checked)))
    stop("Validation requires confirm_validation=TRUE, protocol_frozen=TRUE and documented precision_checked=TRUE")
  ms_backend_check(cfg)
  if(cfg$seed+700000L>=.Machine$integer.max) stop("Seed range too large")
  stopifnot(cfg$R>=1L,cfg$R<1000L,cfg$R_bag>=0L,cfg$R_ablation>=0L,
    cfg$R_bag<=cfg$R,cfg$R_ablation<=cfg$R,cfg$B>=2L,cfg$S>=20L,cfg$workers>=1L,
    !anyDuplicated(cfg$cases),all(cfg$B_grid>=2 & cfg$B_grid<=cfg$B),
    all(cfg$S_grid>=20 & cfg$S_grid<=cfg$S),all(cfg$scale_multipliers>0),
    all(cfg$levels>0 & cfg$levels<1),cfg$innovation%in%c("residual","gaussian"))
  if(!all(cfg$cases%in%c("ETS_AAA","ETS_MAM","NONLINEAR","GARCH","BREAK","SWITCH"))) stop("Unknown scenario")
  dir.create(folder,recursive=TRUE,showWarnings=FALSE);folder<-normalizePath(folder,winslash="/",mustWork=TRUE)
  identity<-cfg;identity$workers<-NULL;identity$source_fingerprint<-ms_fingerprint()
  cp<-file.path(folder,"configuration.rds")
  if(file.exists(cp)) {if(!identical(readRDS(cp),identity)) stop("Source/config mismatch: use a new folder")}
  else ms_save(identity,cp)
  ms_save(sessionInfo(),file.path(folder,"session_info.rds"))
  tasks<-unlist(lapply(cfg$cases,function(ca) lapply(seq_len(cfg$R),function(r) list(case=ca,r=r))),recursive=FALSE)
  t<-proc.time()[["elapsed"]]
  if(cfg$workers==1L) report<-lapply(tasks,ms_worker,cfg=cfg,folder=folder,retry_failed=retry_failed) else {
    cl<-parallel::makePSOCKcluster(cfg$workers);on.exit(parallel::stopCluster(cl),add=TRUE)
    env<-environment(ms_worker);nm<-ls(env,pattern="^ms_");nm<-nm[vapply(nm,function(n) is.function(get(n,env)),logical(1))]
    parallel::clusterExport(cl,nm,envir=env)
    parallel::clusterCall(cl,function(config) {
      Sys.setenv(OMP_NUM_THREADS="1",OPENBLAS_NUM_THREADS="1",MKL_NUM_THREADS="1")
      for(p in c("forecast","meboot")) if(!requireNamespace(p,quietly=TRUE)) stop("Worker missing package")
      actual<-c(R=as.character(getRversion()),forecast=as.character(utils::packageVersion("forecast")),meboot=as.character(utils::packageVersion("meboot")))
      if(!identical(actual,config$software)) stop("Worker software mismatch")
      ms_backend_check(config);NULL
    },cfg)
    report<-parallel::parLapplyLB(cl,tasks,ms_worker,cfg=cfg,folder=folder,retry_failed=retry_failed)
  }
  report<-do.call(rbind,report);wall<-proc.time()[["elapsed"]]-t
  write.csv(report,file.path(folder,"task_report.csv"),row.names=FALSE)
  ms_save(list(wall_seconds=wall,workers=cfg$workers,timestamp=Sys.time()),file.path(folder,"execution.rds"))
  cat("Wall seconds:",wall,"\n");invisible(report)
}


ms_pool <- function(events,values,groups) {
  if(!nrow(events)) return(data.frame())
  if(!all(c(values,groups)%in%names(events))||anyNA(events[,groups,drop=FALSE])) stop("Invalid pooling columns")
  z<-aggregate(events[,values,drop=FALSE],events[,groups,drop=FALSE],function(x) if(all(is.finite(x))) mean(x) else NA_real_)
  z$horizon<-0L;z
}
ms_summary <- function(d,values,groups) {
  if(!nrow(d)) return(data.frame())
  key<-do.call(interaction,c(d[groups],list(drop=TRUE,lex.order=TRUE)))
  do.call(rbind,lapply(split(d,key),function(x) {
    a<-x[1L,groups,drop=FALSE];a$R_complete<-nrow(x)
    for(v in values) {q<-x[[v]];a[[v]]<-if(all(is.finite(q))) mean(q) else NA_real_
      a[[paste0("MCSE_",v)]]<-if(length(q)>1&&all(is.finite(q))) sd(q)/sqrt(length(q)) else NA_real_}
    if("SE"%in%values) {a$RMSE<-sqrt(a$SE);a$MCSE_RMSE<-if(is.finite(a$RMSE)&&a$RMSE>0) a$MCSE_SE/(2*a$RMSE) else NA_real_}
    a
  }))
}
ms_scheduled <- function(r,cfg) {
  c("ETS_standard","ETS_controlled","Akaike_controlled","MEB_original","MESE_original",
    if(length(cfg$B_grid)>1L) unlist(lapply(cfg$B_grid[cfg$B_grid<cfg$B],function(B)
      c(paste0("MEB_original_B",B),paste0("MESE_original_B",B)))),
    if(r<=cfg$R_bag) "BaggedETS_original",
    if(r<=cfg$R_ablation) c("MEB_common","MEB_per_replica","MESE_MEB_selection","BLD_common_ablation"))
}
ms_pairs <- function(pool,reference,metric,by_level=FALSE) {
  key<-c("case","replication",if(by_level) "level")
  b<-pool[pool$method==reference,c(key,metric),drop=FALSE]
  if(!nrow(b)) return(data.frame())
  names(b)[ncol(b)]<-"reference_loss";d<-merge(pool,b,by=key)
  d$difference<-d[[metric]]-d$reference_loss
  q<-ms_summary(d,"difference",c("case","method",if(by_level) "level"))
  q$reference<-reference;q$metric<-metric
  q$lower<-q$difference-qt(.975,pmax(1,q$R_complete-1))*q$MCSE_difference
  q$upper<-q$difference+qt(.975,pmax(1,q$R_complete-1))*q$MCSE_difference
  q
}
ms_collect <- function(folder) {
  cfg<-readRDS(file.path(folder,"configuration.rds"))
  status<-points<-intervals<-selection<-times<-future<-issues<-attempts<-bprec<-sprec<-clips<-choices<-list()
  add<-function(x,y) {x[[length(x)+1L]]<-y;x}
  for(ca in cfg$cases) for(r in seq_len(cfg$R)) {
    stem<-file.path(folder,sprintf("%s_%05d",ca,r));pf<-paste0(stem,"_fit.rds")
    z<-if(file.exists(pf)) readRDS(pf) else NULL
    scheduled<-ms_scheduled(r,cfg)
    if(is.null(z)||isTRUE(z$fatal)) {
      status<-add(status,data.frame(case=ca,replication=r,method=scheduled,fit_success=FALSE,
        fit_error=if(is.null(z)) "not run" else z$error,failed_stage=if(is.null(z)) "not run" else z$failed_stage,
        fit_warnings="",intervals_planned=cfg$compute_intervals & scheduled!="BaggedETS_original",
        numerical_anomaly=NA,interval_success=NA,interval_error="",interval_seconds=NA_real_))
    } else {
      st<-z$status
      if(!setequal(st$method,scheduled)||anyDuplicated(st$method)) stop("Missing/duplicate method status in ",pf)
      st$interval_success<-NA;st$interval_error<-"";st$interval_seconds<-NA_real_
      for(j in seq_len(nrow(st))) if(st$fit_success[j]&&st$intervals_planned[j]) {
        nm<-st$method[j];ip<-paste0(stem,"__",nm,"_interval.rds")
        q<-if(file.exists(ip)) readRDS(ip) else NULL
        st$interval_success[j]<-!is.null(q)&&isTRUE(q$success)
        st$interval_error[j]<-if(is.null(q)) "not run" else if(!isTRUE(q$success)) q$error else ""
        if(!is.null(q)) {
          st$interval_seconds[j]<-q$seconds
          future<-add(future,data.frame(case=ca,replication=r,method=nm,seconds=q$seconds,
            reused= !is.null(q$reused_identical_method)))
          if(isTRUE(q$success)) {
            intervals<-add(intervals,q$events)
            if(nrow(q$S_sensitivity)) sprec<-add(sprec,q$S_sensitivity)
            clips<-add(clips,data.frame(case=ca,replication=r,method=nm,clipped=q$clipped))
          }
        }
      }
      status<-add(status,st)
      if(nrow(z$point_events)) points<-add(points,z$point_events)
      if(nrow(z$selection)) selection<-add(selection,z$selection)
      if(!is.null(z$diagnostic$B_sensitivity)&&nrow(z$diagnostic$B_sensitivity)) bprec<-add(bprec,z$diagnostic$B_sensitivity)
      if(!is.null(z$diagnostic$original_scores)) {
        q<-z$diagnostic$original_scores;q$case<-ca;q$replication<-r;choices<-add(choices,q)
      }
    }
    if(!is.null(z)) {
      if(nrow(z$timing)) {q<-z$timing;q$case<-ca;q$replication<-r;times<-add(times,q)}
      if(nrow(z$optimizer_attempts)) attempts<-add(attempts,z$optimizer_attempts)
      if(nrow(z$numerical_issues)) {q<-z$numerical_issues;q$case<-ca;q$replication<-r;issues<-add(issues,q)}
    }
  }
  bind<-function(x) if(length(x)) do.call(rbind,x) else data.frame()
  st<-bind(status);ev<-bind(points);ie<-bind(intervals);sel<-bind(selection)
  tables<-list(status=st,point_events=ev,interval_events=ie,selection=sel,fit_timing=bind(times),
    future_timing=bind(future),numerical_issues=bind(issues),optimizer_attempts=bind(attempts),
    B_sensitivity=bind(bprec),S_sensitivity=bind(sprec),interval_clipping=bind(clips),original_model_scores=bind(choices))
  ps<-paired<-ints<-data.frame()
  if(nrow(ev)) {
    key<-paste(ev$case,ev$replication,ev$method,ev$horizon,sep="|")
    if(anyDuplicated(key)) stop("Duplicated point events")
    count<-aggregate(horizon~case+replication+method,ev,length)
    if(any(count$horizon!=cfg$H)) stop("Incomplete horizon set in point events")
    v<-c("AE","SE","scaled_AE");pool<-ms_pool(ev,v,c("case","replication","method"))
    nm<-c("case","replication","method","horizon",v)
    ps<-ms_summary(rbind(ev[,nm],pool[,nm]),v,c("case","method","horizon"))
    planned<-aggregate(fit_success~case+method,st,length);names(planned)[3]<-"R_planned"
    ps<-merge(ps,planned,by=c("case","method"),all.x=TRUE)
    refs<-c("ETS_standard","ETS_controlled","Akaike_controlled","MEB_original","MESE_MEB_selection")
    paired<-bind(lapply(refs,function(ref) ms_pairs(pool,ref,"AE")))
    tables$point_summary<-ps;tables$paired_MAE<-paired
    tables$paired_MSE<-bind(lapply(refs,function(ref) ms_pairs(pool,ref,"SE")))
    # Separate matched-support descriptive tables: never silently rank unequal cohorts.
    blocks<-list(primary=cfg$R,bagged_benchmark=cfg$R_bag,ablation=cfg$R_ablation)
    primary_names<-c("ETS_standard","ETS_controlled","Akaike_controlled","MEB_original","MESE_original")
    if(length(cfg$B_grid)>1L) primary_names<-c(primary_names,
      unlist(lapply(cfg$B_grid[cfg$B_grid<cfg$B],function(B) c(paste0("MEB_original_B",B),paste0("MESE_original_B",B)))))
    for(block in names(blocks)) if(blocks[[block]]>0) {
      allowed<-if(block=="primary") primary_names else if(block=="bagged_benchmark")
        c(primary_names,"BaggedETS_original") else c(primary_names,"MEB_common","MEB_per_replica",
          "MESE_MEB_selection","BLD_common_ablation",if(cfg$R_bag>=cfg$R_ablation) "BaggedETS_original")
      d<-pool[pool$replication<=blocks[[block]] & pool$method%in%allowed,]
      q<-ms_summary(d,v,c("case","method"));q$R_block_planned<-blocks[[block]]
      tables[[paste0("point_summary_",block,"_support")]]<-q
    }
  }
  if(nrow(ie)) {
    key<-paste(ie$case,ie$replication,ie$method,ie$level,ie$horizon,sep="|")
    if(anyDuplicated(key)) stop("Duplicated interval events")
    count<-aggregate(horizon~case+replication+method+level,ie,length)
    if(any(count$horizon!=cfg$H)) stop("Incomplete interval horizon set")
    v<-c("covered","width","IS");pool<-ms_pool(ie,v,c("case","replication","method","interval_construction","level"))
    nm<-c("case","replication","method","interval_construction","level","horizon",v)
    ints<-ms_summary(rbind(ie[,nm],pool[,nm]),v,c("case","method","interval_construction","level","horizon"))
    tables$interval_summary<-ints
    tables$paired_interval_score<-bind(lapply(c("ETS_standard","ETS_controlled","MEB_original","MESE_MEB_selection"),
      function(ref) ms_pairs(pool,ref,"IS",by_level=TRUE)))
  }
  if(nrow(sel)) {
    known<-sel[!is.na(sel$truth)&!is.na(sel$selected_model),]
    if(nrow(known)) {known$correct_structure<-known$truth==known$selected_model
      tables$selection_recovery<-ms_summary(known,"correct_structure",c("case","method"))}
    one<-sel[!is.na(sel$selected_model),]
    if(nrow(one)) tables$selection_frequencies<-aggregate(rep(1L,nrow(one)),one[,c("case","method","selected_model")],sum)
    agree<-list()
    for(target in c("MEB_common","MESE_MEB_selection","BLD_common_ablation")) {
      a<-sel[sel$method==target,c("case","replication","selected_model")]
      b<-sel[sel$method=="ETS_controlled",c("case","replication","selected_model")]
      d<-merge(a,b,by=c("case","replication"),suffixes=c("_target","_ref"))
      if(nrow(d)) {d$agreement<-d$selected_model_target==d$selected_model_ref
        q<-ms_summary(d,"agreement","case");q$target<-target;agree<-add(agree,q)}
    }
    tables$selection_agreement<-bind(agree)
  }
  for(nm in names(tables)) if(ncol(tables[[nm]])) write.csv(tables[[nm]],file.path(folder,paste0(nm,".csv")),row.names=FALSE)
  if(any(!st$fit_success)||any(st$numerical_anomaly,na.rm=TRUE)||any(st$intervals_planned & !is.na(st$interval_success)&!st$interval_success))
    warning("Failures/flags remain. Inspect status, magnitudes and completed counts; no automatic exclusions are justified.")
  list(status=st,point_summary=ps,paired=paired,interval_summary=ints,
    fit_timing=tables$fit_timing,future_timing=tables$future_timing,
    B_sensitivity=tables$B_sensitivity,S_sensitivity=tables$S_sensitivity,numerical_issues=tables$numerical_issues)
}
ms_project <- function(result,measured,target) {
  for(nm in c("n","m","H","bounds","scale_multipliers","retry_budgets","polish_rounds","innovation"))
    if(!identical(measured[[nm]],target[[nm]])) stop("Different numerical/statistical setting: ",nm)
  t<-result$fit_timing;if(!nrow(t)) stop("No measured fit times")
  if(!all(target$cases%in%unique(t$case))) stop("Missing scenarios in timing")
  t<-t[t$case%in%target$cases,]
  Rstage<-function(stage) {
    if(stage=="BLD_generation") max(target$R_bag,target$R_ablation) else
    if(stage=="BaggedETS_original") target$R_bag else
    if(grepl("^ablation_",stage)||stage%in%c("BLD_grid","bld_packing")) target$R_ablation else target$R
  }
  growing<-c("MEB_generation","primary_MEB_fits","MEB_original_packing","BLD_generation",
    "BaggedETS_original","ablation_MEB_grid","ablation_MEB_common_packing","ablation_MEB_per_replica_packing",
    "BLD_grid","bld_packing","fit_overhead")
  q<-aggregate(seconds~case+stage,t,mean)
  q$projected_hours<-q$seconds*ifelse(q$stage%in%growing,target$B/measured$B,1)*vapply(q$stage,Rstage,numeric(1))/3600
  f<-result$future_timing
  if(nrow(f)) {
    f<-aggregate(seconds~case+method,f,mean)
    reps<-ifelse(f$method=="BaggedETS_original",target$R_bag,
      ifelse(f$method%in%c("MEB_common","MEB_per_replica","MESE_MEB_selection","BLD_common_ablation"),target$R_ablation,target$R))
    f$projected_hours<-f$seconds*ifelse(f$method=="ETS_standard",1,target$S/measured$S)*reps/3600
  }
  list(fitting=q,future=f,serial_hours=sum(q$projected_hours)+(if(nrow(f)) sum(f$projected_hours) else 0),
    note="Rough serial projection. Missing or unobserved stages and rare retries are not predicted. Not a parallel ETA.")
}
ms_self_tests <- function() {
  d<-data.frame(case="test",replication=c(1L,1L,2L,2L),method="ETS",AE=c(1,3,5,7))
  stopifnot(isTRUE(all.equal(ms_pool(d,"AE",c("case","replication","method"))$AE,c(2,6))))
  z<-ms_interval_events(0,2,3,"test",1,"method",.95,NA_integer_,"test")
  stopifnot(abs(z$IS-42)<1e-10)
  e<-c(-2,-1,.5,1,3);a<-7;n<-length(e)
  ll<-function(x) -.5*length(x)*(log(2*pi)+1+log(mean(x^2)))
  stopifnot(abs(ll(e/a)-n*log(a)-ll(e))<1e-10)
  cat("Lightweight pooling, score and scale-conversion tests passed. No model fitted.\n")
  invisible(TRUE)
}
# Functions only: use RUN_MESE_v7.R. No automatic expensive simulation on source().
