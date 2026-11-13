# COPY_MESE_EMPIRICAL_RESULTS.R
# Reads FULL output only. No launcher source(), ep_run(), fitting or simulation.
FULL_FOLDER <- file.path("MESE_EMPIRICAL_REVIEW_v2","tourism_monthly","full_offsets_0_B100_S20000")
REPORT_ROOT <- "MESE_EMPIRICAL_REPORTS"

mostra_empirica <- local({
  must<-function(x,msg)if(!isTRUE(x))stop(msg,call.=FALSE)
  bind<-function(x){x<-Filter(function(d)!is.null(d)&&nrow(d)>0,x);if(!length(x))return(data.frame());z<-do.call(rbind,x);rownames(z)<-NULL;z}
  flag<-function(x)toupper(trimws(as.character(x)))%in%c("TRUE","T","1")
  must(dir.exists(FULL_FOLDER),paste("Full directory not found:",FULL_FOLDER))
  folder<-normalizePath(FULL_FOLDER,winslash="/",mustWork=TRUE)
  must(file.exists(file.path(folder,"configuration.rds")),"Full configuration missing")
  cfg<-readRDS(file.path(folder,"configuration.rds"))
  must(identical(cfg$stage,"full")&&identical(cfg$dataset,"tourism_monthly"),"This is not the full monthly Tourism run")
  must(identical(as.integer(cfg$origin_offsets),0L)&&cfg$H==12L,"Unexpected origin/horizon design: review before using this extraction")
  files<-list.files(folder,pattern="\\.csv$",full.names=TRUE)
  input<-c(file.path(folder,"configuration.rds"),files)
  before<-tools::md5sum(input)
  raw<-setNames(lapply(files,function(p)read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)),basename(files))
  get<-function(n,required=FALSE){d<-raw[[n]];if(is.null(d)){if(required)stop("Missing ",n,". The full may be unfinished or not collected. Do not restart fitting just to extract results.",call.=FALSE);return(data.frame())};d}
  st<-get("status.csv",TRUE)
  required<-c("series_id","origin","method","fit_success","intervals_planned","interval_success","numerical_anomaly")
  must(all(required%in%names(st)),"Unexpected status schema")
  must(!anyNA(st[,c("series_id","origin","method")]),"Missing status identifiers")
  must(!anyDuplicated(st[,c("series_id","origin","method")]),"Duplicate method-origin status")
  tasks<-get("task_manifest.csv",TRUE)
  planned<-nrow(tasks)*length(cfg$methods)
  must(nrow(st)==planned&&setequal(st$method,cfg$methods),"Missing or unexpected scheduled statuses")
  expected<-merge(tasks[,c("series_id","origin"),drop=FALSE],data.frame(method=cfg$methods),by=NULL)
  k<-function(d)paste(d$series_id,d$origin,d$method,sep="\035")
  must(setequal(k(expected),k(st)),"Status schedule mismatch")
  success<-flag(st$fit_success);ip<-flag(st$intervals_planned);isuccess<-flag(st$interval_success)
  availability<-data.frame(planned_series=length(unique(tasks$series_id)),planned_origins=nrow(tasks),planned_method_origins=planned,
    recorded_status_rows=nrow(st),fits_successful=sum(success),fits_not_successful_or_unknown=sum(!success),
    interval_method_origins_planned=sum(ip),interval_method_origins_successful=sum(ip&isuccess),
    intervals_not_successful_after_fit=sum(success&ip&!isuccess),source_flagged_method_origins=sum(flag(st$numerical_anomaly)))
  per_method<-bind(lapply(cfg$methods,function(nm){x<-st[st$method==nm,,drop=FALSE]
    data.frame(method=nm,scheduled=nrow(x),fit_success=sum(flag(x$fit_success)),fit_not_successful=sum(!flag(x$fit_success)),
      intervals_planned=sum(flag(x$intervals_planned)),intervals_successful=sum(flag(x$intervals_planned)&flag(x$interval_success)),
      numerical_flags=sum(flag(x$numerical_anomaly)))}))
  common<-get("method_summary_common_support.csv")
  available<-get("method_summary_available.csv")
  point<-get("paired_series_point.csv")
  inter<-get("interval_summary_equal_series.csv")
  interpair<-get("paired_series_interval_scores.csv")
  select_columns<-function(d,cols)if(nrow(d))d[,intersect(cols,names(d)),drop=FALSE] else d
  maincols<-c("focal","reference","metric","n_series","n_series_planned","mean_focal","mean_reference","difference","SE_series",
    "lower95","upper95","relative_change_pct","wins","ties","losses","win_pct","p_mean","p_Holm_within_focal_metric")
  prim<-if(nrow(point))point[point$metric==cfg$primary_metric,,drop=FALSE] else point
  secondary<-if(nrow(point))point[point$metric!=cfg$primary_metric,,drop=FALSE] else point
  scaled<-if(nrow(interpair))interpair[interpair$metric=="scaled_IS",,drop=FALSE] else interpair
  unscaled<-if(nrow(interpair))interpair[interpair$metric=="IS",,drop=FALSE] else interpair
  fit_times<-get("fit_timing.csv");future_times<-get("future_timing.csv")
  timing<-if(nrow(fit_times))aggregate(seconds~stage,fit_times,sum) else fit_times
  if(nrow(timing)){timing$summed_worker_hours<-timing$seconds/3600;timing<-timing[order(timing$seconds,decreasing=TRUE),]}
  future<-if(nrow(future_times))aggregate(seconds~method,future_times,sum) else future_times
  execution<-if(file.exists(file.path(folder,"execution_latest.rds")))readRDS(file.path(folder,"execution_latest.rds")) else NULL
  last_run<-if(!is.null(execution))data.frame(last_invocation_wall_seconds=execution$wall_seconds,
    last_invocation_wall_hours=execution$wall_seconds/3600,workers=execution$workers,
    note="Last invocation only; may include checkpoint reuse, not cumulative wall time") else data.frame()
  ni<-get("numerical_issues.csv");bagcodes<-get("bagged_native_codes.csv")
  bagfreq<-if(nrow(bagcodes))aggregate(rep(1L,nrow(bagcodes)),list(code=ifelse(is.na(bagcodes$code),"unknown",as.character(bagcodes$code))),sum) else data.frame()
  if(nrow(bagfreq))names(bagfreq)[2]<-"ensemble_member_count"
  native<-if("native_code"%in%names(st))st[st$method=="ETS_standard",,drop=FALSE] else data.frame()
  nativefreq<-if(nrow(native))aggregate(rep(1L,nrow(native)),list(code=ifelse(is.na(native$native_code),"unknown",as.character(native$native_code))),sum) else data.frame()
  if(nrow(nativefreq))names(nativefreq)[2]<-"series_origin_count"
  important_cfg<-cfg[intersect(c("engine","dataset","design","stage","H","B","S","S_grid","origin_offsets","primary_metric",
    "methods","software","core_md5","module_md5","protocol_hash","protocol_note"),names(cfg))]
  warnings<-c("Extraction of executed FULL outputs, not the old pilot.",
    "No refitting, bootstrap generation or future simulation was run.",
    "Do not rank method means computed on unequal available supports; use common-support and paired tables.",
    "Both MESE variants are preserved. All references and unfavorable results remain in the full text report.",
    "Paired inferential unit is the series. Cross-series dependence may remain.",
    "Native nonzero codes and source flags are diagnostics, not automatic exclusions or competitor defeats.",
    "Actual executed B/S and construction labels govern interpretation, not old approval notes.",
    "Coverage bootstrap intervals are marginal-series diagnostics, not proof of calibration or simultaneous path coverage.",
    "The code has not been executed in R by the assistant; it checks inputs in the user's session.")
  if(!nrow(common))warnings<-c(warnings,"WARNING: common-support method summary is absent. Do not substitute an unequal-support ranking.")
  if(any(!success)||any(ip&!isuccess))warnings<-c(warnings,"WARNING: some fits or nominal interval results are unavailable; retain failure denominators.")
  sections<-list(
    overview=list(executed_configuration=important_cfg,availability=availability,availability_by_method=per_method,
      observed_interval_budgets=get("actual_interval_budgets.csv"),dataset_manifest_head=head(get("data_manifest.csv"),6),last_invocation=last_run),
    point=list(common_support_all_metrics=common,available_support_descriptive=available,
      primary_metric_paired=select_columns(prim,maincols)),
    secondary=list(secondary_point_contrasts=select_columns(secondary,maincols)),
    intervals=list(interval_quality_equal_series=inter,
      paired_scaled_interval_score=scaled,coverage_by_horizon=get("coverage_by_horizon.csv"),S_sensitivity_head=head(get("interval_S_sensitivity.csv"),20)),
    intervals_raw=list(paired_unscaled_interval_score=unscaled),
    selection=list(selected_structure_frequencies=get("selection_frequencies.csv"),selection_agreement=get("selection_agreement.csv")),
    diagnostics=list(numerical_issues=ni,native_ETS_code_frequencies=nativefreq,Bagged_member_code_frequencies=bagfreq,
      fitting_time_components=timing,future_time_components=future,last_invocation=last_run)
  )
  must(identical(unname(before),unname(tools::md5sum(input))),"Input files changed during extraction. Use a stable completed run.")
  dir.create(REPORT_ROOT,recursive=TRUE,showWarnings=FALSE)
  report<-file.path(REPORT_ROOT,paste0("EMPIRICAL_FULL_TO_ANALYSE_",format(Sys.time(),"%Y%m%d_%H%M%S"),".txt"))
  con<-file(report,open="wt",encoding="UTF-8")
  tryCatch({
    writeLines(c(warnings,"",capture.output(dput(important_cfg)),""),con)
    for(n in names(raw))writeLines(c(paste0("===== BEGIN ",n," ====="),
      readLines(file.path(folder,n),warn=FALSE),paste0("===== END ",n," ====="),""),con)
    writeLines(c("===== BEGIN extraction_availability =====",capture.output(print(availability,row.names=FALSE)),
      "===== END extraction_availability ====="),con)
  },finally=close(con))
  must(identical(unname(before),unname(tools::md5sum(input))),"Source files changed while writing report")
  write.csv(data.frame(file=basename(input),md5=unname(before)),paste0(report,".manifest.csv"),row.names=FALSE)
  cat("\nFull-run extraction ready. No model fitted.\n")
  cat("Report:",normalizePath(report,winslash="/",mustWork=TRUE),"\n")
  cat("Report MiB:",round(file.info(report)$size/1024^2,2),"\n")
  cat("Paste sections separately using mostra_empirica().\n")
  function(section="overview"){
    must(length(section)==1L&&section%in%names(sections),paste("Choose:",paste(names(sections),collapse=", ")))
    old<-options(width=180,max.print=1000000);on.exit(options(old),add=TRUE)
    cat("\n========== FULL EMPIRICAL RESULTS:",toupper(section),"==========\n")
    for(n in names(sections[[section]])){
      cat("\n---",n,"---\n");d<-sections[[section]][[n]]
      if(is.data.frame(d))print(d,row.names=FALSE,digits=6,max=1000000) else print(d)
    }
    invisible(NULL)
  }
})

mostra_empirica("overview")

mostra_empirica("point")

mostra_empirica("secondary")

mostra_empirica("intervals")


mostra_empirica("intervals_raw")


mostra_empirica("selection")



mostra_empirica("diagnostics")
