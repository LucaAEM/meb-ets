# EXTRACT_MESE_v7_PAPER_RESULTS.R
# Reads stored validation results and the completed FINAL audit.
# No model fitting, no bootstrap, no simulation, no package installation.
SOURCE_FOLDER <- "MESE_v7_validation"
AUDIT_ROOT <- "MESE_v7_audit"
AUDIT_FOLDER <- ""  # Optional explicit path; blank selects latest compatible completed final audit.
OUTPUT_ROOT <- "MESE_v7_paper_results"

local({
  must<-function(ok,msg)if(!isTRUE(ok))stop(msg,call.=FALSE)
  bind<-function(x){x<-Filter(function(z)!is.null(z)&&nrow(z)>0L,x);if(!length(x))return(data.frame());d<-do.call(rbind,x);rownames(d)<-NULL;d}
  key<-function(d,cols)if(!nrow(d))character() else do.call(paste,c(lapply(d[,cols,drop=FALSE],function(x)ifelse(is.na(x),"<NA>",as.character(x))),sep="\035"))
  groups<-function(d,cols)if(nrow(d))split(seq_len(nrow(d)),key(d,cols)) else list()
  read_table<-function(folder,name,required=TRUE){p<-file.path(folder,name);if(!file.exists(p)){if(required)stop("Missing ",p,call.=FALSE);return(NULL)};read.csv(p,stringsAsFactors=FALSE,check.names=FALSE)}
  truth<-function(x)toupper(trimws(as.character(x)))%in%c("TRUE","T","1")
  wilson<-function(k,n){if(!n)return(c(NA_real_,NA_real_));z<-qnorm(.975);p<-k/n;den<-1+z*z/n;mid<-(p+z*z/(2*n))/den;half<-z*sqrt(p*(1-p)/n+z*z/(4*n*n))/den;c(max(0,mid-half),min(1,mid+half))}
  must(dir.exists(SOURCE_FOLDER),"Validation source directory missing")
  source<-normalizePath(SOURCE_FOLDER,winslash="/",mustWork=TRUE)
  cfg<-readRDS(file.path(source,"configuration.rds"))
  must(identical(cfg$mode,"validation"),"Use validation outputs, not precision outputs")
  if(!nzchar(AUDIT_FOLDER)){
    dd<-list.dirs(AUDIT_ROOT,recursive=FALSE,full.names=TRUE)
    dd<-dd[grepl("^audit_final_",basename(dd))]
    dd<-dd[file.exists(file.path(dd,"audit_configuration.rds"))&file.exists(file.path(dd,"AUDIT_MESE_v7_REPORT.txt"))]
    compatible<-vapply(dd,function(d){m<-tryCatch(readRDS(file.path(d,"audit_configuration.rds")),error=function(e)NULL)
      !is.null(m)&&identical(m$audit_version,"v7_audit_separated_diagnostics_1")&&identical(m$source_configuration,cfg)},logical(1))
    dd<-dd[compatible];must(length(dd)>0L,"No compatible completed final audit. Run AUDIT_MESE_v7_PER_REPLICA_FINAL.R first.")
    audit<-dd[which.max(file.info(file.path(dd,"audit_configuration.rds"))$mtime)]
  }else audit<-AUDIT_FOLDER
  audit<-normalizePath(audit,winslash="/",mustWork=TRUE)
  meta<-readRDS(file.path(audit,"audit_configuration.rds"))
  must(identical(meta$source_configuration,cfg),"Audit/source configuration mismatch")
  checks<-read_table(audit,"integrity_checks.csv")
  must(all(truth(checks$passed)),"The final audit has incomplete/failed integrity checks")
  manifest<-read_table(audit,"input_manifest.csv")
  source_paths<-file.path(source,manifest$file)
  must(all(file.exists(source_paths)),"A source file recorded by the audit is missing")
  must(identical(as.character(manifest$md5),unname(tools::md5sum(source_paths))),"Source CSV/configuration changed after audit")
  audit_inputs<-list.files(audit,pattern="\\.csv$",full.names=TRUE)
  audit_inputs<-c(audit_inputs,file.path(audit,"audit_configuration.rds"))
  audit_hash<-tools::md5sum(audit_inputs)

  pp<-read_table(audit,"point_losses_per_replica.csv")
  paired<-read_table(audit,"point_pairwise_main.csv")
  hp<-read_table(audit,"point_pairwise_specific_horizons.csv")
  ip<-read_table(audit,"interval_IS_pairwise_main.csv")
  im<-read_table(audit,"interval_method_summary.csv")
  idiag<-read_table(audit,"method_diagnostics.csv")
  ds<-read_table(audit,"dataset_issue_index.csv")
  issues<-read_table(audit,"numerical_issues_unmodified.csv")
  checkpoints<-read_table(audit,"checkpoint_audit.csv")
  budgets<-read_table(audit,"actual_interval_budgets.csv")
  detail<-read_table(audit,"paired_losses_and_influence.csv")
  st<-read_table(source,"status.csv")
  sel<-read_table(source,"selection.csv",FALSE)
  pp$valid_result<-truth(pp$valid_result)
  must(all(c("AE","SE","case","replication","method")%in%names(pp)),"Unexpected per-replica schema")
  must(all(paired$subset=="all_available_pairs"),"Primary contrasts unexpectedly contain exclusions")
  must(all(ip$subset=="all_available_pairs"),"Primary interval contrasts unexpectedly contain exclusions")
  dir.create(OUTPUT_ROOT,recursive=TRUE,showWarnings=FALSE)
  destination<-file.path(OUTPUT_ROOT,paste0("extraction_",format(Sys.time(),"%Y%m%d_%H%M%S"),"_",Sys.getpid()))
  must(!dir.exists(destination)&&dir.create(destination,recursive=TRUE),"Cannot create fresh extraction folder")
  destination<-normalizePath(destination,winslash="/",mustWork=TRUE)
  tables<-list()
  add<-function(name,d){if(!is.null(d)&&is.data.frame(d)&&ncol(d)){tables[[name]]<<-d;write.csv(d,file.path(destination,paste0(name,".csv")),row.names=FALSE,na="NA")}}
  labels<-data.frame(method=c("ETS_standard","ETS_controlled","Akaike_controlled","MESE_original","MESE_MEB_selection",
    "MEB_original","MEB_common","MEB_per_replica","BaggedETS_original","BLD_common_ablation"),
    description=c("Native automatic ETS benchmark","Controlled ETS; observed-series IC selection","IC-weighted controlled ETS forecast combination",
      "Parameter aggregation; observed-series IC selection","Parameter aggregation; common MEB-averaged IC selection",
      "Forecast aggregation; observed-series IC selection","Forecast aggregation; common MEB-averaged IC selection",
      "Forecast aggregation; per-replica IC selection","Native Bagged ETS point benchmark","Controlled BLD forecast aggregation; common averaged IC"),
    nominal_intervals=c("Native ETS construction","Residual plug-in","Residual mixture","Residual plug-in","Residual plug-in",
      "Residual mixture","Residual mixture","Residual mixture","Not evaluated as nominal prediction intervals","Residual mixture"),stringsAsFactors=FALSE)
  labels$IC_used_for_controlled_methods<-toupper(cfg$ic)
  add("T00_method_dictionary",labels)
  base<-c("ETS_standard","ETS_controlled","Akaike_controlled","MEB_original","MESE_original")
  panels<-list(primary=list(methods=base,R=cfg$R),bagged=list(methods=c(base,"BaggedETS_original"),R=cfg$R_bag),
    ablation=list(methods=c(base,if(cfg$R_bag>=cfg$R_ablation)"BaggedETS_original","MEB_common","MEB_per_replica","MESE_MEB_selection","BLD_common_ablation"),R=cfg$R_ablation))
  panel_tables<-list()
  for(pn in names(panels)){
    spec<-panels[[pn]];if(spec$R<1L)next
    rows<-list()
    for(ca in cfg$cases){
      d<-pp[pp$case==ca&pp$replication<=spec$R&pp$method%in%spec$methods&pp$valid_result,,drop=FALSE]
      rr<-seq_len(spec$R);complete<-rr[vapply(rr,function(r)setequal(d$method[d$replication==r],spec$methods),logical(1))]
      for(nm in spec$methods){x<-d[d$method==nm&d$replication%in%complete,,drop=FALSE];n<-nrow(x)
        mse<-if(n)mean(x$SE) else NA_real_;se_mse<-if(n>1)sd(x$SE)/sqrt(n) else NA_real_
        rows[[length(rows)+1L]]<-data.frame(panel=pn,case=ca,method=nm,n_planned=spec$R,n_complete_all_methods=length(complete),
          n_excluded_from_complete_panel=spec$R-length(complete),MAE=if(n)mean(x$AE) else NA_real_,
          MCSE_MAE=if(n>1)sd(x$AE)/sqrt(n) else NA_real_,MSE=mse,MCSE_MSE=se_mse,
          pooled_RMSE=sqrt(mse),MCSE_pooled_RMSE=if(is.finite(mse)&&mse>0)se_mse/(2*sqrt(mse)) else NA_real_)
      }
    }
    panel_tables[[pn]]<-bind(rows)
  }
  add("T01_point_primary_common_support",panel_tables$primary)
  add("T02_point_bagged_common_support",panel_tables$bagged)
  add("T03_point_ablation_complete_panel",panel_tables$ablation)
  add("T04_all_point_contrasts",paired)
  add("T05_horizon_contrasts",hp)
  add("T06_interval_quality",im)
  add("T07_all_interval_score_contrasts",ip)
  add("T08_interval_budgets",budgets)
  # Display-focused subsets retain every DGP; full contrasts are preserved above.
  refs<-c("ETS_standard","ETS_controlled","Akaike_controlled","MEB_original","BaggedETS_original")
  add("T04a_MAE_focal_main_references",paired[paired$metric=="AE"&paired$reference%in%refs,,drop=FALSE])
  add("T04b_MSE_focal_main_references",paired[paired$metric=="SE"&paired$reference%in%refs,,drop=FALSE])
  add("T04c_direct_parameter_vs_forecast",paired[paired$reference%in%c("MEB_original","MEB_common","MEB_per_replica"),,drop=FALSE])
  # Selection recovery: compare exact structures and the E/T/S family without damping.
  recovery<-agreement<-list()
  if(!is.null(sel)&&nrow(sel)){
    must(all(c("case","replication","method","selected_model","truth")%in%names(sel)),"Unexpected selection schema")
    must(!anyDuplicated(key(sel,c("case","replication","method"))),"Duplicate selection keys")
    recovery_blocks<-list(all_scheduled=cfg$R,first_bagged_block=cfg$R_bag,first_ablation_block=cfg$R_ablation)
    for(block in names(recovery_blocks)){
      limit<-recovery_blocks[[block]];if(limit<1)next
      for(ix in groups(st[st$replication<=limit,,drop=FALSE],c("case","method"))){
        ss<-st[st$replication<=limit,,drop=FALSE][ix,,drop=FALSE]
        if(!ss$case[1]%in%c("ETS_AAA","ETS_MAM"))next
        x<-sel[sel$case==ss$case[1]&sel$method==ss$method[1]&sel$replication<=limit,,drop=FALSE]
        x<-x[!is.na(x$selected_model)&!is.na(x$truth),,drop=FALSE];n<-nrow(x)
        if(!n)next
        exact<-x$selected_model==x$truth
        family<-gsub("d","",x$selected_model,fixed=TRUE)==gsub("d","",x$truth,fixed=TRUE)
        ce<-wilson(sum(exact),n);cf<-wilson(sum(family),n)
        recovery[[length(recovery)+1L]]<-data.frame(block=block,case=ss$case[1],method=ss$method[1],
          n_scheduled=nrow(ss),n_with_known_structure=n,n_without_known_structure=nrow(ss)-n,
          exact_recovery=mean(exact),MCSE_exact=if(n>1)sd(as.numeric(exact))/sqrt(n) else NA_real_,exact_Wilson_lower=ce[1],exact_Wilson_upper=ce[2],
          ETS_family_recovery=mean(family),family_Wilson_lower=cf[1],family_Wilson_upper=cf[2])
      }
    }
    for(ca in cfg$cases)for(target in c("MEB_common","MESE_MEB_selection","MEB_per_replica","BLD_common_ablation"))for(ref in c("ETS_standard","ETS_controlled")){
      a<-sel[sel$case==ca&sel$method==target&!is.na(sel$selected_model),c("replication","selected_model"),drop=FALSE]
      b<-sel[sel$case==ca&sel$method==ref&!is.na(sel$selected_model),c("replication","selected_model"),drop=FALSE]
      d<-merge(a,b,by="replication",suffixes=c("_target","_reference"));if(!nrow(d))next
      exact<-d$selected_model_target==d$selected_model_reference
      family<-gsub("d","",d$selected_model_target,fixed=TRUE)==gsub("d","",d$selected_model_reference,fixed=TRUE)
      agreement[[length(agreement)+1L]]<-data.frame(case=ca,target=target,reference=ref,n_pairs=nrow(d),agreement_exact=mean(exact),agreement_ETS_family=mean(family))
    }
    add("T09_selection_recovery_matched_blocks",bind(recovery))
    add("T10_selection_agreement",bind(agreement))
    freq<-aggregate(rep(1L,nrow(sel[!is.na(sel$selected_model),,drop=FALSE])),sel[!is.na(sel$selected_model),c("case","method","selected_model"),drop=FALSE],sum)
    names(freq)[ncol(freq)]<-"count";add("T11_selection_frequencies",freq)
  }else{
    add("T09_selection_recovery_exported",read_table(source,"selection_recovery.csv",FALSE))
    add("T10_selection_agreement_exported",read_table(source,"selection_agreement.csv",FALSE))
    add("T11_selection_frequencies_exported",read_table(source,"selection_frequencies.csv",FALSE))
  }
  add("T12_numerical_method_diagnostics",idiag)
  add("T13_dataset_issue_origins",read_table(audit,"dataset_issue_origin_counts.csv"))
  add("T14_likelihood_issue_records",issues)
  if(nrow(issues)){
    issue_summary<-bind(lapply(groups(issues,c("case","issue")),function(ix){x<-issues[ix,,drop=FALSE];v<-x$gap[is.finite(x$gap)]
      data.frame(case=x$case[1],issue=x$issue[1],n_records=nrow(x),n_datasets=length(unique(x$replication)),
        median_gap=if(length(v))median(v) else NA_real_,maximum_gap=if(length(v))max(v) else NA_real_)}))
    add("T14a_likelihood_issue_summary",issue_summary)
  }
  native<-checkpoints[,c("case","standard_code"),drop=FALSE]
  native$code_label<-ifelse(is.na(native$standard_code),"unknown",as.character(native$standard_code))
  nf<-aggregate(rep(1L,nrow(native)),native[,c("case","code_label"),drop=FALSE],sum);names(nf)[3]<-"datasets"
  add("T15_native_ETS_code_frequencies",nf)
  add("T16_multiway_common_support",read_table(audit,"multiway_wins_same_support.csv",FALSE))
  add("T17_numerical_sensitivity",read_table(audit,"pairwise_descriptive_sensitivity.csv"))
  influence<-bind(lapply(groups(detail,c("case","focal","reference","metric","level")),function(ix){
    x<-detail[ix,,drop=FALSE];v<-x$difference;n<-length(v);ord<-order(abs(v),decreasing=TRUE);den<-sum(abs(v));loo<-if(n>1)(sum(v)-v)/(n-1) else NA_real_
    data.frame(case=x$case[1],focal=x$focal[1],reference=x$reference[1],metric=x$metric[1],level=x$level[1],n_pairs=n,
      mean_difference=mean(v),median_difference=median(v),largest_absolute_difference=max(abs(v)),
      largest3_share_absolute_differences=if(den>0)sum(abs(v[ord[seq_len(min(3L,n))]]))/den else NA_real_,
      leave_one_out_mean_min=if(n>1)min(loo) else NA_real_,leave_one_out_mean_max=if(n>1)max(loo) else NA_real_,
      leave_one_out_sign_changes=if(n>1&&mean(v)!=0)any(loo*mean(v)<0) else FALSE)
  }))
  add("T18_loss_influence",influence)
  add("T18a_largest_loss_differences",read_table(audit,"largest_three_loss_differences.csv",FALSE))
  add("T19_bootstrap_parameter_diagnostics",read_table(audit,"bootstrap_parameter_diagnostics.csv",FALSE))
  ft<-read_table(source,"fit_timing.csv");future<-read_table(source,"future_timing.csv")
  timing<-aggregate(seconds~stage,ft,sum);timing$summed_elapsed_hours<-timing$seconds/3600;timing$pct_recorded_fit_elapsed<-100*timing$seconds/sum(timing$seconds)
  add("T20_fitting_time_components",timing[order(timing$seconds,decreasing=TRUE),,drop=FALSE])
  future_time<-aggregate(seconds~method,future,sum);future_time$summed_elapsed_hours<-future_time$seconds/3600
  add("T21_interval_time_components",future_time)
  execution_path<-file.path(source,"execution.rds")
  if(file.exists(execution_path)){
    execution<-readRDS(execution_path)
    add("T22_wall_time",data.frame(wall_seconds=execution$wall_seconds,wall_hours=execution$wall_seconds/3600,workers=execution$workers))
  }
  add("T23_interval_domain_counter",read_table(source,"interval_clipping.csv",FALSE))
  add("T24_integrity_checks",checks)
  requirements<-data.frame(review=c("Interval construction","B versus S","Bagged intervals","Parameter versus forecast aggregation","Per-replica selection",
      "BLD common selection","IC weights","Model recovery and agreement","Numerical diagnostics","Runtime","Complete reporting"),
    evidence=c("T06,T07,T08,T00; specify residual plug-in versus mixture","Configuration and T08; actual S must not be replaced by approval note",
      "T00: native Bagged ETS nominal intervals not evaluated","T04c plus common-support tables; keep selectors distinct","T03,T04,T16",
      "T03,T04,T16; controlled ablation is not native Bagged ETS","T01,T02,T04; criterion is the configured IC","T09,T10,T11; distinguish damping from E/T/S family",
      "T12-T15,T17,T18; codes are not automatic exclusions","T20-T22; shared components are not standalone method runtimes",
      "All DGPs, contrasts and unfavorable outcomes retained"),stringsAsFactors=FALSE)
  add("T25_review_evidence_map",requirements)
  notes<-c(
    "MESE v7 -- PAPER EXTRACTION OF EXISTING RESULTS",
    "This is an extraction, not a fitting correction, new validation or publication-readiness certificate.",
    "Native benchmarks are unchanged. No series, member or result is removed because its loss is unfavorable or its native code is nonzero.",
    "Observed-series selection and common MEB-averaged selection define different MESE variants; preserve both labels.",
    "Configured criterion is reported exactly. An AICc computation must not be described as plain AIC.",
    "B controls bootstrap fitting. S controls future simulation. Native ETS intervals use their separate construction.",
    "Actual source S is preserved. The approval note may describe a different intended budget; do not silently correct the historical run.",
    "T01-T03 use a complete cohort shared by every method in each panel. T03 is conditional on full panel availability and does NOT replace available-pair primary contrasts.",
    "Primary pairwise results in T04/T07 use all valid available pairs on the joint schedule. Pair counts are printed explicitly.",
    "Pooled MAE averages losses across horizons within each replication. Pooled RMSE is sqrt(mean MSE), not mean replication RMSE.",
    "Win/tie/loss definitions and raw-loss tests are inherited unchanged from the final audit. Tolerance-defined sign and raw Wilcoxon tests need not agree.",
    "Confidence intervals are pointwise Monte Carlo intervals. Holm columns are exploratory sensitivities, not a retrospectively prespecified testing plan.",
    "T17 subsets are descriptive sensitivity analyses, not exclusion rules. A dataset-wide signal may concern a method outside a given contrast.",
    "Native code 1 must not be counted as a MESE win or treated automatically as a missing forecast.",
    "Exact structure recovery distinguishes damping. ETS-family recovery ignores damping only; it does not ignore error/trend/seasonal types.",
    "Recovery is conditional on an available known selected structure; missing results and scheduling denominators are reported.",
    "Coverage is marginal, averaged over horizons; not simultaneous path coverage. Zero empirical MCSE does not prove exact population coverage.",
    "T18 leave-one-out diagnostics measure influence, not corrected confidence intervals. Do not delete influential replications to improve results.",
    "T20/T21 sum elapsed time recorded inside workers; they are not CPU time or parallel wall time. Fitting work is shared across methods.",
    "T23 reproduces the engine's clipped counter; it counts flagged innovations in the source and does not by itself verify backend clipping behavior.",
    "No pooled mean of unscaled losses across different DGPs is constructed.",
    "The assistant has not executed this script in R. Input/hash/integrity checks run locally before extraction."
  )
  writeLines(notes,file.path(destination,"READ_ME.txt"))
  writeLines(capture.output(dput(cfg)),file.path(destination,"executed_configuration.txt"))
  must(identical(as.character(manifest$md5),unname(tools::md5sum(source_paths))),"Source files changed during extraction")
  must(identical(unname(audit_hash),unname(tools::md5sum(audit_inputs))),"Audit inputs changed during extraction")
  export_manifest<-data.frame(file=c(paste0("source/",manifest$file),paste0("audit/",basename(audit_inputs))),
    md5=c(as.character(manifest$md5),unname(audit_hash)))
  write.csv(export_manifest,file.path(destination,"input_manifest.csv"),row.names=FALSE)
  saveRDS(list(source=source,audit=audit,configuration=cfg,audit_configuration=meta,created=Sys.time(),session=sessionInfo()),file.path(destination,"extraction_metadata.rds"))
  report<-file.path(destination,"SIMULATION_PAPER_RESULTS.txt")
  con<-file(report,open="wt",encoding="UTF-8")
  tryCatch({
    writeLines(c(notes,"",capture.output(dput(cfg)),""),con)
    # Large replica/domain tables stay as CSV; all summary scenarios and contrasts remain in the text.
    omit<-c("T23_interval_domain_counter","T19_bootstrap_parameter_diagnostics")
    for(n in setdiff(names(tables),omit))writeLines(c(paste0("===== BEGIN ",n,".csv ====="),
      readLines(file.path(destination,paste0(n,".csv")),warn=FALSE),paste0("===== END ",n,".csv ====="),""),con)
  },finally=close(con))
  cat("\n--- Extraction complete: no new model fitted ---\n")
  cat("Source:",source,"\nAudit:",audit,"\nOutput:",destination,"\n")
  cat("Tables:",length(tables),"\nText report MiB:",round(file.info(report)$size/1024^2,2),"\n")
  cat("Actual B:",cfg$B,"; actual S:",cfg$S,"; main R:",cfg$R,"; Bagged R:",cfg$R_bag,"; ablation R:",cfg$R_ablation,"\n")
  cat("Share SIMULATION_PAPER_RESULTS.txt. Tables contain favorable and unfavorable results.\n")
})
