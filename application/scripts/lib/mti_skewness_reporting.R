msx_summary <- function(rows) {
  d<-data.table::as.data.table(rows)
  stopifnot(!anyDuplicated(d[,.(study_id,task_key,method)]))
  result<-d[,{
    w<-width[returned & is.finite(width)]
    list(samples=.N,returned=sum(returned),attained=sum(attained),
      attainment=mean(attained),mcse=sqrt(mean(attained)*(1-mean(attained))/.N),
      mean_width=if(length(w))mean(w) else NA_real_,
      width_q025=if(length(w))unname(quantile(w,.025,type=7)) else NA_real_,
      width_q975=if(length(w))unname(quantile(w,.975,type=7)) else NA_real_,width_samples=length(w))
  },by=.(law_id,n,content,tolerance_confidence,method)]
  result[,lower_95:=0];result[,upper_95:=1]
  result[attained>0,lower_95:=qbeta(.025,attained,samples-attained+1)]
  result[attained<samples,upper_95:=qbeta(.975,attained+1,samples-attained)]
  result[,family_size:=.N,by=method]
  result[,simultaneous_lower:=0];result[,simultaneous_upper:=1]
  result[attained>0,simultaneous_lower:=qbeta(.05/(2*family_size),attained,samples-attained+1)]
  result[attained<samples,simultaneous_upper:=qbeta(1-.05/(2*family_size),attained+1,samples-attained)]
  result[order(n,content,law_id,method)]
}

msx_historical_rows <- function(cfg,context) {
  paths<-names(context$historical$inputs)
  paths<-paths[grepl("/validation/(interval_results|comparator_results).csv$",paths)]
  mwd_require(length(paths)==2L,"Two historical simulation inputs are required.")
  for(p in paths)mwd_require(mwd_file_hash(p)==context$historical$inputs[[p]],"Historical simulation input changed.")
  d<-data.table::rbindlist(lapply(paths,data.table::fread),fill=TRUE)[method %in% cfg$methods]
  d[,`:=`(study_id="mti_fixed_prior_20261114_validation",law_id=response_id,
    task_key=paste("mti_fixed_prior_20261114_validation",job_id,sep=":"))]
  mwd_require(nrow(d)==216000L && !anyDuplicated(d[,.(task_key,method)]),"Historical paired panel is incomplete.")
  d
}

msx_paired <- function(rows) {
  d<-data.table::as.data.table(rows)
  data.table::rbindlist(lapply(c("young_mathew","wilks"),function(comparator){
    a<-d[method=="ecm"];b<-d[method==comparator]
    b<-b[match(a$task_key,b$task_key)]
    stopifnot(identical(a$task_key,b$task_key))
    a[,`:=`(comparator=comparator,attainment_difference=as.numeric(attained)-as.numeric(b$attained),
      width_difference=ifelse(returned & b$returned,width-b$width,NA_real_))]
    a[,.(samples=.N,attainment_difference=mean(attainment_difference),
      attainment_difference_mcse=sd(attainment_difference)/sqrt(.N),
      width_paired_samples=sum(is.finite(width_difference)),
      mean_width_difference=mean(width_difference,na.rm=TRUE),
      width_difference_mcse=sd(width_difference,na.rm=TRUE)/sqrt(sum(is.finite(width_difference)))),
      by=.(law_id,n,content,tolerance_confidence,comparator)]
  }))
}

msx_population_references <- function(context,ids,contents) {
  data.table::rbindlist(lapply(ids,function(id){
    law<-context$laws[[id]]
    data.table::rbindlist(lapply(contents,function(content){
      width<-function(a)law$q(a+content)-law$q(a)
      optimum<-optimize(width,c(0,1-content),tol=1e-13)
      probabilities<-c(0,optimum$minimum)
      a<-probabilities[which.min(vapply(probabilities,width,0))]
      lower<-law$q(a);upper<-law$q(a+content)
      mwd_require(is.finite(upper-lower) && abs(msx_content(law,lower,upper)-content)<1e-10,
        "Population content reference is inconsistent.")
      data.table::data.table(dgp_id=id,dgp_label=law$label,content=content,
        lower_probability=a,upper_probability=a+content,lower=lower,upper=upper,width=upper-lower,
        mean_tilt=integrate(law$q,a,a+content,rel.tol=1e-9,subdivisions=1000L)$value/content,
        status="population_shortest_content_reference")
    }))
  }))
}

msx_review_pdf <- function(summary,labels,path,oracle=NULL) {
  methods<-c("ecm","young_mathew","wilks")
  colors<-c(ecm="#009E73",young_mathew="#CC79A7",wilks="black")
  offsets<-c(ecm=.22,young_mathew=0,wilks=-.22)
  designs<-unique(summary[,.(n,content,tolerance_confidence)])[order(n,content)]
  order<-c("normal","student_t3","asym_laplace_tau010","two_piece_normal_1_12","beta18","beta_shape025_8",
    "exponential","gamma05","gamma_shape025","weibull_shape050","lognormal_hard","lognormal_sd150",
    "invgauss_shape025","lomax_shape350")
  laws<-rev(intersect(order,unique(summary$law_id)))
  stopifnot(setequal(laws,unique(summary$law_id)),all(laws %in% names(labels)))
  grDevices::pdf(path,width=13,height=9,onefile=TRUE)
  on.exit(grDevices::dev.off())
  for(i in seq_len(nrow(designs))){
    design<-designs[i];d<-summary[n==design$n & content==design$content]
    graphics::par(mfrow=c(1,2),mar=c(4.5,12.5,4,1),oma=c(2,0,2,0),las=1,cex=.85)
    for(metric in c("attainment","width")){
      xlim<-if(metric=="attainment")c(.85,1.003) else range(c(d$width_q025,d$width_q975),finite=TRUE)
      if(metric=="width" && !is.null(oracle))xlim<-range(c(xlim,oracle$width[oracle$content==design$content]),finite=TRUE)
      if(metric=="width")xlim<-c(max(.Machine$double.eps,xlim[1]*.9),xlim[2]*1.1)
      graphics::plot(NA,xlim=xlim,ylim=c(.5,length(laws)+.7),yaxt="n",log=if(metric=="width")"x" else "",
        xlab=if(metric=="attainment")"Content-attainment probability" else "Interval width (log scale)",ylab="",
        main=if(metric=="attainment")"Attainment and 95% Monte Carlo intervals" else "95% empirical width ranges and means")
      graphics::axis(2,at=seq_along(laws),labels=unname(labels[laws]),tick=FALSE,cex.axis=.86)
      graphics::abline(h=seq(.5,length(laws)+.5,1),col="grey93")
      if(metric=="attainment")graphics::abline(v=.95,lty=2,col="grey45")
      if(metric=="width" && !is.null(oracle)){
        reference<-oracle[oracle$content==design$content,]
        position<-match(reference$dgp_id,laws)
        graphics::segments(reference$width,position-.35,reference$width,position+.35,col="#D55E00",lty=2,lwd=1.5)
      }
      for(method_name in methods){
        z<-d[method==method_name];pos<-match(z$law_id,laws)+offsets[[method_name]]
        lo<-if(metric=="attainment")z$lower_95 else z$width_q025
        hi<-if(metric=="attainment")z$upper_95 else z$width_q975
        mid<-if(metric=="attainment")z$attainment else z$mean_width
        graphics::segments(pmax(lo,xlim[1]),pos,pmin(hi,xlim[2]),pos,col=colors[[method_name]],lwd=2)
        graphics::points(pmax(xlim[1],pmin(mid,xlim[2])),pos,col=colors[[method_name]],pch=if(metric=="width")4 else 16,cex=.65)
        clipped<-which(lo<xlim[1])
        if(length(clipped))graphics::points(rep(xlim[1],length(clipped)),pos[clipped],pch=17,col=colors[[method_name]],cex=.65)
      }
      graphics::legend("topright",c("MTI-ECM","Young--Mathew","Wilks"),col=colors,lty=1,lwd=2,bty="n",cex=.75)
      if(metric=="width" && !is.null(oracle))graphics::legend("bottomright","Population shortest content interval",
        col="#D55E00",lty=2,bty="n",cex=.7)
    }
    graphics::mtext(sprintf("n = %d; required content = %.2f; tolerance confidence = %.2f",design$n,design$content,design$tolerance_confidence),
      outer=TRUE,side=3,line=.3,cex=1.1)
    graphics::mtext("Attainment intervals describe Monte Carlo uncertainty; triangles indicate clipping below 0.85. Width crosses mark means.",
      outer=TRUE,side=1,line=.3,cex=.7)
  }
}

msx_export <- function(cfg,context,directory) {
  verified<-mwd_json(file.path(directory,"verification.json"))
  input<-file.path(directory,"interval_results.csv")
  mwd_require(verified$passed && mwd_file_hash(input)==verified$interval_results_sha256,"Verified new evidence is required.")
  new<-data.table::fread(input);historical<-msx_historical_rows(cfg,context)
  columns<-c("study_id","task_key","law_id","n","content","tolerance_confidence","replication",
    "method","returned","lower","upper","width","population_content","attained")
  combined<-data.table::rbindlist(list(historical[,..columns],new[,..columns]))
  summary<-msx_summary(combined);paired<-msx_paired(combined)
  output<-file.path(directory,"report");dir.create(output,showWarnings=FALSE)
  mwd_write(combined,file.path(output,"combined_interval_results.csv"))
  mwd_write(summary,file.path(output,"combined_case_summary.csv"))
  mwd_write(paired,file.path(output,"paired_comparison.csv"))
  labels<-setNames(cfg$laws$id,cfg$laws$id)
  labels<-c(normal="Gaussian",student_t3="Student t(3)",exponential="Exponential",
    asym_laplace_tau010="Asymmetric Laplace (0.10)",two_piece_normal_1_12="Two-piece normal (1:12)",
    beta18="Beta(1,8)",gamma05="Gamma(0.5,1)",lognormal_hard="Lognormal (log SD 1.25)",
    setNames(vapply(context$laws,`[[`,"","label"),names(context$laws)))
  oracle<-data.table::rbindlist(list(data.table::fread("figures/data/tolerance_population_shortest_content_oracle.csv"),
    msx_population_references(context,unique(new$law_id),sort(unique(new$content)))),fill=TRUE)
  mwd_write(oracle,file.path(output,"population_content_references.csv"))
  msx_review_pdf(summary,labels,file.path(output,"skewness_extension_review.pdf"),oracle)
  text<-c("# Additional skewed distributions: verified ECM comparison","",
    sprintf("The extension adds %d distributions and %s independent samples to the existing eight-law comparison.",
      data.table::uniqueN(new$law_id),format(data.table::uniqueN(new$task_key),big.mark=",")),
    "The reference method, prior, DP settings and calibration were unchanged. The added-law comparison includes ECM, Young--Mathew and Wilks; it contains no new Gibbs results.",
    "",sprintf("Across the added cases, ECM attainment ranges from %.3f to %.3f.",
      min(new[method=="ecm",mean(attained),by=.(law_id,n,content)]$V1),
      max(new[method=="ecm",mean(attained),by=.(law_id,n,content)]$V1)),
    "Widths are summarized separately by distribution and design. Empirical 95% width ranges describe variation between replications; attainment intervals describe Monte Carlo uncertainty.",
    "", "The complete case tables and paired comparisons must be reviewed before manuscript replacement. No new distribution-free guarantee follows from this experiment.")
  writeLines(text,file.path(output,"scientific_review.md"))
  # Publication-sized granular tables are staged for review, not copied into the article.
  heading<-"Distribution & Method & Attainment (95\\% CI) & 95\\% width range \\\\"
  table<-c("\\begin{longtable}{llrr}","\\toprule",heading,"\\midrule","\\endfirsthead",
    "\\toprule",heading,"\\midrule","\\endhead")
  for(design_index in seq_len(nrow(cfg$design))){
    design<-cfg$design[design_index,];d<-summary[n==design$n & content==design$content]
    table<-c(table,sprintf("\\multicolumn{4}{l}{\\textbf{n=%d, c=%.2f, p=%.2f}} \\\\*",design$n,design$content,design$tolerance_confidence))
    for(id in unique(d$law_id))for(method_name in cfg$methods){
      z<-d[law_id==id & method==method_name]
      table<-c(table,paste0(sprintf("%s & %s & %.3f [%.3f, %.3f] & [%.3f, %.3f]",
        if(method_name==cfg$methods[1])labels[[id]] else "",
        c(ecm="MTI-ECM",young_mathew="Young--Mathew",wilks="Wilks")[[method_name]],
        z$attainment,z$lower_95,z$upper_95,z$width_q025,z$width_q975),
        if(method_name!=tail(cfg$methods,1))" \\\\*" else " \\\\"))
    }
    table<-c(table,"\\midrule")
  }
  writeLines(c(table,"\\bottomrule","\\end{longtable}"),file.path(output,"case_tables.tex"))
  writeLines(c("\\documentclass[10pt]{article}","\\usepackage[margin=0.65in]{geometry}",
    "\\usepackage{booktabs,longtable}","\\begin{document}",
    "\\section*{Casewise tolerance comparison}",
    "Attainment is the proportion of returned intervals with true population content at least the requested content, counting nonreturns as unsuccessful. Brackets in the attainment column give pointwise 95\\% Monte Carlo confidence intervals. Width ranges are empirical 2.5th--97.5th percentiles within each distribution and design.",
    "\\small\\input{case_tables.tex}","\\end{document}"),file.path(output,"review_tables.tex"))
  old_directory<-getwd();setwd(output)
  status<-tryCatch({
    first<-system2("pdflatex",c("-interaction=nonstopmode","-halt-on-error","review_tables.tex"),
      stdout="review_tables_build.log",stderr="review_tables_build.log")
    if(first==0L)system2("pdflatex",c("-interaction=nonstopmode","-halt-on-error","review_tables.tex"),
      stdout="review_tables_build.log",stderr="review_tables_build.log") else first
  },finally=setwd(old_directory))
  mwd_require(status==0L,"The granular review-table PDF did not compile.")
  files<-setdiff(list.files(output,full.names=TRUE),file.path(output,"provenance.json"))
  mgfp_json_write(list(verified=TRUE,source_commit=mgfp_git("rev-parse","HEAD"),
    historical_panel="mti_fixed_prior_20261114_validation",added_study=cfg$study_id,
    prior_uses_analysis_data=FALSE,article_replaced=FALSE,files=mgfp_hashes(files)),file.path(output,"provenance.json"))
  output
}
