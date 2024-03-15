
# Aesthetic functions and presets
{
  # GeomSplitViolin
  {
    GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                             draw_group = function(self, data, ..., draw_quantiles = NULL) {
                               data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                               grp <- data[1, "group"]
                               newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                               newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                               newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                               
                               if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                 stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                           1))
                                 quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                                 aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                 aesthetics$alpha <- rep(1, nrow(quantiles))
                                 both <- cbind(quantiles, aesthetics)
                                 quantile_grob <- GeomPath$draw_panel(both, ...)
                                 ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                               }
                               else {
                                 ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                               }
                             })
  
    geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                                draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                                show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
          position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
          params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
    }
  }
  # Dot bar plot
    stacked_snake_count_plot <- function(group_ext,group_int,data,n_bins=4,
                                         position_nudge_width = .25/2,legend.position = "bottom",
                                         aspect.ratio=.75,coord_flip=TRUE,xlab=""){
      # Organize data and positioning
      df <- data.frame(group_ext,group_int) %>%
        arrange(group_ext,group_int)
      
      snake_bins <- function(n,n_bins){
        do.call(rbind,lapply(1:ceiling(n/n_bins), function (i){
          if(IsOdd(i)){bin <- 1:n_bins
          } else {bin <- n_bins:1
          }
          y <- rep(i,n_bins)
          data.frame(bin,y)
        }))[1:n,]
      }
      
      df <- cbind(df,
                  do.call(rbind,lapply(1:length(unique(df$group_ext)),function(x) {
                    snake_bins(n=nrow(df[df$group_ext==unique(df$group_ext)[x],]),n_bins=n_bins)
                  })))
      
      df$position_nudge <- position_nudge_width*(df$bin-(n_bins+1)/2)-position_nudge_width/2
      # Output plot
      p <- ggplot() +
        geom_dotplot(data=df,aes(x=group_ext,(y=y-0.5)*n_bins,fill=group_int),
                     binaxis = "y", binwidth = n_bins,stackratio=0,
                     method = "dotdensity", position_nudge(x = df$position_nudge))+
        scale_y_continuous(expand=c(0,0))+
        theme_light() +
        theme(legend.position = "bottom",
              aspect.ratio=aspect.ratio,
              panel.border = element_blank(),
              panel.grid = element_blank(),
              axis.line.y = element_line()
        )+
        ylab("Count")+
        xlab(xlab)
      if(coord_flip){
        p <- p + coord_flip() + theme(aspect.ratio=1/aspect.ratio)
      }
      p 
    }
  
  # Color palette
  {
    palette_weezer_blue <- c("#00a2e7","#dee5cd","#010c09","#083259","#b2915f","#d7b1b7","#00374b","#124e80", "#001212")
    palette_weezer_pinkerton <- c("#d5bf98","#14140b","#70624b","#8a8d82","#304251","#465656","#945a2d","#708090")
    palette_weezer_green <- c("#bece30","#020100","#4f6238","#cac986","#981f2c","#c13f33","#461005")
    palette_weezer_maladroit <- c("#e0dcce","#575b61","#b69e44","#953d31","#e5b066","#343729","#3e3131")
    palette_weezer_make_believe <- c("#000000","#EAECEB","#C2C2C2","#A0A0A0","#313131")
    palette_weezer_red <- c("#ED1B34","#8A817C","#141311","#8B8D9C","#332E28")
    palette_weezer_raditude <- c("#EC2221","#FBFFFB","#FDF600","#CEB181","#4E1110")
    palette_weezer_everything <- c("#E8A662","#F4F5F1","#463D47","#7F3009","#35180E","F6F3CF")
    palette_weezer_white <- c("#FDFDFD","#242424","#E3E3E3","#B6B6B6","#EEEDED")
    palette_weezer_pacific_daydream <- c("#1E3555","#5C6455","#FBE4BC","#1D1F1E","#69797B","#F8E6CF","#F8E6CF")
    palette_weezer_teal <- c("#1DBBBE","#D6A8CD","#F8F8F8","#182633","#90C5DF")
    palette_weezer_black <- c("#2D2B2C","#060606","#E9E7E8","#0E0E0E")
    palette_weezer_ok_human <- c("#B2A17A","#B3B470","#B1A78F","#D1BE8F","#726D5C","#B8B6A6","#5B4F3F")
    palette_weezer_van_weezer <- c("#B2023E","#E933D3","#770265","#170032","#FDF8FF","#170032","#5329F9","#F3FED5")
    
    palette_score_charts <- c(palette_weezer_blue[1],
                              palette_weezer_red[1],
                              palette_weezer_green[1],
                              palette_weezer_teal[1],
                              
                              palette_weezer_pinkerton[1],
                              palette_weezer_van_weezer[3]
    )
  }
  
  format.round <- function(x,digits){
    format(round(x,digits),nsmall=digits)
  }
  
  
  format.text.CI <- function(point.estimate,CI.lb,CI.ub,alpha=.05,digits=1,format.percent=FALSE){
    if (format.percent==TRUE){
      point.estimate <- 100*point.estimate
      CI.lb <- 100*CI.lb
      CI.ub <- 100*CI.ub
      end.notation <- "%"
    } else {
      end.notation <- ""
    }
    
    paste0(format.round(point.estimate,digits),
           end.notation," (",100*(1-alpha),"% CI ",
           format.round(CI.lb,digits),"-",format.round(CI.ub,digits),
           end.notation,")")
  }
}

# Statistical functions
{
  bootstrap.clust <- function(data=NA,FUN=NA,keepvars=NA,clustervar=NA,
                              alpha=.05,tails="two-tailed",iters=200,
                              format.percent=FALSE,digits=1,na.rm=TRUE){
    # Drop any variables from the dataframe that are not required for speed (optional)
    # and/or with missing values
    data.internal <- data
    if (!anyNA(keepvars)){
      keepvars <- na.omit(unique(c(keepvars,clustervar)))
      data.internal <- data.internal[c(keepvars)]
      # Drop any rows with missing data
      if (na.rm){ data.internal <- na.omit(data.internal)}
    }
    # Warn that na.rm not enabled if keepvars isn't specified
    if (anyNA(keepvars) & na.rm==TRUE){
      warning("na.rm not enabled for boostrap.clust if keepvars isn't specified")
    }
    
    # Set up cluster designations
      if(anyNA(clustervar)){ data.internal$cluster.id <- 1:nrow(data.internal) 
      } else { data.internal$cluster.id <- data.internal[[clustervar]] }
      cluster.set <- unique(data.internal$cluster.id)
    # Generate original target variable
      point.estimate <- FUN(data.internal)
    # Create distribution of bootstrapped samples
      estimates.bootstrapped <- replicate(iters,{
        # Generate sample of clusters to include
          clust.list <- sample(cluster.set,length(cluster.set),replace = TRUE)
        # Build dataset from cluster list
          data.clust <- sapply(clust.list, function(x) which(data.internal[,"cluster.id"]==x))
          data.clust <- data.internal[unlist(data.clust),]
        # Run function on new data
          tryCatch(FUN(data.clust),finally=NA)
      },simplify=TRUE)
    # Generate outcomes measures
      if(is.matrix(estimates.bootstrapped)){
        n_estimates <- nrow(estimates.bootstrapped)
        SE <- unlist(lapply(1:n_estimates,function(i) {sd(estimates.bootstrapped[i,],na.rm = TRUE)}))
        if (tails == "two-tailed"){
          CI.lb <- unlist(lapply(1:n_estimates,function(i) {quantile(estimates.bootstrapped[i,], alpha/2,na.rm = TRUE)}))
          CI.ub <- unlist(lapply(1:n_estimates,function(i) {quantile(estimates.bootstrapped[i,], 1-alpha/2,na.rm = TRUE)}))
        } else if (tails == "one-tailed, upper"){
          CI.lb <- unlist(lapply(1:n_estimates,function(i) {NA}))
          CI.ub <- unlist(lapply(1:n_estimates,function(i) {quantile(estimates.bootstrapped[i,], 1-alpha,na.rm = TRUE)}))
        } else if (tails == "one-tailed, lower"){
          CI.lb <- unlist(lapply(1:n_estimates,function(i) {quantile(estimates.bootstrapped[i,], alpha,na.rm = TRUE)}))
          CI.ub <- unlist(lapply(1:n_estimates,function(i) {NA}))
        }
      } else {
        SE <- sd(estimates.bootstrapped,na.rm = TRUE)
        if (tails == "two-tailed"){
          CI.lb <- quantile(estimates.bootstrapped, alpha/2,na.rm = TRUE)
          CI.ub <- quantile(estimates.bootstrapped, 1-alpha/2,na.rm = TRUE)
        } else if (tails == "one-tailed, upper"){
          CI.lb <- NA
          CI.ub <- quantile(estimates.bootstrapped, 1-alpha,na.rm = TRUE)
        } else if (tails == "one-tailed, lower"){
          CI.lb <- quantile(estimates.bootstrapped, alpha,na.rm = TRUE)
          CI.ub <- NA
        }
      }
    
    # Outputs
      output.list <- list("point.estimate"=point.estimate,"SE"=SE,
                          "CI.lb"=CI.lb,"CI.ub"=CI.ub,
                          "estimates.bootstrapped"=estimates.bootstrapped,
                          "alpha"=alpha, "tails"=tails
                          )
      output.list[["formatted.text"]] <- 
        format.text.CI(point.estimate=point.estimate,
                       "CI.lb"=CI.lb,"CI.ub"=CI.ub,
                       alpha=alpha,digits=digits,
                       format.percent=format.percent)
    return(output.list)
  }
  
  # Probability/Risk ratio
  probability.ratio <- function(data=NA,exposure,outcome,weight=NA){
    # Generate dataset
      if (is.na(data)){
        exposure <- as.numeric(exposure)
        outcome <- as.numeric(outcome)
        data <- data.frame(exposure,outcome)
        if(anyNA(weight)){
         data$weight <- 1
        } else {
          data$weight <- weight
        }
      } else {
        data$expsoure <- as.numeric(data[[exposure]])
        data$outcome <- as.numeric(data[[exposure]])
        data$weight <- as.numeric(data[[weight]])
      }
    
      prob.exposed <- weighted.mean(data[data$exposure==1,]$outcome,data[data$exposure==1,]$weight,na.rm = TRUE)
      prob.unexposed <- weighted.mean(data[data$exposure==0,]$outcome,data[data$exposure==0,]$weight,na.rm = TRUE)
      probability.ratio <- prob.exposed/prob.unexposed

    return(probability.ratio)
  }
  
  weighted.quantile <- function(x,weights,quantile=.5,na.rm=FALSE){
    # Modified from R package Limma, Gordon Smyth<smyth@wehi.edu.au>, http://bioinf.wehi.edu.au/limma
    # Check / modify data
      if (missing(weights)) 
        weights <- rep.int(1, length(x))
      else {
        if(length(weights) != length(x)) stop("'x' and 'weights' must have the same length")
        if(any(is.na(weights))) stop("NA weights not allowed")
        if(any(weights<0)) stop("Negative weights not allowed")
      }
      if(na.rm==TRUE){
        weights <- weights[i <- !is.na(x)]
        x <- x[i]
      }
      if(all(weights==0)) {
        warning("All weights are zero")
        return(NA)
      }
    # Find quantile
      order.x <- order(x)
      x <- x[order.x]
      weights <- weights[order.x]
      p <- cumsum(weights)/sum(weights)
      n <- sum(p<quantile)
      if(p[n+1] > quantile)
        x[n+1]
      else
        (x[n+1]+x[n+2])/2
  }
  
  # Weighted median, wrapper function
  weighted.median <- function (x, weight, na.rm = FALSE){
    weighted.quantile(x,weight,quantile=.5,na.rm)
  }
}

# Paper-specific functions
{
  # Replications
  {
    repli_outcomes_default_subset <- function(){
      df <- repli_outcomes
      
      df <- df[df$repli_type %in% c("new data","secondary data"),]
      df <- df[df$repli_version_of_record %in% c(TRUE),]
      df <- df[df$repli_is_generalizability %in% c(FALSE),]
      
      select_manylabs_selected <- c("not_manylabs","ml_aggregation")
      if ("not_manylabs" %in% select_manylabs_selected){
        df[df$manylabs_type %in% select_manylabs_selected | df$is_manylabs==FALSE,]
      } else {
        df <- df[df$manylabs_type %in% select_manylabs_selected,]
      }
      df <- df[df$power_for_effect_size %in% c("50% for 100%","90% for 50%","90% for 75%","lab power analysis","not performed"),]
      
      df
    }
  }
}