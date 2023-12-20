
# Aesthetic functions and presets
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
}

# Statistical functions
{
  bootstrap.clust <- function(data=NA,FUN=NA,clustervar=NA,
                              alpha=.05,tails="two-tailed",iters=200){
    # Set up cluster designations
    if(anyNA(clustervar)){ data$cluster.id <- 1:nrow(data) 
    } else { data$cluster.id <- data[[clustervar]] }
    cluster.set <- unique(data$cluster.id)
    # Generate original target variable
    point.estimate <- FUN(data)
    # Create distribution of bootstrapped samples
    estimates.bootstrapped <- replicate(iters,{
      # Generate sample of clusters to include
      clust.list <- sample(cluster.set,length(cluster.set),replace = TRUE)
      # Build dataset from cluster list
      data.clust <- do.call(rbind,lapply(1:length(clust.list), function(x) {
        data[data$cluster.id == clust.list[x],]
      }))
      # Run function on new data
      tryCatch(FUN(data.clust),finally=NA)
    },
    simplify=TRUE)
    # Outcomes measures
    if(is.matrix(estimates.bootstrapped)){
      n_estimates <- nrow(estimates.bootstrapped)
      SE <- unlist(lapply(1:n_estimates,function(x) {sd(estimates.bootstrapped[x,],na.rm = TRUE)}))
      if (tails == "two-tailed"){
        CI.lb <- unlist(lapply(1:n_estimates,function(x) {quantile(estimates.bootstrapped[x,], alpha/2,na.rm = TRUE)}))
        CI.ub <- unlist(lapply(1:n_estimates,function(x) {quantile(estimates.bootstrapped[x,], 1-alpha/2,na.rm = TRUE)}))
      } else if (tails == "one-tailed, upper"){
        CI.lb <- unlist(lapply(1:n_estimates,function(x) {NA}))
        CI.ub <- unlist(lapply(1:n_estimates,function(x) {quantile(estimates.bootstrapped[x,], 1-alpha,na.rm = TRUE)}))
      } else if (tails == "one-tailed, lower"){
        CI.lb <- unlist(lapply(1:n_estimates,function(x) {quantile(estimates.bootstrapped[x,], alpha,na.rm = TRUE)}))
        CI.ub <- unlist(lapply(1:n_estimates,function(x) {NA}))
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
    return(list("point.estimate"=point.estimate,"SE"=SE,
                "CI.lb"=CI.lb,"CI.ub"=CI.ub,"estimates.bootstrapped"=estimates.bootstrapped))
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
