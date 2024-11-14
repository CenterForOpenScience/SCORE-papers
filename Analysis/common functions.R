
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
    
    rounded.bars <- function(data,nesting.structure,chart.palette=NA,
                                   display_axis=TRUE,axis_only=FALSE,legend=FALSE,legend.color="black",
                                   flip_x = FALSE,weightvar=NA){
      # Data manipulation
      {
        data.nested <- merge(data,nesting.structure,by="cat",all.x=TRUE,all.y=FALSE)
        
        if (is.na(weightvar)){
          data.nested$weight <- 1
        } else {
          data.nested$weight<-data.nested[[weightvar]]
        }
        cats_rects <- data.nested %>%
          group_by(cat,cat2,cat3) %>%
          #summarise(count=n())%>%
          summarise(count=sum(weight))%>%
          ungroup() %>%
          arrange(match(cat, nesting.structure$cat))%>%
          mutate(proportion=count/sum(count),
                 cumproportion = cumsum(proportion))
        cats_rects$cat <- ordered(cats_rects$cat,levels=nesting.structure$cat,labels=nesting.structure$cat)
        
        if(legend==TRUE){
          cats_rects$count <- 1
          cats_rects <- cats_rects %>%
            group_by(cat2) %>%
            mutate(count2=n(),
                   proportion = 1/n()) %>%
            group_by(cat3) %>%
            mutate(count3=n()) %>%
            group_by(cat2,cat3) %>%
            mutate(count23=n())
          
          cats_rects$proportion<-cats_rects$proportion*1/length(unique(cats_rects$cat2))
          cats_rects$proportion <- ifelse(!is.na(cats_rects$cat3),
                                          cats_rects$proportion*(1/cats_rects$count3),
                                          cats_rects$proportion)
          cats_rects$proportion <- ifelse(!is.na(cats_rects$cat3),
                                          cats_rects$proportion*(1/length(unique(cats_rects$cat2))/sum(cats_rects[!is.na(cats_rects$cat3),]$proportion)),
                                          cats_rects$proportion)
          
          cats_rects$cumproportion <- cumsum(cats_rects$proportion)
        }
        
        cats_rects$xmin <- ifelse(is.na(cats_rects$cat3),
                                  cumsum(cats_rects$proportion)-cats_rects$proportion,
                                  NA)
        cats_rects$xmax <- ifelse(is.na(cats_rects$cat3),
                                  cumsum(cats_rects$proportion),
                                  NA)
        cats_rects$ymin <- ifelse(is.na(cats_rects$cat3),
                                  0,NA)
        cats_rects$ymax <- ifelse(is.na(cats_rects$cat3),
                                  1,NA)
        
        # Force last percent to be exactly equal to 1, needed due to compounding rounding error from adding floats
        cats_rects$cumproportion[nrow(cats_rects)] <- 1
        cats_rects$xmax[nrow(cats_rects)] <- 1
        
        cats_rects$yproportion <- NA
        cats_rects$ycumproportion <- NA
        cats_rects$xinternalproportion <- NA
        cats_rects$xinternalcumproportion <- NA
        for(cat2 in unique(cats_rects[!is.na(cats_rects$cat3),]$cat2)) {
          cats_rects$selected2 <- !is.na(cats_rects$cat2) & cats_rects$cat2==cat2
          xmin_total <- 
            min(cats_rects[cats_rects$selected2,]$cumproportion-cats_rects[cats_rects$selected2,]$proportion)
          
          xmax_total <- max(cats_rects[cats_rects$selected2,]$cumproportion)
          
          for(cat3 in unique(cats_rects[cats_rects$cat2==cat2,]$cat3)){
            cats_rects$selected3 <- !is.na(cats_rects$cat3) & 
              cats_rects$cat2==cat2 & 
              cats_rects$cat3==cat3
            cats_rects[cats_rects$selected3,]$yproportion <- 
              sum(cats_rects[cats_rects$selected3,]$proportion) / 
              sum(cats_rects[cats_rects$selected2,]$proportion)
            
            cats_rects[cats_rects$selected3,]$xinternalproportion <- 
              cats_rects[cats_rects$selected3,]$proportion / 
              sum(cats_rects[cats_rects$selected3,]$proportion)
            
            cats_rects[cats_rects$selected3,]$xinternalcumproportion <- 
              cumsum(cats_rects[cats_rects$selected3,]$xinternalproportion)
            
            cats_rects[cats_rects$selected3,]$xmin <-
              (cats_rects[cats_rects$selected3,]$xinternalcumproportion-cats_rects[cats_rects$selected3,]$xinternalproportion)*(xmax_total-xmin_total) +
              xmin_total
            
            cats_rects[cats_rects$selected3,]$xmax <-
              cats_rects[cats_rects$selected3,]$xinternalcumproportion*(xmax_total-cats_rects[cats_rects$selected3,]$xmin) +
              cats_rects[cats_rects$selected3,]$xmin
          }
          if(all(as.character(cats_rects$cat)==as.character(cats_rects$cat2))){
            cats_rects <- cats_rects %>%
              group_by(cat3) %>%
              mutate(ycumproportion = (row_number()==1)*yproportion) %>%
              mutate(ycumproportion = cumsum(ifelse(is.na(ycumproportion), 0, ycumproportion)) + ycumproportion*0)
          } else{
            cats_rects <- cats_rects %>%
              group_by(cat3) %>%
              mutate(ycumproportion = (row_number()==1)*yproportion) %>%
              ungroup() %>%
              mutate(ycumproportion = cumsum(ifelse(is.na(ycumproportion), 0, ycumproportion)) + ycumproportion*0)
          }
          
          cats_rects[cats_rects$selected2,]$ymin <- cats_rects[cats_rects$selected2,]$ycumproportion-cats_rects[cats_rects$selected2,]$yproportion
          cats_rects[cats_rects$selected2,]$ymax <- cats_rects[cats_rects$selected2,]$ycumproportion
        }
        
        cats_rects$xcenter <- (cats_rects$xmin+cats_rects$xmax)/2
        cats_rects$ycenter <- (cats_rects$ymin+cats_rects$ymax)/2
        
        if(flip_x == TRUE){
          cats_rects$xmax.temp <- cats_rects$xmin*-1
          cats_rects$xmin <- cats_rects$xmax*-1
          cats_rects$xmax <- cats_rects$xmax.temp
          cats_rects$xmax.temp <- NULL
          cats_rects$ymax.temp <- cats_rects$ymin*-1
          cats_rects$ymin <- cats_rects$ymax*-1
          cats_rects$ymax <- cats_rects$ymax.temp
          cats_rects$ymax.temp <- NULL
          
          cats_rects$xcenter <- cats_rects$xcenter*-1
          cats_rects$ycenter <- cats_rects$ycenter*-1
        }
        
        
      }
      
      # Chart generation
      {
        # Base chart
        {
          if (!axis_only){
            plot <- ggplot(data=cats_rects,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,fill=cat)) + 
              funkyheatmap::geom_rounded_rect(radius=unit(3, "points"),show.legend=FALSE,
                                              color="black",
                                              size=0)+
              scale_x_continuous(expand=expansion(add = c(0, .05)),
                                 labels = scales::percent_format(),
                                 breaks=c(0,.25,.5,.75,1))
          } else {
            plot <- ggplot(data=cats_rects,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,fill=cat)) + 
              funkyheatmap::geom_rounded_rect(radius=unit(3, "points"),show.legend=FALSE,
                                              color="black",
                                              size=0)
          }
          
          labels <- c("0%","25%","50%","75%","100%")
          breaks <- c(0,.25,.5,.75,1)
          limits <- c(0,1)
          
          if(flip_x == TRUE){
            labels <- rev(labels)
            breaks <- -1*rev(breaks)
            limits <- -1*rev(limits)
          }
          
          plot <- plot +
            scale_x_continuous(#expand=expansion(add = c(0, .05)),
                               labels = labels,
                               breaks=breaks,
                               limits=limits)+
            theme_light() +
            theme(legend.position = "none",
                  legend.title=element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y = element_blank(),
                  axis.line = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
            )
        }
        # Options
        {
          if(display_axis==FALSE){
            plot <- plot + theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x = element_blank())
          }
          if (!anyNA(chart.palette)){
            plot <- plot + scale_fill_manual(values=chart.palette[levels(cats_rects$cat) %in% cats_rects$cat])
          }
          if(legend==TRUE){
            # plot <- plot+
            #   geom_text(data=cats_rects,aes(x=xcenter,y=ycenter,label=cat),
            #             color=legend.color)
          }
        }
        # Output
        {
          plot
          return(list("cats_rects"=cats_rects,"plot" = plot))
        }
      }
    }
    
    snakebins <- function(data,nesting.structure,chart.palette=NA,
                                   display_axis=TRUE,axis_only=FALSE,legend.color="black",
                                   n_bins=6,n_bins_max=NA,bin_width_x=1,
                                   flip_x = FALSE,collapsevar=NA){
      # Data manipulation
      snake_bins <- function(n,n_bins){
        do.call(rbind,lapply(1:ceiling(n/n_bins), function (i){
          if((i %% 2) == 1){ymax <- 1:n_bins
          } else {ymax <- n_bins:1}
          xmax <- rep(i,n_bins)
          ymin <- ymax-1
          xmin <- xmax-1
          
          xmin <- xmin*bin_width_x
          xmax <- xmax*bin_width_x
          ymin <- ymin*1/n_bins
          ymax <- ymax*1/n_bins
          xcenter <- (xmin+xmax)/2
          ycenter <- (ymin+ymax)/2
          data.frame(xmin,xmax,ymin,ymax,xcenter,ycenter)
        }))[1:n,]
      }
      
      # If data are to be collapsed, collapse into upper category
      if (!is.na(collapsevar)){
        # Generate initial weights
        data$collapsevar <- data[[collapsevar]]
        data <- data %>%
          group_by(collapsevar) %>%
          mutate(weight=1/n())
        
        # Get reweighted 
        proportion <- data %>%
          group_by(cat) %>%
          #summarise(count=n())%>%
          summarise(weightcountrounded=round(sum(weight),0)) 
        
        # Create expanded dataset
        cats.expanded <- do.call(c,lapply(1:nrow(proportion),
                                          function(x) {
                                            rep(proportion$cat[x],proportion$weightcountrounded[x])
                                          }))
        data <- data.frame(cats.expanded)
        colnames(data) <- c("cat")
      }
      
      data.bins <- cbind(arrange(data,data$cat),snake_bins(n=nrow(data),n_bins))

      # Chart generation
      {
        if(!axis_only){
          snakebin_plot <- ggplot(data=data.bins,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,fill=cat)) + 
            funkyheatmap::geom_rounded_rect(
              radius=unit(3, "points"),show.legend=FALSE,
              color="black",
              size=0)
        } else {
          snakebin_plot <- ggplot(data=data.bins,aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,fill=cat))+
            funkyheatmap::geom_rounded_rect(
              radius=unit(3, "points"),show.legend=FALSE,
              color="black",fill="white",
              size=0)
        }
        
        labels <- c("0",n_bins_max)
        breaks <- c(0,bin_width_x*n_bins_max/n_bins)
        limits <- c(0,bin_width_x*n_bins_max/n_bins)
        
        if(flip_x == TRUE){
          labels <- rev(labels)
          breaks <- -1*rev(breaks)
          limits <- -1*rev(limits)
        }
        
        snakebin_plot <- snakebin_plot + 
          scale_x_continuous(#expand=expansion(add = c(0, .05)),
            expand=c(0,2),
            #expand=expansion(add = c(10, 10)),
                             labels = labels,
                             breaks= breaks,
                             limits = limits)+
          theme_light() +
          theme(legend.position = "none",
                legend.title=element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y = element_blank(),
                axis.line = element_blank(),
                axis.ticks.y = element_blank(),
                plot.margin = margin(t = 0, r = 0, b = 0, l = 0)
          )
        # Options
        {
          if(display_axis==FALSE){
            snakebin_plot <- snakebin_plot + theme(axis.text.x=element_blank(),axis.title.x=element_blank(),axis.ticks.x = element_blank())
          }
          if (!anyNA(chart.palette)){
            snakebin_plot <- snakebin_plot + scale_fill_manual(values=chart.palette[levels(data$cat) %in% data$cat])
          }
        }
        # Output
        {
          snakebin_plot
          return(list("plot" = snakebin_plot))
        }
      }
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
    palette_weezer_everything <- c("#E8A662","#F4F5F1","#463D47","#7F3009","#35180E","#F6F3CF")
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
                              palette_weezer_van_weezer[3],
                              palette_weezer_pinkerton[5]
    )
  }
  
  format.round <- function(x,digits){
    format(round(x,digits),nsmall=digits,trim=TRUE)
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
           end.notation," [",100*(1-alpha),"% CI ",
           format.round(CI.lb,digits)," - ",format.round(CI.ub,digits),
           end.notation,"]")
  }
  
  format.text.percent <- function(x,n,alpha=.05,digits=1,confint=TRUE){
    p <- binconf(x,n)
    text <- paste0(format.round(100*p[1],digits),"%")
    if(confint){
      text <- format.text.CI(p[1],p[2],p[3],alpha,digits,format.percent = TRUE)
    } else
      text <- paste0(format.round(100*p[1],digits=digits),"%")
    text
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
  
  # Clustered/weighted proportion (convenience function)
  cw.proportion <- function (data, weights, clusters,iters){

    df <- data.frame(data,weights,clusters)
    
    bootstrap.clust(data=df,
                    FUN=function(x) {
                      sum(as.numeric(x$data)*x$weights)/sum(x$weights)
                    },
                    clustervar = "clusters",
                    keepvars=c("clusters","data","weights"),
                    alpha=.05,tails="two-tailed",iters=iters,
                    format.percent=TRUE,digits=1
    )
  }
}
# 
# # Paper-specific functions
# {
#   # Replications
#   {
#     repli_outcomes_default_subset <- function(){
#       df <- repli_outcomes
#       
#       df <- df[df$repli_type %in% c("new data","secondary data"),]
#       df <- df[df$repli_version_of_record %in% c(TRUE),]
#       df <- df[df$repli_is_generalizability %in% c(FALSE),]
#       
#       select_manylabs_selected <- c("not_manylabs","ml_aggregation")
#       if ("not_manylabs" %in% select_manylabs_selected){
#         df[df$manylabs_type %in% select_manylabs_selected | df$is_manylabs==FALSE,]
#       } else {
#         df <- df[df$manylabs_type %in% select_manylabs_selected,]
#       }
#       df <- df[df$power_for_effect_size %in% c("50% for 100%","90% for 50%","90% for 75%","lab power analysis","not performed"),]
#       
#       df
#     }
#   }
# }