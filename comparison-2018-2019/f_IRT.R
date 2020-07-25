# Self-defined functions (derived from functions of ltm and mirt packages)

## Plot correlation coefficient heat map and p-values
mycor_plot <- function (x, group) {
    # Convert correlation coefficient matrix to data frame
    corr <- x$cor.mat
    corr[lower.tri(corr)] <- NA
    corr <- melt(corr, na.rm=T)
    
    # Convert pvalues to significant levels
    pval <- as.data.frame(x$p.values)
    pval$V1 <- as.factor(pval$V1)
    levels(pval$V1) <- group$Item[1:19]
    pval$V2 <- as.factor(pval$V2)
    levels(pval$V2) <- group$Item[2:20]
    pval$sig <- cut(pval$pvals, breaks=c(0, 0.001, 0.01, 0.05, 0.1, 1))
    
    ggplot(corr, aes(Var1, Var2)) + 
        geom_tile(aes(fill=value), color='white') +
        scale_fill_distiller(palette='Reds', limits=c(0,1), direction=1, name='Kendall\nCorrelation') + 
        geom_point(data=pval, aes(V1, V2, color=sig)) + 
        scale_color_brewer(palette='Greens', name='p-value') +
        theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1), 
             axis.title.x=element_blank(), 
             axis.title.y=element_blank())
}


## Store coefficients in different lists for convenience
mycoef.mirt <- function (x) {
  nitems <- extract.mirt(x, 'nitems')
  betas <- coef(x)[1:nitems]
  itemnames <- names(betas)
  ncatg <- extract.mirt(x, 'K')
  
#  a <- matrix(0, ncol=nitems, nrow=ncol(betas[[1]])-ncatg[1]*2)
  a <- matrix(0, ncol=nitems, nrow=1)
  for (i in 1:nitems) {
    a[,i] <- betas[[i]][1, 1:nrow(a)]
  }
  colnames(a) <- itemnames
  
  ak <- list()
  for (i in 1:nitems) {
    ak[[i]] <- betas[[i]][1, seq(nrow(a)+1, length=ncatg[i])]
  }
  names(ak) <- itemnames
  
  d <- list()
  for (i in 1:nitems) {
    d[[i]] <- betas[[i]][1, sort(seq(ncol(betas[[i]]), length=ncatg[i], by=-1))]
  }
  names(d) <- itemnames
  
  list(a=a, ak=ak, d=d)
}


## Calculate category response function
mycrf.mirt <- function (x, zrange = c(-3.8, 3.8), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL) {
  nitems <- extract.mirt(x, 'nitems')
  ncatg <- extract.mirt(x, 'K')
  
  coefs <- if (is.null(coefs)) {
      mycoef.mirt(x)
  } else {
      coefs
  }
  
  a <- coefs$a
  ak <- coefs$ak
  d <- coefs$d
  
  p <- list()
  for (i in 1:nitems) {
    numer <- matrix(0, ncol=ncatg[i], nrow=length(z))
    for (k in 1:ncatg[i]) {
      numer[,k] <- exp(ak[[i]][k] * colSums(outer(c(a[,i]), z)) + d[[i]][k])
    }
    p[[i]] <- prop.table(numer, 1)
  }
  names(p) <- colnames(a)
  p
}


## Calculate expected score for each item, using different scoring functions (ak)
myexpected.mirt <- function (x, zrange = c(-3.8, 3.8), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL) {
  nitems <- extract.mirt(x, 'nitems')
  
  p <- mycrf.mirt(x, z=z, coefs=coefs)
  ak <- mycoef.mirt(x)$ak
  
  e <- matrix(0, ncol=nitems, nrow=nrow(p[[1]]))
  for (i in 1:nitems) {
    e[,i] <- p[[i]] %*% ak[[i]]
  }
  colnames(e) <- names(p)
  e
}


## Calculate information, based on self-defined scoring functions (ak)
myinfo.mirt <- function (x, zrange = c(-3.8, 3.8), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL) {
  nitems <- extract.mirt(x, 'nitems')
  
  T.bar <- myexpected.mirt(x, z=z, coefs=coefs)
  coefs <- if (is.null(coefs)) {
      mycoef.mirt(x)
  } else {
      coefs
  }
  ak <- coefs$ak
  a <- coefs$a
  p <- mycrf.mirt(x, z=z, coefs=coefs)
  info <- matrix(0, ncol=nitems, nrow=nrow(p[[1]]))
  for (i in 1:nitems) {
    ii <- outer(ak[[i]], T.bar[, i], "-")^2
    info[, i] <- a[, i]^2 * colSums(ii * t(p[[i]]))
  }
  colnames(info) <- names(p)
  info
}


## Calculate information of an interval and its proportion of total information
myareainfo.mirt <- function (x, zrange = c(-3.8, 3.8), z = seq(zrange[1], zrange[2], length = 201), which.items = 1:extract.mirt(x, 'nitems'), coefs=NULL, ...){
  f <- function (theta, x, which.items) {
    if (length(which.items) == 1) {
      myinfo.mirt(x=x, z=matrix(theta), coefs=coefs)[, which.items]
    } else {
      rowSums(myinfo.mirt(x=x, z=matrix(theta), coefs=coefs)[, which.items])
    }
  }
  ii <- integrate(f, lower = zrange[1L], upper = zrange[2L], x=x, which.items=which.items, ...)
  iT <- integrate(f, lower = -50, upper = 50, x=x, which.items=which.items, ...)
  ret <- data.frame(LowerBound=min(zrange), UpperBound=max(zrange),
                    Info=ii$value, TotalInfo=iT$value, Proportion=ii$value/iT$value,
                    nitems=length(which.items))
  rownames(ret) <- ''
  ret
}


##
mysummaryinfo.mirt <- function (object, which.items = 1:extract.mirt(object, 'nitems'), coefs=NULL, type=c('major','total'), each=T, ...) {
    z <- seq(-4, 4, length=10001)
    iif <- myinfo.mirt(object, z=z, coefs=coefs)
    type <- match.arg(type)
    
    if (each) {
        df <- data.frame(MaxInfo.z = sapply(data.frame(iif[,which.items]), function (x) {
            z[which.max(x)]
        }))
        if (type == 'major') {
            df$LowerMajor.info <- sapply(which.items, function (x) {
                myareainfo.mirt(object, zrange=c(-2,0), which.items = x, coefs=coefs)$Info
            })
            df$UpperMajor.info <- sapply(which.items, function (x) {
                myareainfo.mirt(object, zrange=c(0,2), which.items = x, coefs=coefs)$Info
            })
            df$Major.info <- df$LowerMajor.info + df$UpperMajor.info
        } else {
            info.df <- sapply(which.items, function (x) {
                myareainfo.mirt(object, zrange=c(-50,0), which.items = x, coefs=coefs)
            })
            df$Lower.info <- info.df['Info',]
            df$Upper.info <- unlist(info.df['TotalInfo',]) - unlist(info.df['Info',])
            df$Total.info <- info.df['TotalInfo',]
        }   
    } else {
        tif <- if (length(which.items)==1) {
            rowSums(matrix(iif[,which.items]))
        } else {
            rowSums(iif[,which.items])
        }
        df <- data.frame(MaxInfo.z = z[which.max(tif)])
        if (type == 'major') {
            df$LowerMajor.info <- myareainfo.mirt(object, zrange=c(-2,0), which.items=which.items, coefs=coefs)$Info
            df$UpperMajor.info <- myareainfo.mirt(object, zrange=c(0,2), which.items=which.items, coefs=coefs)$Info
            df$Major.info <- df$LowerMajor.info + df$UpperMajor.info
        } else {
            info.df <- myareainfo.mirt(object, zrange=c(-50,0), which.items=which.items, coefs=coefs)
            df$Lower.info <- info.df$Info
            df$Upper.info <- info.df$TotalInfo - info.df$Info
            df$Total.info <- info.df$TotalInfo
        }
    }
    df
}

#### These are the functions.
## Plot ltm-like graphs, using mirt object
myplot.mirt <- function (x, multi = 1, type = c("ICC", "IIC"), items = NULL, zrange = c(-3.8, 3.8), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL, find.avg = FALSE, labels = NULL, legend = FALSE, xlab="Ability", cx = "top", cy = NULL, ncol = 1, bty = "n", col = palette(), lty = 1, pch, lwd = 1, cex = par("cex"), ...) {
  nitems <- extract.mirt(x, 'nitems')
  z0 <- which(z==0)
  type <- match.arg(type)
  itms <- if(!is.null(items)) {
    items
  } else {
    1:nitems
  }
  
  if (type == "ICC") {
    y <- myexpected.mirt(x, zrange, coefs=coefs)/multi
    Main <- if (itms[1] == 0) {
      "Test Response Function"
    } else {
      "Item Characteristic Curves"
    }
  } else if (type == "IIC") {
    y <- myinfo.mirt(x, zrange, coefs=coefs)
    Main <- if (itms[1] == 0) {
      "Test Information Function"
    } else {
      "Item Information Curves"
    }
  } else {
    stop("Can't plot type ", type)
  }
  
  if (itms[1] == 0) {
    if (type == "ICC") {
      plot(range(z), c(0,100), type="n", xlab=xlab, ylab="Expected Total Score", main=Main)
      lines(z, rowSums(y), lwd=lwd)
      if (find.avg) {
        segments(0, rowSums(y)[z0], 0, -5, lty=2)
        segments(0, rowSums(y)[z0], -4.1, rowSums(y)[z0], lty=2)
        points(0, rowSums(y)[z0])
        text(0, rowSums(y)[z0], labels=paste0("(0,",round(rowSums(y)[z0],2),")"), pos=4)
      }
    } else {
      plot(z, rowSums(y), type='l', lwd=lwd, xlab=xlab, ylab="Information", main=Main)
    }
  } else {
    if (length(itms) == 1) Main <- paste0(Main, " - ", colnames(y)[itms])
    if (type == "ICC") {
      plot(range(z), c(0,5), type="n", xlab=xlab, ylab="Expected Score", main=Main)
    } else {
      plot(range(z), c(0, max(y)), type="n", xlab=xlab, ylab="Information", main=Main)
    }
    for (i in seq(along=itms)) {
      ii <- itms[i]
      lines(z, y[, ii], col=col[ii], lwd=lwd)
    }
    if (legend) {
      lab <- colnames(y)[itms]
      legend(cx, cy, legend = lab, lty = lty, lwd = lwd,
             pch = pch, col = col[itms], bty = bty, ncol = ncol, 
             cex = cex, ...)
    }
  }
}


## Plot ltm-like graphs using ggplot2
myggplot.mirt <- function (x, multi = 1, zrange = c(-5, 5), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL, find.avg = FALSE, labels = NULL, legend = FALSE, xlab="Ability", cx = "top", cy = NULL, ncol = 1, bty = "n", col = palette(), lty = 1, pch, lwd = 1, cex = par("cex"), ...) {
    nitems <- extract.mirt(x, 'nitems')
    z0 <- which(z==0)
    
    y1 <- myexpected.mirt(x, zrange, coefs=coefs)/multi
    y2 <- myinfo.mirt(x, zrange, coefs=coefs)
    
    irf.df <- data.frame(z, y1)
    iif.df <- data.frame(z, y2)
        
    irf.df <- melt(irf.df, 'z')
    iif.df <- melt(iif.df, 'z')
    
    trf.df <- data.frame(z, sums=rowSums(y1))
    tif.df <- data.frame(z, sums=rowSums(y2))

    p1 <- ggplot(irf.df, aes(z, value, col=variable)) + 
            geom_line(size=0.75) + 
            scale_color_manual(values=col, name='') +
            coord_cartesian(ylim=c(0,1), xlim=c(-4,4)) +
            labs(title='Item Response Curves', x='Ability', y='Expected Score')
    p2 <- ggplot(trf.df, aes(z, sums)) + 
            geom_line(size=0.75) +
            geom_point(aes(x=0, y=sums[z0])) + 
            geom_segment(aes(x=0, y=sums[z0], xend=0, yend=-5), size=0.25, linetype=3) +
            geom_segment(aes(x=0, y=sums[z0], xend=-5, yend=sums[z0]), size=0.25, linetype=3) + 
            geom_label_repel(data=subset(trf.df, z==0), label=paste0('(',0,',',round(trf.df$sums[z0],2),')'), hjust=-0.25, vjust=1) + 
            coord_cartesian(ylim=c(0,100), xlim=c(-4,4)) +
            labs(title='Test Response Curve', x='Ability', y='Expected Total Score')
    p3 <- ggplot(iif.df, aes(z, value, col=variable)) +
            geom_line(size=0.75) +
            scale_color_manual(values=col) + 
            scale_y_continuous(limits=c(0, NA)) +
            coord_cartesian(xlim=c(-4,4)) +
            labs(title='Item Information Curves', x='Ability', y='Information')
    p4 <- ggplot(tif.df, aes(z, sums)) +
            geom_line(size=0.75) + 
            scale_y_continuous(limits=c(0, NA)) +
            coord_cartesian(xlim=c(-4,4)) +
            labs(title='Test Information Curve', x='Ability', y='Information')
    
    ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend=T, legend='bottom', align='v')
}


## Plot a single item response curves plot, using ggplot2
myggplot.irf <- function (x, multi = 1, zrange = c(-5, 5), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL, find.avg = FALSE, labels = NULL, legend = FALSE, xlab="Ability", cx = "top", cy = NULL, ncol = 1, bty = "n", col = palette(), lty = 1, pch, lwd = 1, cex = par("cex"), ...) {
    nitems <- extract.mirt(x, 'nitems')
    
    y1 <- myexpected.mirt(x, zrange, coefs=coefs)/multi

    irf.df <- data.frame(z, y1)
    irf.df <- melt(irf.df, 'z')
    
    
    ggplot(irf.df, aes(z, value, col=variable)) + 
            geom_line(size=0.75) + 
            scale_color_manual(values=col, name='') +
            coord_cartesian(ylim=c(0,5), xlim=c(-4,4)) +
            labs(title='Item Response Curves', x='Ability', y='Expected Score')
}


## Plot a single information curves plot, using ggplot2
myggplot.info <- function (x, multi = 1, zrange = c(-5, 5), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL, find.avg = FALSE, labels = NULL, legend = FALSE, xlab="Ability", cx = "top", cy = NULL, ncol = 1, bty = "n", col = palette(), lty = 1, pch, lwd = 1, cex = par("cex"), ...) {
    nitems <- extract.mirt(x, 'nitems')

    y2 <- myinfo.mirt(x, zrange, coefs=coefs)

    iif.df <- data.frame(z, y2)
    iif.df <- melt(iif.df, 'z')
    
    ggplot(iif.df, aes(z, value, col=variable)) +
        geom_line(size=0.75) +
        scale_color_manual(values=col, name='') + 
        scale_y_continuous(limits=c(0, NA)) +
        coord_cartesian(xlim=c(-4,4)) +
        labs(title='Item Information Curves', x='Ability', y='Information')
}


## Plot stacked information curves, using ggplot2
myggplot.stackinfo <- function (x, multi = 1, zrange = c(-5, 5), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL, find.avg = FALSE, labels = NULL, legend = FALSE, xlab="Ability", cx = "top", cy = NULL, ncol = 1, bty = "n", col = palette(), lty = 1, pch, lwd = 1, cex = par("cex"), ...) {
    nitems <- extract.mirt(x, 'nitems')

    y2 <- myinfo.mirt(x, zrange, coefs=coefs)

    iif.df <- data.frame(z, y2)
    iif.df <- melt(iif.df, 'z')
    
    ggplot(iif.df, aes(z, value, fill=variable)) +
        geom_area(position='stack') + 
        scale_fill_manual(values=col, name='') + 
        scale_y_continuous(limits=c(0, NA)) +
        coord_cartesian(xlim=c(-4,4)) + 
        labs(title='Item Information Curves', x='Ability', y='Information')
}


##
mybootgof.mirt <- function (object, gpcm_mats=list(), B=99, constrain=NULL) {
    nas <- any(na.ind <- is.na(extract.mirt(fit_pre17_GPCM, 'data')))
    
    pearson.chi <- function (object) {
        R <- residuals(object, type='exp')
        res <- (R$res)^2
        if (nas) {
            sum(res, na.rm=T)
        } else {
            sum(res, na.rm=T) + sum(R$freq) - sum(R$exp)
        }
    }
    
    Tobs <- pearson.chi(object)
    df <- extract.mirt(object, 'df')

    ncatg <- extract.mirt(object, 'K')
    nitems <- extract.mirt(object, 'nitems')
    
    parnum <- c(0, cumsum(ncatg-1))
    N <- nrow(extract.mirt(object, 'data'))
    
    Var.betas <- extract.mirt(object, 'vcov')
    vec.betas <- extract.mirt(object, 'parvec')
    
    Ts <- numeric(B)
    
    # Unlike GoF.gpcm in ltm, this function is forced to run B valid samples
    pb <- txtProgressBar(min = 0, max = B, style = 3)
    i <- 1
    repeat {
        tstat <- try({
            new.betas <- mvrnorm(1, vec.betas, Var.betas)
            
            a <- new.betas[grepl('a1',names(new.betas))]
            if (length(a)==1) {
                a <- rep(a, nitems)
            } else if (length(a) != nitems) {
                stop('Something went wrong! ')
            } else {
                a <- a
            }
            
            d <- matrix(NA, ncol=max(ncatg), nrow=nitems)
            for (j in 1:length(mats)) {
                d[j, 2:ncatg[j]] <- new.betas[grepl('d',names(new.betas))][(parnum[j]+1):parnum[j+1]]
            }
            d[,1] <- 0
            
            newData <- simdata(a, d, N, itemtype='gpcm', gpcm_mats=gpcm_mats)
            if (nas) newData[nd.ind] <- NA
            fit <- suppressMessages(mirt(newData, 1, itemtype="gpcm", constrain=constrain, gpcm_mats=gpcm_mats, verbose=F))
            pearson.chi(fit)
        }, TRUE)
        Ts[i] <- if (!inherits(tstat, 'try-error')) {
            tstat
        } else {
            as.numeric(NA)
        }
        if (!inherits(tstat, 'try-error')) {
            Ts[i] <- tstat
            setTxtProgressBar(pb, i)
            i <- i+1
        }
        if (i > B+1) break
    }
    close(pb)
#     good <- !is.na(Ts)
#     if ((newB <- sum(good)) < B) {
#         warning('the fit failed in ', B-newB, ' datasets.\n')
#         B <- newB
#     }
    p.val <- (1 + sum(Ts >= Tobs)) / (B+1)
    
    list(Tobs=Tobs, df=df, B=B, p.value=p.val)
}


## Substitute fixed common items parameters
myfcip <- function (a, b, ...) {
  a_par <- mirt(a, 1, itemtype="gpcm", ...)
  b_par <- mirt(b, 1, itemtype="gpcm", pars="values", ...)
  
  sameq <- qnum2017[!grepl("N", qnum2017)]
  
  for (i in 1:length(sameq)) {
    b_par[b_par$item==sameq[i],]$value <- c(coef(a_par)[[sameq[i]]])
    b_par[b_par$item==sameq[i],]$est <- FALSE
  }
  
  # b_par$est[nrow(b_par)-1] <- TRUE
  
  b_par
}
