## List of self-defined functions (derived from functions of ltm package)
# mycr2catg                 -     Change responses from credit to category
# mycatg2cr                 -     Change responses from category to credit
# myplot.gpcm               -     Add expected score plots to plot.gpcm
# myplot.resid.gpcm         -     Plot of theoretical and empirical IRFs 
#                                 and compute chi-square tests of item-fit
# myGoF.gpcm                -     Add progress bar to GoF.gpcm
# myprint.xtable.aov.gpcm   -     Apply 'xtable' to objects of class "aov.gpcm"

# -----

mycr2catg <- function (x) {
  for (i in 1:nrow(x)) {
    for (j in 1:length(catg)) {
      x[i,j] <- which(catg[[j]]==x[i,j])
    }
  }
  x
}

mycatg2cr <- function (x) {
  for (i in 1:nrow(x)) {
    for (j in 1:length(catg)) {
      x[i,j] <- catg[[j]][x[i,j]]
    }
  }
  x
}

# -----

myplot.gpcm <- function (x, type = "ICC", items = NULL, find.avg = FALSE, labels = NULL, legend = FALSE, cx = "top", cy = NULL, ncol = 1, bty = "n", col = palette(), lty = 1, pch, lwd = 1, cex = par("cex"), ...) {
  betas <- x$coefficients
  nitems <- length(betas)
  ncatg <- sapply(betas, length)
  itms <-  if (!is.null(items)) {
    items
  }
  else 1:nitems
  ctg <- "all"
  type <- "ICC"
  plot.items <- type == "ICC"
  plot.info <- !plot.items
  
  zrange <- c(-3.8, 3.8)
  z <- seq(zrange[1], zrange[2], length = 101)
  z0 <- which(z==0)
  cpr <- lapply(mycrf.GPCM(betas, z, x$IRT.param), t)
  
  expectedscore <- matrix(numeric(nitems * 101), ncol=nitems)
  for (i in 1:nitems) {
    catg <- as.numeric(names(dsc$perc[[i]]))
    p <- cpr[[i]]
    expectedscore[,i] <- catg %*% t(p)
  }
  
  if (itms == 0) {
    expectedtotalscore <- rowSums(expectedscore)
    plot(range(z), c(0,100), type="n", xlab="Ability", ylab="Expected Total Score", main="Test Response Function")
    lines(z, expectedtotalscore, lwd=lwd)
    if (find.avg) {
      segments(0, expectedtotalscore[z0], 0, -5, lty=2)
      segments(0, expectedtotalscore[z0], -4.1, expectedtotalscore[z0], lty=2)
      points(0, expectedtotalscore[z0])
      text(0, expectedtotalscore[z0], labels=paste0("(0,",round(expectedtotalscore[z0],2),")"), pos=4)
    }
  }
  else {
    if (length(itms) == 1)
      main = paste0("Item Characteristic Curve - Item: Q", itms)
    else main = "Item Characteristic Curves"
    plot(range(z), c(0,5), type="n", xlab="Ability", ylab="Expected Score", main=main)
    for (i in seq(along=itms)) {
      ii <- itms[i]
      lines(z, expectedscore[,ii], col=col[ii], lwd=lwd)
    }
    if (legend) {
      lab <- paste("Q", itms, sep="")
      legend(cx, cy, legend = lab, lty = lty, lwd = lwd,
             pch = pch, col = col, bty = bty, ncol = ncol, 
             cex = cex, ...)
    }
  }
}

mylinpred.GPCM <-
  function (betas, z, IRT.param = TRUE) {
    lapply(betas, function (x) {
      nx <- length(x)
      if (IRT.param)
        t(x[nx] * outer(z, x[-nx], "-"))
      else
        outer(x[-nx], x[nx] * z , "+")
    })
  }
mycrf.GPCM <-
  function (betas, z, IRT.param = TRUE, log = FALSE, eps = .Machine$double.eps^(1/2)) {
    lapply(mylinpred.GPCM(betas, z, IRT.param), function (x) {
      num <- exp(apply(x, 2, cumsum))
      if (!is.matrix(num))
        num <- t(num)
      den <- 1 + colSums(num)
      out <- rbind(1/den, num/rep(den, each = nrow(x)))
      if (any(ind <- out == 1))
        out[ind] <- 1 - eps
      if (any(ind <- out == 0))
        out[ind] <- eps
      if (log)
        out <- log(out)
      out
    })
  }

# -----

myplot.resid.gpcm <- function (object, items = NULL, bins = NULL, plot = TRUE, ...) {
  if (plot)
    myplot.gpcm(object, items=items, ...)
  
  N <- nrow(object$X)
  bins <- if (!is.null(bins)) bins
  else N
  ninbin.lower <- floor(N/bins)
  ninbin.higher <- ninbin.lower + 1
  rest <- N - ninbin.lower * bins
  bin.size <- rep(ninbin.lower, bins)
  bin.size[sample.int(N, rest)] <- ninbin.higher
  
  bin.median <- numeric(bins)
  cr.mean.obs <- numeric(bins)
  
  for (b in 1:bins) {
    i.stop <- sum(bin.size[1:b])
    i.start <- i.stop - bin.size[b] + 1
    bin.median[b] <- median(mdt.fitted$z1[i.start:i.stop])
    if (items == 0)
      cr.mean.obs[b] <- mean(rowSums(mdt.fitted[i.start:i.stop, 1:20]))
    else
      cr.mean.obs[b] <- mean(mdt.fitted[i.start:i.stop, items])
    if (plot) 
      points(bin.median[b], cr.mean.obs[b])
  }
  
  if (items != 0) {
    cpr <- lapply(mycrf.GPCM(object$coefficients, bin.median, object$IRT.param), t)
    cr.mean.exp <- catg[[items]] %*% t(cpr[[items]])
    bock.chisq <- sum(bin.size[b] * (cr.mean.obs - cr.mean.exp)^2 / cr.mean.exp / (5 -cr.mean.exp))
    df <- bins - length(catg[[items]])
    p.val <- pchisq(bock.chisq, df, lower.tail=F)
    if (plot) {
      text(1,1, labels=paste0("df: ", df), pos=4)
      text(1,0.5, labels=paste0("p.val: ", round(p.val,4)), pos=4)
    }
    p.val
  }
}

# -----

myGoF.gpcm <- function (object, simulate.p.value = TRUE, B = 99, seed = NULL, 
                        ...) {
  if (!inherits(object, "gpcm")) 
    stop("Use only with 'gpcm' objects.\n")
  nas <- any(na.ind <- is.na(object$X))
  pearson.chi <- function(object) {
    R <- resid(object)
    res <- (R[, "Resid"])^2
    if (nas) 
      sum(res, na.rm = TRUE)
    else sum(res, na.rm = TRUE) + sum(R[, "Obs"]) - sum(R[, 
                                                          "Exp"])
  }
  Tobs <- pearson.chi(object)
  betas <- object$coefficients
  ncatg <- sapply(betas, length)
  p <- length(betas)
  df <- prod(ncatg) - attr(logLik(object), "df") - 1
  p.val <- if (!simulate.p.value) {
    pchisq(Tobs, df, lower.tail = FALSE)
  }
  else {
    constraint <- object$constraint
    IRT.param <- object$IRT.param
    vec.betas <- if (constraint == "gpcm") {
      unlist(betas, use.names = FALSE)
    }
    else if (constraint == "1PL") {
      betas[seq(1, p - 1)] <- lapply(betas[seq(1, p - 
                                                 1)], function(x) x[-length(x)])
      unlist(betas, use.names = FALSE)
    }
    else {
      betas <- lapply(betas, function(x) x[-length(x)])
      unlist(betas, use.names = FALSE)
    }
    Var.betas <- vcov(object)
    n <- nrow(object$X)
    Ts <- numeric(B)
    if (!is.null(seed)) 
      set.seed(seed)
    old <- options(warn = (-1))
    on.exit(options(old))
    pb <- txtProgressBar(min = 0, max = B, style = 3)
    for (i in 1:B) {
      tstat <- try({
        new.betas <- mvrnorm(1, vec.betas, Var.betas)
        new.betas <- mybetas.gpcm(new.betas, p, ncatg, 
                                  constraint)
        newData <- rmvordlogis(n, new.betas, IRT.param, 
                               "gpcm")
        if (nas) 
          newData[na.ind] <- NA
        fit <- gpcm(newData, constraint, start.val = new.betas, 
                    control = object$control)
        pearson.chi(fit)
      }, TRUE)
      Ts[i] <- if (!inherits(tstat, "try-error")) 
        tstat
      else as.numeric(NA)
      setTxtProgressBar(pb, i)
    }
    close(pb)
    good <- !is.na(Ts)
    if ((newB <- sum(good)) < B) {
      warning("the fit failed in ", B - newB, " datasets.\n")
      B <- newB
    }
    (1 + sum(Ts[good] >= Tobs))/(B + 1)
  }
  out <- list(Tobs = Tobs, p.value = p.val, df = df, simulate.p.value = simulate.p.value, 
              B = B, call = object$call)
  class(out) <- "GoF.gpcm"
  out
}

mybetas.gpcm <-
  function (thetas, nitems, ncatg, constraint, keep.names = FALSE) {
    betas <- if (constraint == "gpcm") {
      ii <- rep(1:nitems, ncatg)
      split(thetas, ii)
    } else if (constraint == "1PL") {
      nt <- length(thetas)
      ii <- rep(1:nitems, ncatg - 1)
      lapply(split(thetas[-nt], ii), function (x) c(x, thetas[nt]))
    } else {
      ii <- rep(1:nitems, ncatg - 1)
      lapply(split(thetas, ii), function (x) c(x, 1))       
    }
    if (!keep.names)
      names(betas) <- NULL
    betas
  }

# -----

myprint.xtable.aov.gpcm <- function (x) {
  if (!inherits(x, "aov.gpcm")) 
    stop("Use only with 'aov.gpcm' objects.\n")
  aov.df <- data.frame(AIC=c(x$aic0, x$aic1, NA), BIC=c(x$bic0, x$bic1, NA), log.Lik=c(x$L0, x$L1, x$LRT), df=c(x$nb0, x$nb1, x$df), p.value=c(NA, NA, x$p.value))
  rownames(aov.df) <- c(x$nam0, x$nam1, 'LR')
  print(xtable(aov.df, digits=c(0,0,0,0,0,3), align=c('c',rep('r',5))), hline.after=c(-1,0,2,3))
}

# -----

myplot2 <- function (x, type = c("ICC", "IIC", "OCCu", "OCCl"), items = NULL, 
          category = NULL, zrange = c(-3.8, 3.8), z = seq(zrange[1], 
                                                          zrange[2], length = 100), annot, labels = NULL, legend = FALSE, 
          cx = "top", cy = NULL, ncol = 1, bty = "n", col = palette(), 
          lty = 1, pch, xlab, ylab, main, sub = NULL, cex = par("cex"), 
          cex.lab = par("cex.lab"), cex.main = par("cex.main"), cex.sub = par("cex.sub"), 
          cex.axis = par("cex.axis"), plot = TRUE, ...) 
{
  if (!inherits(x, "gpcm")) 
    stop("Use only with 'gpcm' objects.\n")
  type <- match.arg(type)
  betas <- x$coefficients
  nitems <- length(betas)
  ncatg <- sapply(betas, length)
  itms <- if (!is.null(items)) {
    if (!is.numeric(items) || length(items) > nitems) 
      stop("'items' must be a numeric vector of length at most ", 
           nitems)
    if (type == "ICC" && any(items < 1 | items > nitems)) 
      stop("'items' must contain numbers between 1 and ", 
           nitems, " denoting the items.\n")
    if (type == "IIC" && any(items < 0 | items > nitems)) 
      stop("'items' must contain numbers between 0 and ", 
           nitems)
    items
  }
  else 1:nitems
  ctg <- if (!is.null(category)) {
    if (length(category) > 1) 
      stop("'category' must be a number indicating the category.\n")
    if (category < 0 || category > max(ncatg)) 
      stop(paste("'category' must be a number between 1 and ", 
                 max(ncatg), ".\n", sep = ""))
    if (any(ind <- category > ncatg)) {
      if (sum(ind) > 1) 
        warning("Items ", paste(items[ind], collapse = ", "), 
                " are excluded since they have only ", paste(ncatg[ind], 
                                                             collapse = ", "), " categories, respectively.\n")
      else warning("Item ", items[ind], " is excluded since they have only ", 
                   ncatg[ind], " categories.\n")
      itms <- itms[!ind]
    }
    category
  }
  else "all"
  cpr <- myinfoGPCM(betas, z, x$IRT.param)
  plot.items <- type == "ICC" || (type == "IIC" & (is.null(items) || 
                                                     all(items > 0)))
  plot.info <- !plot.items
  if (missing(main)) {
    Main <- if (type == "ICC") {
      "Item Response Category Characteristic Curves"
    }
    else if (type == "OCCl" || type == "OCCu") {
      "Item Operation Characteristic Curves"
    }
    else {
      if (plot.items) 
        "Item Information Curves"
      else "Test Information Function"
    }
    mis.ind <- TRUE
  }
  else mis.ind <- FALSE
  if (missing(ylab)) {
    ylab <- if (type == "ICC" || (type == "OCCl" | type == 
                                  "OCCu")) 
      "Probability"
    else "Information"
  }
  if (missing(xlab)) {
    xlab <- "Ability"
  }
  if (missing(annot)) {
    annot <- !legend
  }
  col. <- col
  lty. <- lty
  if (type == "ICC" || (type == "OCCl" | type == "OCCu")) {
  }
  else {
    p <- cpr[, itms, drop = FALSE]
    if (plot) {
      r <- if (plot.items) 
        range(p)
      else range(rowSums(cpr))
      if (mis.ind) {
        main <- Main
      }
      plot(range(z), r, type = "n", xlab = xlab, ylab = ylab, 
           main = main, sub = sub, cex = cex, cex.lab = cex.lab, 
           cex.main = cex.main, cex.axis = cex.axis, cex.sub = cex.sub, 
           ...)
      if (plot.items) {
        col <- rep(col., length.out = length(itms))
        lty <- rep(lty., length.out = length(itms))
        if (!missing(pch)) {
          pch <- rep(pch, length.out = length(itms))
          pch.ind <- round(seq(15, 85, length = 4))
        }
        pos <- round(seq(10, 90, length = ncol(p)))
        for (i in seq(along = itms)) {
          lines(z, p[, i], lty = lty[i], col = col[i], 
                ...)
          if (!missing(pch)) 
            points(z[pch.ind], p[pch.ind, i], pch = pch[i], 
                   col = col[i], cex = cex, ...)
          if (annot) 
            text(z[pos[i]], p[pos[i], i], adj = c(0, 
                                                  1.2), if (missing(labels)) 
                                                    i
                 else labels[i], col = col[i], cex = cex, 
                 ...)
        }
        if (legend) {
          ncol. <- if (is.null(ncol)) 
            ncol. <- if (nitems > 8) 
              2
          else 1
          else ncol
          legend(cx, cy, legend = if (missing(labels)) 
            colnames(cpr)[itms]
            else labels, lty = lty, pch = pch, col = col, 
            bty = bty, ncol = ncol., cex = cex, ...)
        }
      }
      else {
        col <- col.[1]
        lty <- lty.[1]
        p <- rowSums(cpr)
        lines(z, p, lty = lty, col = col, ...)
        if (!missing(pch)) 
          points(z, p, pch = pch, col = col, cex = cex, 
                 ...)
        if (legend) 
          legend(cx, cy, legend = "Information", lty = lty, 
                 col = col, pch = pch, bty = bty, ncol = 1, 
                 ...)
      }
      return.value <- if (plot.items) 
        cbind(z = z, item.info = p)
      else cbind(z = z, test.info = p)
    }
    else {
      return(if (plot.items) cbind(z = z, item.info = p) else cbind(z = z, 
                                                                    test.info = rowSums(cpr)))
    }
  }
  invisible(return.value)
}

myinfoGPCM <-
  function (betas, z, IRT.param) {
    n <- length(z)
    p <- length(betas)
    alphas <- sapply(betas, tail, 1)
    prs <- mycrf.GPCM(betas, z, IRT.param)
    T.bar <- matrix(0, n, p)
    for (j in 1:p) {
      T.bar[, j] <- colSums(prs[[j]] * catg[[j]])
    }
    info <- matrix(0, n, p)
    for (j in 1:p) {
      ii <- outer(catg[[j]], T.bar[, j], "-")^2
      info[, j] <- alphas[j]^2 * colSums(prs[[j]] * ii)
    }
    colnames(info) <- names(betas)
    info
  }
