parfm_wcov = function (formula, cluster = NULL, strata = NULL, data, inip = NULL,
          iniFpar = NULL, dist = c("weibull", "inweibull", "frechet",
                                   "exponential", "gompertz", "loglogistic", "lognormal",
                                   "logskewnormal"), frailty = c("none", "gamma", "ingau",
                                                                 "possta", "lognormal", "loglogistic"), method = "nlminb",
          maxit = 500, Fparscale = 1, showtime = FALSE, correct = 0)
{
  if (missing(data)) {
    data <- eval(parse(text = paste("data.frame(", paste(all.vars(formula),
                                                         collapse = ", "), ")")))
  }
  dist <- tolower(dist)
  dist <- match.arg(dist)
  frailty <- tolower(frailty)
  frailty <- match.arg(frailty)
  if (frailty == "none" && !is.null(cluster)) {
    warning(paste0("With frailty='none' the cluster variable '",
                   cluster, "' is not used!"))
  }
  if (frailty == "none" && !is.null(iniFpar)) {
    warning("With frailty='none' the argument 'iniFpar' is not used!")
  }
  if (frailty == "possta") {
    if (10^correct == Inf || 10^-correct == 0) {
      stop("'correct' is too large!")
    }
    if (10^correct == 0 || 10^-correct == Inf) {
      stop("'correct' is too small!")
    }
  }
  else if (correct != 0) {
    warning(paste0("'correct' has no effect when 'frailty = ",
                   frailty, "'"))
  }
  obsdata <- NULL
  if (length(formula[[2]]) == 3) {
    obsdata$time <- eval(formula[[2]][[2]], envir = data)
    obsdata$event <- eval(formula[[2]][[3]], envir = data)
  }
  else if (length(formula[[2]]) == 4) {
    obsdata$trunc <- eval(formula[[2]][[2]], envir = data)
    obsdata$time <- eval(formula[[2]][[3]], envir = data)
    obsdata$event <- eval(formula[[2]][[4]], envir = data)
  }
  if (!all(levels(as.factor(obsdata$event)) %in% 0:1)) {
    stop(paste("The status indicator 'event' in the Surv object",
               "in the left-hand side of the formula object", "must be either 0 (no event) or 1 (event)."))
  }
  obsdata$x <- as.data.frame(model.matrix(formula, data = data))
  if (is.null(cluster)) {
    if (frailty != "none") {
      stop(paste("if you specify a frailty distribution,\n",
                 "then you have to specify the cluster variable as well"))
    }
    else {
      obsdata$cluster <- rep(1, nrow(data))
    }
    obsdata$ncl <- 1
    obsdata$di <- sum(obsdata$event)
  }
  else {
    if (!cluster %in% names(data)) {
      stop(paste0("object '", cluster, "' not found"))
    }
    obsdata$cluster <- eval(as.name(cluster), envir = data)
    obsdata$ncl <- length(levels(as.factor(obsdata$cluster)))
    obsdata$di <- aggregate(obsdata$event, by = list(obsdata$cluster),
                            FUN = sum)[, , drop = FALSE]
    cnames <- obsdata$di[, 1]
    obsdata$di <- as.vector(obsdata$di[, 2])
    names(obsdata$di) <- cnames
  }
  if (is.null(strata)) {
    obsdata$strata <- rep(1, length(obsdata$time))
    obsdata$nstr <- 1
    obsdata$dq <- sum(obsdata$event)
  }
  else {
    if (!strata %in% names(data)) {
      stop(paste0("object '", strata, "' not found"))
    }
    obsdata$strata <- eval(as.name(strata), envir = data)
    obsdata$nstr <- length(levels(as.factor(obsdata$strata)))
    obsdata$dq <- aggregate(obsdata$event, by = list(obsdata$strata),
                            FUN = sum)[, , drop = FALSE]
    snames <- obsdata$dq[, 1]
    obsdata$dq <- as.vector(obsdata$dq[, 2])
    names(obsdata$dq) <- snames
  }
  if (!is.null(cluster) && !is.null(strata)) {
    obsdata$dqi <- xtabs(x ~ Group.1 + Group.2, data = aggregate(obsdata$event,
                                                                 by = list(obsdata$cluster, obsdata$strata), FUN = sum))
    dimnames(obsdata$dqi) <- list(cluster = dimnames(obsdata$dqi)[[1]],
                                  strata = dimnames(obsdata$dqi)[[2]])
  }
  else if (!is.null(cluster)) {
    obsdata$dqi <- obsdata$di
  }
  else if (!is.null(strata)) {
    obsdata$dqi <- obsdata$dq
  }
  else {
    obsdata$dqi <- sum(obsdata$event)
  }
  if (frailty == "none") {
    nFpar <- 0
  }
  else if (frailty %in% c("gamma", "ingau", "possta", "lognormal")) {
    nFpar <- 1
  }
  obsdata$nFpar <- nFpar
  if (dist == "exponential") {
    nBpar <- 1
  }
  else if (dist %in% c("weibull", "inweibull", "frechet", "gompertz",
                       "lognormal", "loglogistic")) {
    nBpar <- 2
  }
  else if (dist %in% c("logskewnormal")) {
    nBpar <- 3
  }
  obsdata$nBpar <- nBpar
  nRpar <- ncol(obsdata$x) - 1
  obsdata$nRpar <- nRpar
  if (!is.null(inip)) {
    if (length(inip) != nBpar * obsdata$nstr + nRpar) {
      stop(paste("number of initial parameters 'inip' must be",
                 nBpar * obsdata$nstr + nRpar))
    }
    p.init <- inip
    if (dist %in% c("exponential", "weibull", "inweibull",
                    "frechet", "gompertz")) {
      if (any(p.init[1:obsdata$nstr] <= 0)) {
        stop(paste("with that baseline, the 1st parameter has to be > 0"))
      }
      p.init[1:obsdata$nstr] <- log(p.init[1:obsdata$nstr])
    }
    if (dist %in% c("weibull", "inweibull", "frechet", "gompertz",
                    "lognormal", "loglogistic", "logskewnormal")) {
      if (any(p.init[obsdata$nstr + 1:obsdata$nstr] <=
              0)) {
        stop(paste("with that baseline, the 2nd parameter has to be > 0"))
      }
      p.init[obsdata$nstr + 1:obsdata$nstr] <- log(p.init[obsdata$nstr +
                                                            1:obsdata$nstr])
    }
  }
  else {
    sink("NUL")
    inires <- optimx(par = rep(0, nRpar + nBpar), fn = optMloglikelihood,
                     method = method, obs = obsdata, dist = dist, frailty = "none",
                     correct = correct, hessian = FALSE, control = list(maxit = maxit,
                                                                        starttests = FALSE, dowarn = FALSE))
    sink()
    p.init <- inires[1:(nRpar + nBpar)]
    rm(inires)
  }
  if (frailty == "none") {
    pars <- NULL
  }
  else if (frailty %in% c("gamma", "ingau", "lognormal")) {
    if (is.null(iniFpar)) {
      iniFpar <- 1
    }
    else if (iniFpar <= 0) {
      stop("initial heterogeneity parameter (theta) has to be > 0")
    }
    pars <- log(iniFpar)
  }
  else if (frailty == "possta") {
    if (is.null(iniFpar)) {
      iniFpar <- 0.5
    }
    else if (iniFpar <= 0 || iniFpar >= 1) {
      stop("initial heterogeneity parameter (nu) must lie in (0, 1)")
    }
    pars <- log(-log(iniFpar))
  }
  pars <- c(pars, unlist(p.init))
  res <- NULL
  sink("NUL")
  todo <- expression({
    res <- optimx(par = pars, fn = optMloglikelihood, method = method,
                  obs = obsdata, dist = dist, frailty = frailty, correct = correct,
                  hessian = FALSE, control = list(maxit = maxit, starttests = FALSE,
                                                  dowarn = FALSE))
  })
  if (showtime) {
    extime <- system.time(eval(todo))[1]
  }
  else {
    eval(todo)
    extime <- NULL
  }
  sink()
  if (res$convcode > 0) {
    warning("optimisation procedure did not converge,\n              conv = ",
            bquote(.(res$convergence)), ": see ?optimx for details")
  }
  it <- res$niter
  lL <- -res$value
  if (frailty == "possta") {
    lL <- lL + correct * log(10) * obsdata$ncl
  }
  estim_par <- as.numeric(res[1:(nFpar + nBpar * obsdata$nstr +
                                   nRpar)])
  if (frailty %in% c("gamma", "ingau")) {
    theta <- exp(estim_par[1:nFpar])
    sigma2 <- NULL
    nu <- NULL
  }
  else if (frailty == "lognormal") {
    theta <- NULL
    sigma2 <- exp(estim_par[1:nFpar])
    nu <- NULL
  }
  else if (frailty == "possta") {
    theta <- NULL
    sigma2 <- NULL
    nu <- exp(-exp(estim_par[1:nFpar]))
  }
  else if (frailty == "none") {
    theta <- NULL
    sigma2 <- NULL
    nu <- NULL
  }
  if (dist == "exponential") {
    lambda <- exp(estim_par[nFpar + 1:obsdata$nstr])
    ESTIMATE <- c(lambda = lambda)
  }
  else if (dist %in% c("weibull", "inweibull", "frechet")) {
    rho <- exp(estim_par[nFpar + 1:obsdata$nstr])
    lambda <- exp(estim_par[nFpar + obsdata$nstr + 1:obsdata$nstr])
    ESTIMATE <- c(rho = rho, lambda = lambda)
  }
  else if (dist == "gompertz") {
    gamma <- exp(estim_par[nFpar + 1:obsdata$nstr])
    lambda <- exp(estim_par[nFpar + obsdata$nstr + 1:obsdata$nstr])
    ESTIMATE <- c(gamma = gamma, lambda = lambda)
  }
  else if (dist == "lognormal") {
    mu <- estim_par[nFpar + 1:obsdata$nstr]
    sigma <- exp(estim_par[nFpar + obsdata$nstr + 1:obsdata$nstr])
    ESTIMATE <- c(mu = mu, sigma = sigma)
  }
  else if (dist == "loglogistic") {
    alpha <- estim_par[nFpar + 1:obsdata$nstr]
    kappa <- exp(estim_par[nFpar + obsdata$nstr + 1:obsdata$nstr])
    ESTIMATE <- c(alpha = alpha, kappa = kappa)
  }
  else if (dist == "logskewnormal") {
    xi <- estim_par[nFpar + 1:obsdata$nstr]
    omega <- exp(estim_par[nFpar + obsdata$nstr + 1:obsdata$nstr])
    alpha <- estim_par[nFpar + 2 * obsdata$nstr + 1:obsdata$nstr]
    ESTIMATE <- c(xi = xi, omega = omega, alpha = alpha)
  }
  if (nRpar == 0) {
    beta <- NULL
  }
  else {
    beta <- estim_par[-(1:(nFpar + nBpar * obsdata$nstr))]
    names(beta) <- paste("beta", names(obsdata$x), sep = ".")[-1]
  }
  ESTIMATE <- c(theta = theta, sigma2 = sigma2, nu = nu, ESTIMATE,
                beta = beta)
  resHessian <- optimHess(par = ESTIMATE, fn = Mloglikelihood,
                          obs = obsdata, dist = dist, frailty = frailty, correct = correct,
                          transform = FALSE)
  var <- try(diag(solve(resHessian)), silent = TRUE)
  if (class(var) == "try-error" | any(is.nan(var))) {
    warning(var[1])
    STDERR <- rep(NA, nFpar + nBpar * obsdata$nstr + nRpar)
    PVAL <- rep(NA, nFpar + nBpar * obsdata$nstr + nRpar)
  }
  else {
    if (any(var <= 0)) {
      warning(paste("negative variances have been replaced by NAs\n",
                    "Please, try other initial values", "or another optimisation method"))
    }
    if (frailty %in% c("gamma", "ingau")) {
      seTheta <- sapply(1:nFpar, function(x) {
        ifelse(var[x] > 0, sqrt(var[x]), NA)
      })
      seSigma2 <- seNu <- NULL
    }
    else if (frailty == "lognormal") {
      seSigma2 <- sapply(1:nFpar, function(x) {
        ifelse(var[x] > 0, sqrt(var[x]), NA)
      })
      seTheta <- seNu <- NULL
    }
    else if (frailty == "possta") {
      seNu <- sapply(1:nFpar, function(x) {
        ifelse(var[x] > 0, sqrt(var[x]), NA)
      })
      seTheta <- seSigma2 <- NULL
    }
    if (dist == "exponential") {
      seLambda <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + x] > 0, sqrt(var[nFpar + x]),
               NA)
      })
      STDERR <- c(seLambda = seLambda)
    }
    else if (dist %in% c("weibull", "inweibull", "frechet")) {
      seRho <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + x] > 0, sqrt(var[nFpar + x]),
               NA)
      })
      seLambda <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + obsdata$nstr + x] > 0, sqrt(var[nFpar +
                                                             obsdata$nstr + x]), NA)
      })
      STDERR <- c(seRho = seRho, seLambda = seLambda)
    }
    else if (dist == "gompertz") {
      seGamma <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + x] > 0, sqrt(var[nFpar + x]),
               NA)
      })
      seLambda <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + obsdata$nstr + x] > 0, sqrt(var[nFpar +
                                                             obsdata$nstr + x]), NA)
      })
      STDERR <- c(seGamma = seGamma, seLambda = seLambda)
    }
    else if (dist == "lognormal") {
      seMu <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + x] > 0, sqrt(var[nFpar + x]),
               NA)
      })
      seSigma <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + obsdata$nstr + x] > 0, sqrt(var[nFpar +
                                                             obsdata$nstr + x]), NA)
      })
      STDERR <- c(seMu = seMu, seSigma = seSigma)
    }
    else if (dist == "loglogistic") {
      seAlpha <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + x] > 0, sqrt(var[nFpar + x]),
               NA)
      })
      seKappa <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + obsdata$nstr + x] > 0, sqrt(var[nFpar +
                                                             obsdata$nstr + x]), NA)
      })
      STDERR <- c(seAlpha = seAlpha, seKappa = seKappa)
    }
    else if (dist == "logskewnormal") {
      seXi <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + x] > 0, sqrt(var[nFpar + x]),
               NA)
      })
      seOmega <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + obsdata$nstr + x] > 0, sqrt(var[nFpar +
                                                             obsdata$nstr + x]), NA)
      })
      seAlpha <- sapply(1:obsdata$nstr, function(x) {
        ifelse(var[nFpar + 2 * obsdata$nstr + x] > 0,
               sqrt(var[nFpar + 2 * obsdata$nstr + x]), NA)
      })
      STDERR <- c(seXi = seXi, seOmega = seOmega, seAlpha = seAlpha)
    }
    if (nRpar == 0) {
      seBeta <- NULL
    }
    else {
      seBeta <- numeric(nRpar)
      varBeta <- var[-(1:(nFpar + nBpar * obsdata$nstr))]
      for (i in 1:nRpar) {
        seBeta[i] <- ifelse(varBeta[i] > 0, sqrt(varBeta[i]),
                            NA)
      }
      PVAL <- c(rep(NA, nFpar + nBpar * obsdata$nstr),
                2 * pnorm(q = -abs(beta/seBeta)))
    }
    STDERR <- c(STDERR, se.beta = seBeta)
    if (frailty != "none") {
      STDERR <- c(se.theta = seTheta, se.sigma2 = seSigma2,
                  se.nu = seNu, STDERR)
    }
  }
  resmodel <- cbind(ESTIMATE = ESTIMATE, SE = STDERR)
  rownames(resmodel) <- gsub("beta.", "", rownames(resmodel))
  if (nRpar > 0)
    resmodel <- cbind(resmodel, `p-val` = PVAL)
  class(resmodel) <- c("parfm", class(resmodel))
  Call <- match.call()
  if (!match("formula", names(Call), nomatch = 0))
    stop("A formula argument is required")
  Terms <- terms(formula, data = data)
  attributes(resmodel) <- c(attributes(resmodel), list(call = Call,
                                                       convergence = res$convergence, it = it, extime = extime,
                                                       nobs = nrow(data), shared = (nrow(data) > obsdata$ncl),
                                                       loglik = lL, dist = dist, cumhaz = attributes(Mloglikelihood(p = estim_par,
                                                                                                                    obs = obsdata, dist = dist, frailty = frailty, correct = correct))$cumhaz,
                                                       cumhazT = attributes(Mloglikelihood(p = estim_par, obs = obsdata,
                                                                                           dist = dist, frailty = frailty, correct = correct))$cumhazT,
                                                       di = obsdata$di, dq = obsdata$dq, dqi = obsdata$dqi,
                                                       frailty = frailty, clustname = cluster, stratname = strata,
                                                       correct = correct, formula = as.character(Call[match("formula",
                                                                                                            names(Call), nomatch = 0)]), terms = attr(Terms,
                                                                                                                                                      "term.labels"), FisherI = resHessian))
  if (frailty != "none") {
    names(attr(resmodel, "cumhaz")) <- names(attr(resmodel,
                                                  "di")) <- unique(obsdata$cluster)
  }
  if (showtime)
    cat("\nExecution time:", extime, "second(s) \n")
  return(list( "model" = resmodel, "cov" = solve(resHessian)))
}

Mloglikelihood <- function(p,
                           obs,
                           dist,
                           frailty,
                           correct,
                           transform = TRUE) {
  # ---- Assign the number of frailty parameters 'obs$nFpar' --------------- #
  # ---- and compute Sigma for the Positive Stable frailty ----------------- #

  if (frailty %in% c("gamma", "ingau")) {
    theta <- ifelse(transform, exp(p[1]), p[1])
  } else if (frailty == "lognormal") {
    sigma2 <- ifelse(transform, exp(p[1]), p[1])
  } else if (frailty == "possta") {
    nu <- ifelse(transform, exp(-exp(p[1])), p[1])
    D <- max(obs$dqi)
    Omega <- Omega(D, correct = correct, nu = nu)
  }


  # ---- Baseline hazard --------------------------------------------------- #
  if (frailty == 'none') obs$nFpar <- 0

  # baseline parameters
  if (dist %in% c("weibull", "inweibull", "frechet")) {
    if (transform) {
      pars <- cbind(rho    = exp(p[obs$nFpar + 1:obs$nstr]),
                    lambda = exp(p[obs$nFpar + obs$nstr + 1:obs$nstr]))
    } else {
      pars <- cbind(rho    = p[obs$nFpar + 1:obs$nstr],
                    lambda = p[obs$nFpar + obs$nstr + 1:obs$nstr])
    }
    beta <- p[-(1:(obs$nFpar + 2 * obs$nstr))]
  } else if (dist == "exponential") {
    if (transform) {
      pars <- cbind(lambda = exp(p[obs$nFpar + 1:obs$nstr]))
    } else {
      pars <- cbind(lambda = p[obs$nFpar + 1:obs$nstr])
    }
    beta <- p[-(1:(obs$nFpar + obs$nstr))]
  } else if (dist == "gompertz") {
    if (transform) {
      pars <- cbind(gamma  = exp(p[obs$nFpar + 1:obs$nstr]),
                    lambda = exp(p[obs$nFpar + obs$nstr + 1:obs$nstr]))
    } else {
      pars <- cbind(gamma  = p[obs$nFpar + 1:obs$nstr],
                    lambda = p[obs$nFpar + obs$nstr + 1:obs$nstr])
    }
    beta <- p[-(1:(obs$nFpar + 2 * obs$nstr))]
  } else if (dist == "lognormal") {
    if (transform) {
      pars <- cbind(mu    = p[obs$nFpar + 1:obs$nstr],
                    sigma = exp(p[obs$nFpar + obs$nstr + 1:obs$nstr]))
    } else {
      pars <- cbind(mu    = p[obs$nFpar + 1:obs$nstr],
                    sigma = p[obs$nFpar + obs$nstr + 1:obs$nstr])
    }
    beta <- p[-(1:(obs$nFpar + 2 * obs$nstr))]
  } else if (dist == "loglogistic") {
    if (transform) {
      pars <- cbind(alpha = p[obs$nFpar + 1:obs$nstr],
                    kappa = exp(p[obs$nFpar + obs$nstr + 1:obs$nstr]))
    } else  {
      pars <- cbind(alpha = p[obs$nFpar + 1:obs$nstr],
                    kappa = p[obs$nFpar + obs$nstr + 1:obs$nstr])
    }
    beta <- p[-(1:(obs$nFpar + 2 * obs$nstr))]
  } else if (dist == "logskewnormal") {
    if (transform) {
      pars <- cbind(mu    = p[obs$nFpar + 1:obs$nstr],
                    sigma = exp(p[obs$nFpar + obs$nstr + 1:obs$nstr]),
                    alpha = exp(p[obs$nFpar + 2 * obs$nstr + 1:obs$nstr]))
    } else {
      pars <- cbind(mu    = p[obs$nFpar + 1:obs$nstr],
                    sigma = p[obs$nFpar + obs$nstr + 1:obs$nstr],
                    alpha = p[obs$nFpar + 2 * obs$nstr + 1:obs$nstr])
    }
    beta <- p[-(1:(obs$nFpar + 3 * obs$nstr))]
  }
  rownames(pars) <- levels(as.factor(obs$strata))

  # baseline: from string to the associated function
  dist <- eval(parse(text = dist))


  # ---- Cumulative Hazard by cluster and by strata ------------------------- #

  cumhaz <- NULL
  cumhaz <- matrix(unlist(
    sapply(levels(as.factor(obs$strata)),
           function(x) {t(
             cbind(dist(pars[x, ], obs$time[obs$strata == x], what = "H"
             ) * exp(as.matrix(obs$x)[
               obs$strata == x, -1, drop = FALSE] %*% as.matrix(beta)),
             obs$cluster[obs$strata == x]))
           })), ncol = 2, byrow = TRUE)
  cumhaz <- aggregate(cumhaz[, 1], by = list(cumhaz[, 2]),
                      FUN = sum)[, 2, drop = FALSE]
  ### NO FRAILTY
  if (frailty == "none") cumhaz <- sum(cumhaz)

  # Possible truncation
  if (!is.null(obs$trunc)) {
    cumhazT <- matrix(unlist(
      sapply(levels(as.factor(obs$strata)),
             function(x) {t(
               cbind(dist(pars[x, ], obs$trunc[obs$strata == x], what = "H"
               ) * exp(as.matrix(obs$x)[
                 obs$strata == x, -1, drop = FALSE] %*% as.matrix(beta)),
               obs$cluster[obs$strata == x]))
             })), ncol = 2, byrow = TRUE)
    cumhazT <- aggregate(cumhazT[, 1], by = list(cumhazT[, 2]),
                         FUN = sum)[, 2, drop = FALSE]
    ### NO FRAILTY
    if (frailty == "none") cumhazT <- sum(cumhazT)
  }

  # ---- log-hazard by cluster --------------------------------------------- #
  loghaz <- NULL
  if (frailty != "none")  {
    loghaz <- matrix(unlist(
      sapply(levels(as.factor(obs$strata)),
             function(x) {
               t(cbind(obs$event[obs$strata == x] * (
                 dist(pars[x, ], obs$time[obs$strata == x],
                      what = "lh") +
                   as.matrix(obs$x)[
                     obs$strata == x, -1, drop = FALSE] %*%
                   as.matrix(beta)),
                 obs$cluster[obs$strata == x]))
             })), ncol = 2, byrow = TRUE)
    loghaz <- aggregate(loghaz[, 1], by = list(loghaz[, 2]), FUN = sum)[
      , 2, drop = FALSE]
  } else {
    loghaz <- sum(apply(cbind(rownames(pars), pars), 1,
                        function(x) {
                          sum(obs$event[obs$strata == x[1]] * (
                            dist(as.numeric(x[-1]),
                                 obs$time[obs$strata == x[1]],
                                 what = "lh") +
                              as.matrix(obs$x[
                                obs$strata == x[1], -1, drop = FALSE]
                              ) %*% as.matrix(beta)))
                        }))
  }


  # ---- log[ (-1)^di L^(di)(cumhaz) ]-------------------------------------- #
  logSurv <- NULL
  if (frailty == "gamma") {
    logSurv <- mapply(fr.gamma,
                      k = obs$di, s = as.numeric(cumhaz[[1]]),
                      theta = rep(theta, obs$ncl),
                      what = "logLT")
  } else if (frailty == "ingau") {
    logSurv <- mapply(fr.ingau,
                      k = obs$di, s = as.numeric(cumhaz[[1]]),
                      theta = rep(theta, obs$ncl),
                      what = "logLT")
  } else if (frailty == "possta") {
    logSurv <- sapply(1:obs$ncl,
                      function(x) fr.possta(k = obs$di[x],
                                            s = as.numeric(cumhaz[[1]])[x],
                                            nu = nu, Omega = Omega,
                                            what = "logLT",
                                            correct = correct))
  } else if (frailty == "lognormal") {
    logSurv <- mapply(fr.lognormal,
                      k = obs$di, s = as.numeric(cumhaz[[1]]),
                      sigma2 = rep(sigma2, obs$ncl),
                      what = "logLT")
  } else if (frailty == "none") {
    logSurv <- mapply(fr.none, s = cumhaz, what = "logLT")
  }

  ### Possible left truncation
  if (!is.null(obs$trunc)) {
    logSurvT <- NULL
    if (frailty == "gamma") {
      logSurvT <- mapply(fr.gamma,
                         k = 0, s = as.numeric(cumhazT[[1]]),
                         theta = rep(theta, obs$ncl),
                         what = "logLT")
    } else if (frailty == "ingau") {
      logSurvT <- mapply(fr.ingau,
                         k = 0, s = as.numeric(cumhazT[[1]]),
                         theta = rep(theta, obs$ncl),
                         what = "logLT")
    } else if (frailty == "possta") {
      logSurvT <- sapply(1:obs$ncl,
                         function(x) fr.possta(
                           k = 0,
                           s = as.numeric(cumhazT[[1]])[x],
                           nu = nu, Omega = Omega,
                           what = "logLT",
                           correct = correct))
    } else if (frailty == "lognormal") {
      logSurvT <- mapply(fr.lognormal,
                         k = 0, s = as.numeric(cumhazT[[1]]),
                         sigma2 = rep(sigma2, obs$ncl),
                         what = "logLT")
    } else if (frailty == "none") {
      logSurvT <- mapply(fr.none, s = cumhazT, what = "logLT")
    }
  }


  # ---- Minus the log likelihood ------------------------------------------ #
  Mloglik <- -sum(as.numeric(loghaz[[1]]) + logSurv)
  if (!is.null(obs$trunc)) {
    Mloglik <- Mloglik + sum(logSurvT)
  }
  attr(Mloglik, "cumhaz") <- as.numeric(cumhaz[[1]])
  if (!is.null(obs$trunc)) {
    attr(Mloglik, "cumhazT") <- as.numeric(cumhazT[[1]])
  } else {
    attr(Mloglik, "cumhazT") <- NULL
  }
  attr(Mloglik, "loghaz") <- as.numeric(loghaz[[1]])
  attr(Mloglik, "logSurv") <- logSurv
  if (!is.null(obs$trunc)) {
    attr(Mloglik, "logSurvT") <- logSurvT
  }
  return(Mloglik)
}
################################################################################
################################################################################



################################################################################
# the same as Mloglikelihood, without attributes, to be passed to optimx()     #
################################################################################
optMloglikelihood <- function(p, obs, dist, frailty, correct) {
  res <- Mloglikelihood(p = p, obs = obs, dist = dist,
                        frailty = frailty, correct = correct)
  as.numeric(res)}
################################################################################
