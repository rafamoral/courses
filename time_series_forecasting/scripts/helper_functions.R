## Code adapted from Nicholas Clark

# Replicating mgcv::gam.check() but without the plots
small_check <- function(model){
  cat("Basis dimension (k) checking results. Low p-value (k-index < 1) may\n") 
  cat("indicate that k is too low, especially if edf is close to k\'.\n\n")
  printCoefmat(mgcv::k.check(model), digits = 3)
}

# Call marginaleffects::plot_predictions to inspect model fits
plot_fcs <- function(model, n_ahead = 0){
  p <- plot_predictions(model, 
                        condition = list(time = 1:(max(dat$time) + n_ahead)),
                        type = 'response',
                        points = 0.5)
  
  if(n_ahead > 0){
    p <- p + 
      geom_vline(xintercept = max(dat$time), 
                 linetype = 'dashed')
  }
  p
}

# Compute the covariance kernel for a given draw
# of GP alpha and rho parameters
quad_kernel = function(rho, alpha, times){
  covs <- alpha ^ 2 * 
    exp(-0.5 * ((times / rho) ^ 2))
  return(covs)
}

# Compute kernels for each posterior draw
plot_kernels = function(gp_ests, max_time = 50){
  # set up an empty matrix to store the kernel estimates 
  # for each posterior draw
  draw_seq <- seq(0, max_time, length.out = 100)
  kernels <- matrix(NA, nrow = NROW(gp_ests), ncol = 100)
  
  # use a for-loop to estimate the kernel for each draw using
  # our custom quad_kernel() function
  for(i in 1:NROW(gp_ests)){
    kernels[i,] <- quad_kernel(rho = gp_ests$`rho_gp_trend_time_`[i],
                               alpha = gp_ests$`alpha_gp_trend_time_`[i],
                               times = draw_seq)
  }
  
  # Calculate posterior empirical quantiles of the kernels
  probs <- c(0.05, 0.2, 0.5, 0.8, 0.95)
  cred <- sapply(1:NCOL(kernels),
                 function(n) quantile(kernels[,n],
                                      probs = probs))
  
  # Plot the kernel uncertainty estimates
  pred_vals <- draw_seq
  plot(1, xlim = c(0, max_time), ylim = c(0, max(cred)), type = 'n',
       xlab = 'Time difference', ylab = 'Covariance',
       bty = 'L')
  box(bty = 'L', lwd = 2)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[1,], rev(cred[5,])),
          col = "#DCBCBC", border = NA)
  polygon(c(pred_vals, rev(pred_vals)), c(cred[2,], rev(cred[4,])),
          col = "#B97C7C", border = NA)
  lines(pred_vals, cred[3,], col = "#7C0000", lwd = 2.5)
}