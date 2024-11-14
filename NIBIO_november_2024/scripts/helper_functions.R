## functions written by Dr Mark Andrews (github.com/mark-andrews)
## glm_loo_cv adapted by Dr Rafael Moral (github.com/rafamoral)

lm_loo_cv <- function(m, se = FALSE, deviance_scale = FALSE) {
  
  data_df <- m$model
  n <- nrow(data_df)
  lm_formula <- formula(m)
  outcome_var <- all.vars(lm_formula)[1]
  
  lm_drop_i <- function(i){
    m_not_i <- lm(lm_formula, data = slice(data_df, -i))
    slice(data_df, i) %>% 
      modelr::add_predictions(m_not_i) %>% 
      transmute(lpd = dnorm(x = .[[outcome_var]], mean = pred, sd = sigma(m_not_i), log = TRUE)) %>% 
      unlist()
  }
  
  s <- map_dbl(seq(n), lm_drop_i)
  
  if (deviance_scale) s <- -2 * s
  
  if (!se) {
    sum(s)
  } else {
    c(elpd = sum(s),
      se = sqrt(length(s)) * sd(s))
  }

}

bootstrap_lm <- function(lm_model, n = 10000) {
  boot <- modelr::bootstrap(lm_model$model, n)
  map(boot$strap, ~lm(formula(lm_model), data = .))
}


AICc <- function(model) {
  
  LL <- logLik(model)
  k <- LL %>% attr('df')
  LL <- as.numeric(LL)
  N <- nrow(model$model)
  
  (-2 * LL + 2 * k) + (2 * k * (k + 1))/(N - k - 1)
  
}

akaike_weights <- function(aic) {
  d_aic <- aic - min(aic)
  f <- exp(-d_aic/2)
  f/sum(f)
}

get_rsq <- function(m) summary(m)$r.sq
get_rss <- function(m) sum(residuals(m)^2)
get_adjrsq <- function(m) summary(m)$adj.r.sq

glm_loo_cv <- function(m, se = FALSE, deviance_scale = FALSE) {
  
  data_df <- m$model
  fam <- m$family[1]$family
  ddist <- switch(fam,
                  "gaussian" = gamlss.dist::dNO,
                  "Gamma" = gamlss.dist::dGA,
                  "inverse.gaussian" = gamlss.dist::dIG)
  
  n <- nrow(data_df)
  glm_formula <- formula(m)
  outcome_var <- all.vars(glm_formula)[1]
  
  glm_drop_i <- function(i) {
    m_not_i <- update(m, data = slice(data_df, -i))
    slice(data_df, i) %>% 
      modelr::add_predictions(m_not_i, type = "response") %>% 
      transmute(lpd = ddist(x = .[[outcome_var]], mu = pred, sigma = sqrt(summary(m_not_i)$dispersion), log = TRUE)) %>% 
      unlist()
  }
  
  s <- map_dbl(seq(n), glm_drop_i)
  
  if (deviance_scale) s <- -2 * s
  
  if(!se) {
    return(sum(s))
  } else {
    return(c(elpd = sum(s),
             se = sqrt(length(s)) * sd(s)))
  }
  
}