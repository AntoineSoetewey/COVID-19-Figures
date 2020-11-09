# broom functions for mgcv models
augment.gam <- function(model, se_fit = FALSE, newdata = NULL){
  if (is.null(newdata)){
    r <- model.frame(model)
    if(se_fit){
      tmp <- predict(model, type = "link", se.fit = se_fit)
      r$.fitted <- tmp$fit
      r$.se.fit <- tmp$se.fit
    } else{
      r$.fitted <- fitted(model)
    }
    r$.resid <- resid(model)
    r$.std.resid <- residuals(model, type = "scaled.pearson")
    r$.hat <- model$hat
    r$.cooksd <- cooks.distance(model)
  } else {
    r <- as.data.frame(newdata)
    if(se_fit){
      tmp <- predict(model, type = "link", newdata = newdata, se.fit = se_fit)
      r$.fitted <- tmp$fit
      r$.se.fit <- tmp$se.fit
    } else{
      r$.fitted <- predict(model, type = "link", newdata = newdata)
    }

  }
  return(r)
}

tidy.gam <- function (x, conf.int = FALSE, conf.level = 0.95, exponentiate = FALSE,
          ...)
{
  ret <- as_tibble(summary(x)$p.table, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error",
                     "statistic", "p.value")
  coefs <- tibble::enframe(stats::coef(x), name = "term",
                           value = "estimate")
  ret <- left_join(coefs, ret, by = c("term", "estimate"))
  if (conf.int) {
    ci <- broom_confint_terms(x, level = conf.level)
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  if (exponentiate) {
    ret <- exponentiate(ret)
  }
  ret
}
