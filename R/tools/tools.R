irls <- function(imodel, wfunc, tol) {
  
  model_data <- eval(imodel$call$data)
  
  repeat {
    b0 <- imodel$coef
    model_data$wts <- wfunc(imodel)
    imodel <- lm(formula(imodel), weights=wts, data=model_data)
    b1 <- imodel$coef
    if(max(abs((b1-b0)/b0))<=tol) break
  }
  
  imodel
}

send_email <- function(to, subject, body, attachment = "") {
  
  if(attachment == "") att_flag <- ""
  else att_flag <- paste0("-a ", attachment)
  command <- paste0("echo '", body, "' | mutt -s '", subject, "' ", att_flag, " -- ", to)
  system(command = command)  
}