#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'


linreg <- function(formula, data){
  # cheking the input
  stopifnot(length(all.vars(formula)) > 1,
            is.data.frame(data))
  # Picking out the Y variable and creating the X-matrix
  Y <- data[,all.vars(formula)[1]]
  X <- model.matrix(formula, data)
  
  # calculating the coefficients
  beta_hat <- solve(t(X)%*%X) %*% t(X) %*% Y
  
  # Estimating Y_hat
  y_hat <- X %*% beta_hat
  
  # The residuals
  e <- y-y_hat
  
  # Degrees of freedom
  df <- nrow(X) - ncol(X)
  
  # Residual variance 
  sigma_2 <- (t(e) %*% e) / df
  
  # Variance of beta_hat
  VAR_B <- sigma_2[1] * solve((t(X) %*% X))
  
  # T-values for the beta coefficients
  t_b <- beta_hat/(sqrt(diag(VAR_B)))
  
  # The p-values for the coefficients
  p_values <-  pt(t_b, df=df,lower.tail = FALSE)
  
}



linreg <- setRefClass("linreg", fields = list(formula='formula',data='data.frame',
                                              Y = 'numeric',X = 'matrix',
                                              beta_hat='matrix', y_hat='array',
                                              e='array', df='numeric',
                                              sigma_2='matrix',VAR_B='matrix',
                                              t_b='array', p_values='array'),
                      methods = list(
                        initialize = function(formula, data){
                          
                          .self$data <- data
                          .self$formula <- formula
                          # cheking the input
                         stopifnot(length(all.vars(formula)) >= 1,
                                   is.data.frame(data))
                          # Picking out the Y variable and creating the X-matrix
                          .self$Y <- data[,all.vars(formula)[1]]
                          .self$X <- model.matrix(formula, data)
                          
                          # calculating the coefficients
                          .self$beta_hat <- solve(t(.self$X)%*%.self$X) %*% t(.self$X) %*% .self$Y
                          
                          # Estimating Y_hat
                          .self$y_hat <- .self$X %*% .self$beta_hat
                          
                          # The residuals
                          .self$e <- .self$Y-.self$y_hat
                          
                          # Degrees of freedom
                          .self$df <- nrow(.self$X) - ncol(.self$X)
                          
                          # Residual variance 
                          .self$sigma_2 <- (t(.self$e) %*% .self$e) /.self$df
                          
                          # Variance of beta_hat
                          .self$VAR_B <- .self$sigma_2[1] * solve((t(.self$X) %*% .self$X))
                          
                          # T-values for the beta coefficients
                          .self$t_b <- .self$beta_hat/(sqrt(diag(.self$VAR_B)))
                          
                          # The p-values for the coefficients
                          .self$p_values <- 1 - pt(.self$t_b, df=.self$df)
                          
                        },
                        plot = function() {
                          print(ggplot2::ggplot(data.frame(.self$y_hat,"Residuals"=.self$e),ggplot2::aes(x=y_hat,y=Residuals)) + 
                            ggplot2::geom_point(size=4, fill='white', shape=21) +
                            ggplot2::stat_summary(fun=median, geom='line', color='red') +
                            ggplot2::theme_bw() + ggplot2::xlab(paste0('Fitted Values \n',formula))+
                            ggplot2::ggtitle('Residual vs Fitted values')+
                            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
                          
                         print(ggplot2::ggplot(data.frame(.self$y_hat,"Residuals"=abs(scale(.self$e))),ggplot2::aes(x=y_hat,y=Residuals)) + 
                            ggplot2::geom_point(size=4, fill='white', shape=21) +
                            ggplot2::stat_summary(fun=median, geom='line', color='red') +
                            ggplot2::theme_bw() + ggplot2::xlab(paste0('Fitted Values \n',formula))+
                             ggplot2::ylab('Standardized Residuals')+
                            ggplot2::ggtitle('Scale-Location')+
                            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
                        },
                        summary = function() {
                          cat('Call \n', formula, 'data=',data,'\n')
                      
                          cat('Coefficients:')
                          tab <- data.frame(t(.self$beta_hat))
                          colnames(tab) <- rownames(.self$beta_hat)
                          print(tab, row.names = FALSE)
                          
                          },
                        resid = function() {
                          as.vector(.self$e)
                        },
                        pred = function() {
                          as.vector(.self$y_hat)},
                        coeff = function() {
                          vec <- as.vector(.self$beta_hat)
                          names(vec) <- rownames(.self$beta_hat)
                          vec},
                        summary = function() {
                          
                          p_print <- ifelse(.self$p_values < 2e-16, '<2e-16',round(.self$p_values,4))
                          sum_tab <- data.frame('Estimate'=.self$beta_hat, 'Std.Error'=round(sqrt(diag(.self$VAR_B)),4),
                                            't value' = round(.self$t_b,4), 'Pr(>|t|)' = .self$p_print, check.names = FALSE)
                          print(sum_tab)
                          cat('Residual standard error: ',round(sqrt(.self$sigma_2),4) ,'on ',df,' degrees of freedom' )
                          
                          }
                      )
                      
                      
                      
                      
)
ye <- linreg$new(formula =Petal.Length ~ Species, data = iris)

formula = Petal.Length ~ Species

summary(lm(formula = Petal.Length ~ Species, data = iris))

ye


