#' @title RC-class object to create linear model 
#' 
#' @name linreg
#' 
#' @description 
#' An object of RC-class that is used to create a linear model from given formula.
#' The formula should have variables that stems from a given dataset and formalized as 'Y ~ X'.
#' The created model can then be used to access varying functions for analyzing.
#' Functions available are: print, plot, resid, pred, coef, summary.
#' 
#' @param formula 
#' A formula denoted as 'Y ~ X' that formalizes the relationship to be linearly calculated.
#' The independent variables on the right side of the tilde sign can include multiple, separated by '+'.
#'
#' @param data A data frame that contains the variables which are to be modelled.
#' 
#' 
#' @returns An RC-class object of the created linear model on which analyze can be made.
#'
#' 
#' @export linreg
#' 
#' @import ggplot2 devtools
#' 
#' @field formula A linear regression-formula.
#' @field data A data frame with variables for the model.
#' @field Y A vector for the response variable.
#' @field X A matrix of the x variables.
#' @field beta_hat A matrix with the coefficients.
#' @field y_hat A vector with predicted values.
#' @field e A vector with residuals.
#' @field sigma_2 A matrix for the residual variance.
#' @field VAR_B A matrix with the variance for the coefficients.
#' @field t_b A vector with t-values for each coefficient.
#' @field p_values A vector with p-values for each coefficient.
#' @field df_name A character vector with the name of the data frame.
#' 
#' 
#' @source \url{https://en.wikipedia.org/wiki/Ordinary_least_squares}

linreg <- setRefClass("linreg", fields = list(formula='formula',data='data.frame',
                                              Y = 'numeric',X = 'matrix',
                                              beta_hat='matrix', y_hat='array',
                                              e='array', df='numeric',
                                              sigma_2='matrix',VAR_B='matrix',
                                              t_b='array', p_values='array',
                                              df_name='character'),
                      methods = list(
                        initialize = function(formula, data){
                          
                          .self$data <- data
                          .self$formula <- formula
                          # cheking the input
                         stopifnot(length(all.vars(formula)) >= 1,
                         is.data.frame(data),all.vars(formula) %in% colnames(data))
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
                          
                          # taking ot the name of the dataframe
                          .self$df_name <-  deparse(substitute(data))
                          
                        },
                        plot = function() {
                          "creating a dataframe to create plots"
                          
                          res_fit_df <- data.frame(y_hat, e)
                          res_fit_df$std_res <- as.numeric(abs(scale(res_fit_df$e)))
                          
                          
                          # Picking out the quantiles to calculate outliers
                          res_q <- quantile(res_fit_df$e, probs = c(0.25,0.75))
                          res_q_std <- quantile(res_fit_df$std_res, probs = c(0.25,0.75))
                          
                          # calculating the outliers
                          outliers <- res_fit_df[which(e < res_q[1]-1.6*(res_q[2]-res_q[1]) | e > res_q[2]+1.6*(res_q[2]-res_q[1])),]
                          outliers_std <- res_fit_df[which(res_fit_df$std_res < res_q_std[1]-1.6*(res_q_std[2]-res_q_std[1]) | res_fit_df$std_res > res_q_std[2]+1.6*(res_q_std[2]-res_q_std[1])),]
                         
                          # The first graph of the residuals vs fitted values
                          base::print( ggplot2::ggplot(data = res_fit_df, ggplot2::aes(x = y_hat, y = e)) +
                            # Making it a scatterplot with white points
                            ggplot2::geom_point(size = 4, fill = "white", shape = 21) + ggplot2::theme_bw() + 
                            
                            ggplot2::labs(y = "Residuals", x = paste0("Fitted values \n",deparse(formula)), title = "Residuals vs Fitted") +
                            ggplot2::geom_hline(yintercept = 0, color = "gray", linetype = "dotted") + 
                            # A red line which shows a loess curve
                            ggplot2::stat_smooth(method = "loess", color = "red", se = FALSE, span = 1) +
                            # Changing the theme and labs to make the graph visually better
                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                  text = ggplot2::element_text(size = 12), plot.title = ggplot2::element_text(size = 16, hjust = 0.5)) +
                            ggplot2::geom_text(data = res_fit_df[rownames(outliers),], 
                                      ggplot2::aes(label = rownames(outliers), vjust = 0, hjust = 1.3), check_overlap = TRUE) + 
                            ggplot2::scale_y_continuous(limits = c(-1.5, 1.5), breaks = c(-1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5)))
                          
                          
                          
                          # creating a ggplot2 diagram of the standardized absolute 
                          # squared root residuals 
                          
                          ggplot2::ggplot(data = res_fit_df, ggplot2::aes(x = y_hat, y = sqrt(std_res))) + 
                            # Making it a scatterplot with white points
                            ggplot2::geom_point(size = 4, fill = "white", shape = 21) + ggplot2::theme_bw() + 
                            # Using expression to make a title with square root sign
                            ggplot2::labs(y = expression(sqrt("|Standardized residuals|")), x = paste0("Fitted values \n",deparse(formula)), title = "Scale-Location") +
                            # A red line which shows a loess curve
                            ggplot2::geom_smooth(method = "loess", color = "red", se = FALSE, span = 1) +
                            # Changing the theme and labs to make the graph visually better
                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                  text = ggplot2::element_text(size = 12), plot.title = ggplot2::element_text(hjust = 0.5, size = 16)) +
                            
                            ggplot2::geom_text(data = res_fit_df[rownames(outliers_std),], 
                            ggplot2::aes(label = rownames(outliers_std), vjust = 0, hjust = 1.3), check_overlap = TRUE) + 
                            ggplot2::scale_y_continuous(limits = c(0, 1.8), breaks = c(0, 0.5, 1.0, 1.5))
                        },
                        print = function() {
                           "Printing the formula and the coefficients" 
                          # making it look like printing the lm-function
                          cat("Call:","\nlinreg(formula = ", format(formula),", data = ",
                              format(.self$df_name),")\n\n", sep="")
                        
                          cat('Coefficients:\n')

                          mat <- c(t(.self$beta_hat))
                          names(mat) <- colnames(t(.self$beta_hat))
                          base::print(round(mat,3))
                          
                          },
                        resid = function() {
                          "Return the residuals as a vector"
                          as.vector(.self$e) # the residuals as a vector
                        },
                        pred = function() {
                          "Return the predicted Y values"
                          as.vector(.self$y_hat)}, # the predicted y´s as a vector
                        coef = function() {
                          "The coefficients as a named vector"
                          # the coefficients as a named vector
                          vec <- as.vector(.self$beta_hat)
                          names(vec) <- rownames(.self$beta_hat) # giving it the names
                          vec},
                        summary = function() {
                          "The summary table which looks like the lm summary()"
                          smallval <- ifelse(.self$p_values < 2e-16, '<2e-16',round(.self$p_values,4))
                          stars <- p_values
                          stars[stars < 0.001] <- "***"
                          stars[stars >= 0.001] <- "**"
                          stars[stars >= 0.01] <- "*"
                          stars[stars >= 0.05] <- "."
                          stars[stars >= 0.1] <- " "
                          
                          
                          sum_tab <- data.frame('Estimate'=round(.self$beta_hat,3), 'Std.Error'=round(sqrt(diag(.self$VAR_B)),3),
                                            't value' = round(.self$t_b,3), 'Pr(>|t|)' = smallval," "=stars, check.names = FALSE)
                          cat('Coefficients:\n')
                          
                          
                          base::print(sum_tab)
                          cat('\n','Residual standard error:',round(sqrt(.self$sigma_2),4) ,'on',df,'degrees of freedom' )
                          
                          }
                      )
)
