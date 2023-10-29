#' @title RC-class ridge regression model
#'
#' @name ridgereg
#' 
#' @description 
#' An object of RC-class that is used to create a ridge regression model
#' for a given formula and dataset. The created model can then be used to access varying functions for analyzing.
#' Functions available are: print, pred and coef.
#'
#' @param formula
#' A formula denoted as 'Y ~ X' that formalizes the relationship to be linearly calculated.
#' The independent variables on the right side of the tilde sign can include multiple,
#' separated by '+'.
#'
#' @param data A data frame that contains the variables which are to be modelled.
#'
#' @param lambda Hyperparameter to tune the model coefficients
#' 
#' @returns An RC-class object of the created ridge regression model on which
#'  analyze can be made.
#'
#' 
#' @export ridgereg
#' 
#' @import stringr
#' 
#' @field formula A ridge regression-formula.
#' @field data A data frame with variables for the model.
#' @field Y A vector for the response variable.
#' @field X A matrix of the x variables.
#' @field beta_hat A matrix with the coefficients.
#' @field y_hat A vector with predicted values.
#' @field Xnrom A matrix with scaled variable values
#' @field df_name A character vector with the name of the data frame.
#' 
#' @source \url{https://en.wikipedia.org/wiki/Ridge_regression}


ridgereg <- setRefClass('ridgereg', fields = list(formula = 'formula',
                                                  data = 'data.frame',
                                                  lambda = 'numeric',
                                                  Y= 'numeric',
                                                  X = 'matrix',
                                                  Xnorm = 'matrix',
                                                  beta_hat='matrix', y_hat='array',
                                                  df_name='character')
                                                  
                                                  ,
                        methods = list(
                          initialize = function(formula, data, lambda){
                            
                            .self$data <- data
                            .self$formula <- formula
                            # checking the input
                            stopifnot(length(all.vars(formula)) >= 1,
                                      is.data.frame(data),all.vars(formula) %in% colnames(data),
                                      lambda>=0)
                            
                            # picking out my variables from the formula
                            .self$Y <- .self$data[,all.vars(.self$formula)[1]]
                            
                            .self$X <- model.matrix(.self$formula, .self$data)[,-1]
                            
                            # getting the sd and mean of the X-matrix
                            Xsd <-apply(X,2,sd)
                            mean_X <- apply(X, 2, mean)
                            
                            # Normalize the numeric variables in the formula and creating the X matrix
                            .self$Xnorm <- scale(X,center=TRUE)
    
                            
                            # Regression calculations
                            # creating the identity matrix with values from lambda instead of 1
                            Iden <- diag(x=lambda, nrow = ncol(t(.self$Xnorm) %*% .self$Xnorm))
                            
                            .self$beta_hat <- solve(t(.self$Xnorm) %*% .self$Xnorm + Iden) %*% (t(.self$Xnorm) %*% Y)
                            
                            # the beta parameters
                            b <- .self$beta_hat/Xsd
                            
                            intercept <- mean(.self$Y) - sum(b*mean_X)
                            
                            .self$beta_hat <- rbind(intercept,b)
                            
                            
                            # Estimating Y_hat
                            .self$y_hat <- as.matrix(cbind(const=1,.self$X)) %*% .self$beta_hat

                            
                            # taking the name of the data frame
                            .self$df_name <-  deparse(substitute(.self$data))
                          },
                          
                          print = function(){
                            
                            "Printing the formula and the coefficients" 
                            # making it look like printing the lm-function
                            cat("Call:","\nlinreg(formula = ", format(.self$formula),", data = ",
                                format(.self$df_name),")\n\n", sep="")
                            
                            cat('Coefficients:\n')
                            
                            mat <- c(t(.self$beta_hat))
                            names(mat) <- colnames(t(.self$beta_hat))
                            base::print(round(mat,3))
                            
                            
                          },
                          
                          show = function(){
                            
                            "The coefficients as a named vector when printing the object"
                            # the coefficients as a named vector
                            vec <- as.vector(.self$beta_hat)
                            names(vec) <- rownames(.self$beta_hat) # giving it the names
                            base::print(vec)
                            },
                          
                          pred = function(newdata=data){
                            "Return the predicted Y values of training data or new data"
                            
                            # Checking the input
                            stopifnot(all.vars(.self$formula)[-1] %in% colnames(newdata))
                            
                            # creating a formula without y, as the new data probably don't have y
                            form <- as.formula(stringr::str_flatten(.self$formula[-2]))
                            
                            x <- model.matrix(form, newdata)
            
                            pred_Y <- x  %*%  .self$beta_hat # calculate the predictions
                            
                            base::print(as.vector(pred_Y))
                            },
                          
                          
                          
                          coef = function(){
                            "The coefficients as a named vector"
                            # the coefficients as a named vector
                            vec <- as.vector(.self$beta_hat)
                            names(vec) <- rownames(.self$beta_hat) # giving it the names
                            base::print(vec)}
                          
                            
                     )
                 )


