data("iris")
data('cars')

test_that("Ridge rejects errounous input", {
  # error in formula or data
  expect_error(mod <- ridgereg$new(formula = Petal.Length~Septsal.Width+Sepal.Length, data=iris,lambda=5))
  expect_error(mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data=irfsfdis,lambda=5))
  
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5)
  # New data without variables from the formula
  expect_error(mod <-ridgereg_mod$pred(cars))
  # without lambda
  expect_error(mod <- ridgereg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data=irfsfdis))
  
})


test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})


test_that("results are close to 'MASS' ", {
  mass_reg <- MASS::lm.ridge(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda = 5)
  
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris,lambda=5)

  r1 <- round(ridgereg_mod$coef(),1) # rounding the values so they are the same
  r2 <- round(coefficients(mass_reg),1)
  expect_equal(r1[[1]],r2[[1]]) 
  expect_equal(r1[2],r2[2])
  expect_equal(r1[3],r2[3])
})





