y1t = c(60,30,10)
y2t = c(50,40,10)
y1t1 = y1t/100
y2t1 = y2t/100

trt_data = data.frame(DOOR = c(rep(1,60), rep(2,30), rep(3,10)), ARM = rep(1,100))
ctr_data = data.frame(DOOR = c(rep(1,50), rep(2,40), rep(3,10)), ARM = rep(0,100))
test_data1 <- rbind.data.frame(trt_data, ctr_data)
obj1 = door_summary(data = test_data1, doorVar = "DOOR",
                    trtVar = "ARM", trtCodes = c(1, 0))

test_that("Calculate DOOR probability with frequency data", {
  testthat::expect_equal(calc_doorprob(y1t, y2t)[[1]], 0.545)
})

test_that("Calculate DOOR probability with proportion data", {
  testthat::expect_equal(calc_doorprob(y1t1, y2t1, data_type = "prop")[[1]], 0.545)

})

test_that("Calculate DOOR probability with summary object", {
  testthat::expect_equal(calc_doorprob(summary_obj = obj1)[[1]], 0.545)
})

test_that("Calculate DOOR probability with missing data", {
  testthat::expect_type(calc_doorprob(y1 = c(50,NA,30,20,10),
                                      y2 = c(40,30,NA,10,10)),
                        "double")
  testthat::expect_type(calc_doorprob(y1 = c(50,NA,30,20,0)/100,
                                      y2 = c(40,30,NA,10,20)/100, data_type = "prop"),
                        "double")
})

test_that("Calculate DOOR probability with extreme data", {
  testthat::expect_type(calc_doorprob(y1 = c(50,0,0,0,0),
                                      y2 = c(0,30,0,0,10)),
                        "double")
  testthat::expect_type(calc_doorprob(y1 = c(0,0,30,20,10),
                                      y2 = c(20,30,0,0,10)),
                        "double")
})




