# summary data
y1t = c(60,30,10)
y2t = c(50,40,10)
p1t = y1t / 100
p2t = y2t / 100
n1 = 100
n2 = 100
# individual data
trt_data = data.frame(DOOR = c(rep(1,60), rep(2,30), rep(3, 10)), ARM = rep(1, 100))
ctr_data = data.frame(DOOR = c(rep(1,50), rep(2,40), rep(3, 10)), ARM = rep(0, 100))
test_data1 <- rbind.data.frame(trt_data, ctr_data)
obj1 = door_summary(data = test_data1, doorVar = "DOOR", trtVar = "ARM",
                    trtCodes = c(1,0))


test_that("Hypothesis test with frequency data", {
  testthat::expect_s3_class(door_test(y1t, y2t), "htest")
})

test_that("Hypothesis test with proportion data", {
  testthat::expect_s3_class(door_test(p1t, p2t, n1 = 100, n2 = 100, data_type = "prop"), "htest")
})

test_that("Hypothesis test with individual data", {
  testthat::expect_s3_class(door_test(summary_obj = obj1), "htest")
})

