gk1 <- c(100, 80, 60, 40, 0)
gk2 <- c(100, 100, 100, 100, 0)
gk3 <- c(100, 100, 100, 0, 0)
gk4 <- c(100, 100, 0, 0, 0)
gk5 <- c(100, 0, 0, 0, 0)
gk <- cbind(gk1, gk2, gk3, gk4, gk5)

y1 <- c(60, 30, 20, 10, 10)
y2 <- c(50, 40, 10, 5, 15)
y1p <- c(40, 30, 20, 5, 5)/100
y2p <- c(30, 30, 30, 5, 5)/100

# individual data
trt_data = data.frame(DOOR = c(rep(1,60), rep(2,30), rep(3, 10), rep(4, 5), rep(5, 15)), ARM = rep(1, 120))
ctr_data = data.frame(DOOR = c(rep(1,50), rep(2,40), rep(3, 10), rep(5, 15), rep(5, 5)), ARM = rep(0, 120))
test_data1 <- rbind.data.frame(trt_data, ctr_data)
obj1 = door_summary(data = test_data1, doorVar = "DOOR", trtVar = "ARM", trtCodes = c(1,0))

test_that("partial credit analysis", {
  expect_s3_class(partial_credit_analysis(grade_key = gk, y1 = y1, y2 = y2),
                  "partial_credit_summary")

  expect_s3_class(partial_credit_analysis(grade_key = gk, y1 = y1p, y2 = y2p,
                                          n1 = 100, n2 = 100, data_type = "prop"),
                  "partial_credit_summary")

  expect_s3_class(partial_credit_analysis(grade_key = gk, summary_obj = obj1),
                  "partial_credit_summary")
})
