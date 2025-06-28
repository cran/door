trt_data = data.frame(DOOR = c(rep(1,60), rep(2,30), rep(3, 10)), ARM = rep(1, 100))
ctr_data = data.frame(DOOR = c(rep(1,50), rep(2,40), rep(3, 10)), ARM = rep(0, 100))
comp1 = c(rep(1, 40), rep(2, 60), rep(1, 50), rep(2, 50))
comp2 = c(rep(1, 30), rep(2, 70), rep(1, 40), rep(2, 60))

test_data1 <- rbind.data.frame(trt_data, ctr_data)
test_data1 <- cbind(test_data1, comp1, comp2)

test_that("DOOR summary transformation works", {
  expect_s3_class(door_summary(data = test_data1, doorVar = "DOOR", trtVar = "ARM",
                               trtCodes = c(1,0)),
                  "DOORSummary")

  expect_s3_class(door_summary(data = test_data1, doorVar = "DOOR", trtVar = "ARM", trtCodes = c(1,0),
                               trtLabel = c("T", "C"), compVars = c("comp1", "comp2")),
                  "DOORSummary")
})
