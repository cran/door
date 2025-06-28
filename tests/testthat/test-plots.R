# Summary data
y1t = c(60, 30, 10)
y2t = c(50, 40, 10)
y1p = y1t / 100
y2p = y2t / 100
# Individual data
trt_data = data.frame(DOOR = c(rep(1, 60), rep(2, 30), rep(3, 10)), ARM = rep(1, 100))
ctr_data = data.frame(DOOR = c(rep(1, 50), rep(2, 40), rep(3, 10)), ARM = rep(0, 100))
comp1 = c(rep(1, 40), rep(2, 60), rep(1, 50), rep(2, 50))
comp2 = c(rep(1, 30), rep(2, 70), rep(1, 40), rep(2, 60))
test_data1 <- rbind.data.frame(trt_data, ctr_data)
test_data1 <- cbind(test_data1, comp1, comp2)
obj1 = door_summary(data = test_data1, doorVar = "DOOR", trtVar = "ARM",
                    trtCodes = c(1,0), compVars = c("comp1", "comp2"))
# Partial credit grade keys
gk1 = c(100, 50, 0)
gk2 = cbind(c(100, 100, 0), c(100, 50, 0), c(100, 0, 0))
pc_obj1 = partial_credit_analysis(grade_key = gk1, y1t, y2t)
pc_obj2 = partial_credit_analysis(grade_key = gk2, y1t, y2t)
# Component table
comptable = data.frame(compname = c("A", "B"), trt = c(30, 20), ctr = c(40, 25))

# Tests
test_that("DOOR barplot", {
  testthat::expect_type(door_barplot(y1 = y1t, y2 = y2t), "list")
  testthat::expect_type(door_barplot(y1 = y1p, y2 = y2p), "list")
  testthat::expect_type(door_barplot(summary_obj = obj1), "list")
})

test_that("DOOR component barplot", {
  testthat::expect_type(door_component_barplot(comp_table = comptable, n1 = 100, n2 = 100), "list")
  testthat::expect_type(door_component_barplot(summary_obj = obj1), "list")
})

test_that("Cumulative forest plots", {
  testthat::expect_type(door_cumulative_forestplot(y1t, y2t), "list")
  testthat::expect_type(door_cumulative_forestplot(y1p, y2p, n1 = 100, n2 = 100, data_type = "prop"), "list")
  testthat::expect_type(door_cumulative_forestplot(summary_obj = obj1), "list")
})

test_that("Component forest plots", {
  testthat::expect_type(door_component_forestplot(comp_table = comptable, y1 = y1t, y2 = y2t), "list")
})

test_that("Partial credit plot", {
  testthat::expect_type(partial_credit_biplot(pc_obj1), "list")
  testthat::expect_type(partial_credit_biplot(pc_obj2), "list")
  testthat::expect_type(partial_credit_contour_plot(y1 = y1t, y2 = y2t), "list")

})




