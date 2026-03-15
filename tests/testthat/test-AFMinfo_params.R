test_that("AFMinfo image sizes", {
  s_list = AFM.getSampleImages()
  s_list = sort(s_list)
  size_um = c(4,3.25,1,10,NA,2.5)
  for (i in seq_along(s_list)) {
    q <- AFMinfo(s_list[i])
    # cat(s_list[i],"  ",q$imageSize.um," expected:", size_um[i], "\n")
    expect_equal(q$imageSize.um, size_um[i])
  }
})

test_that("AFMinfo scan angles", {
  s_list = AFM.getSampleImages()
  s_list = sort(s_list)
  angle = c(0,0,0,0,NA,0)
  for (i in seq_along(s_list)) {
    q <- AFMinfo(s_list[i])
    # cat(s_list[i],"  ",q$scanAngle,"\n")
    expect_equal(q$scanAngle, angle[i])
  }
})
