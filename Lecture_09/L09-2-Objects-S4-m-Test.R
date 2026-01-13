###  TESTING

pts1 =
    vcoords(x = round(rnorm(10), 2),
            y = round(rnorm(10), 2),
            value = sample(100, 10,  replace = TRUE))

pts2 =  vcoords(x = round(rnorm(10), 2),
            y = round(rnorm(10), 2),
            value = sample(100, 10,  replace = TRUE))


pts1
sqrt(pts1)
2 * pts1
pts1 < 50
pts1 + pts2
### coords(pts2) = coords(pts1)


### Need Some Other Functionality too
