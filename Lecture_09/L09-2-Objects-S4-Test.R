###  Testing for the "coords" Class

###  Constructor, Printing, Length and Accessors

pts =
    coords(x = round(rnorm(5), 2),
           y = round(rnorm(5), 2))

pts
length(pts)
xcoords(pts)
ycoords(pts)

###  Subsetting

pts[1:2]
pts[-1]
pts[c(TRUE,FALSE)]
pts[1:2] = pts[5]

xcoords(pts)[1] = 7

###  Bounding Box

bbox(pts)

###  Testing for the "vcoords" Class

vpts = vcoords(x = round(rnorm(5), 2),
              y = round(rnorm(5), 2),
              v = round(runif(5, 0, 100)))

vpts
length(vpts)
xcoords(vpts)
ycoords(vpts)
values(vpts)
values(vpts)
values(vpts)[1] = NA

###  Subsetting

vpts[1] = vpts[5]

###  Math methods and Arithmetic

sqrt(vpts)
vpts + 10
vpts + sqrt(vpts)

###  This will work when multiple inheritance is used to derive the
###  "vcoords" class from "numeric" and "coords".  No code needs to be
###  written.

values(vpts)[c(1, 4)] = NA
which(is.na(vpts))
which(is.finite(vpts))

###  Coercion Methods between the "coords" and "vcoords" functions.
###  This requires as call to setAs.

as(vpts, "coords")
