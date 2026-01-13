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
pts
xcoords(pts)[1] = 7
pts

###  Bounding Box

bb = bbox(pts)
bb
xcoords(bb)
ycoords(bb)


###  Testing for the "vcoords" Class

vpts = vcoords(x = round(rnorm(5), 2),
               y = round(rnorm(5), 2),
               v = round(runif(5, 0, 100)))

vpts
length(vpts)
xcoords(vpts)
ycoords(vpts)
values(vpts)
values(vpts)[1] = NA
vpts

###  Subsetting

vpts[1] = vpts[5]

###  Math methods and Arithmetic

sqrt(vpts)
vpts + 10
vpts + sqrt(vpts)


###  Coercion Methods between the "coords" and "vcoords" functions

as.coords(vpts)
as.vcoords(pts)
values(pts) = 1:5
pts
