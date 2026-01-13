###  The "coords" Class
###
###  Objects from this class represent locations in the x-y plane.
###  They are created with the "coords" constructor function and the
###  coordinates can be extracted with the "xcoords" and "ycoords"
###  accessor functions.
###
###  Suitable "print" and "length" methods are defined.
###
###  Functions which provide a way of changing the "x" and "y" values
###  within "coords" objects are defined.
###
###  Functions which make it possible to extract and replace subsets
###  of coords objects are also defined.
###
###  A new generic function called "bbox" which obtains bounding boxes
###  is defined as is a method for coords objects.


###  The "coords" constructor and accessor functions.
###
###  These functions define how "coords" objects are created and their
###  component parts accessed.  All functions which maniplate this
###  kind of object should work by calling these functions rather than
###  using the low-level representation.  This makes it possible to
###  change the representation of the objects and to have all the
###  software which manipulates this kind of object continue to work
###  without change.

coords =
    function(x, y)
    {
        if (!is.numeric(x) || !is.numeric(y) ||
            !all(is.finite(x)) ||
            !all(is.finite(y)))
                stop("invalid coordinates")
        if (length(x) != length(y))
            stop("coordinate lengths differ")
        pts = list(x = x, y = y)
        class(pts) = "coords"
        pts
    }

xcoords = function(obj) obj$x
ycoords = function(obj) obj$y

###  Methods for "length" and "print"
###
###  These definitions show how to implement methods for existing
###  generic functions.  Note that all access to the components of the
###  objects is through calls to the accessor functions.

print.coords =
    function(obj)
    {
          print(paste("(",
                      format(xcoords(obj)),
                      ", ",
                      format(ycoords(obj)),
                      ")", sep = ""),
                quote = FALSE)
    }

length.coords =
    function(obj) length(xcoords(obj))


###  Subsetting and Subset Replacement Methods
###
###  Defining this methods makes it possible to write expressions like
###      pts[i]
###  and
###      pts[i] = npts
###  All the standard ways of specifying subsets are supported as is
###  the the standard recycling mechanism.
###
###  The "[.coords" function subsets the x and y values separately and
###  then makes a new "coords" object form the subsetted values.  The
###  "[.coords<-" function works in a similar fashion.

`[.coords` =
    function(x, i)
    coords(xcoords(x)[i], ycoords(x)[i])

`[<-.coords` =
    function(x, i, value) {
        if (!inherits(value, "coords"))
            stop("invalid right-hand side")
        xc = xcoords(x)
        yc = ycoords(x)
        xc[i] = xcoords(value)
        yc[i] = ycoords(value)
        coords(xc, yc)
    }

        
###  Bounding Boxes
###
###  A new generic function which provides a way of obtaining bounding
###  boxes is defined.  An appropriate method for obtaining the
###  bounding box for the points in a "coords" object is provided.
###  Note: This is different from the version in the lecture slides.
###  The version here probably makes more sense.

bbox =
    function(obj)
    UseMethod("bbox")

bbox.coords =
    function(obj)
    coords(range(xcoords(obj)), range(ycoords(obj)))


###  Functions for replacing the "x" and "y" values in "coords"
###  objects.
###
###  This functions make it possible to write expressions like
###      xcoords(pts) = xnew
###      ycoords(pts) = ynew
###  As a bonus (because of the way R works) it also makes it possible
###  to use expressions of the form:
###      xcoords(pts)[i] = xnew
###      ycoords(pts)[i] = ynew
###
###  It will be useful to make these functions generic so that the
###  behaviour can be inherited by related subclasses.
###
###  Again notice that all manipulation of the objects takes place
###  using the constructor and accessors.

`xcoords<-` = function(x, value) UseMethod("xcoords<-")
`ycoords<-` = function(x, value) UseMethod("ycoords<-")

`xcoords<-.coords` =
    function(x, value) {
        if (!is.numeric(value) && length(value) != length(x))
            stop("invalid x replacement")
        coords(value, ycoords(x))
    }

`ycoords<-.coords` =
    function(x, value) {
        if (!is.numeric(value) && length(value) != length(x))
            stop("invalid y replacement")
        coords(xcoords(x), value)
    } 
    



###  The "vcoords" Class
###
###  This class of object is "just like" a coords object, but with the
###  addition of numerical values at each point.  The similarity is
###  such that "vcoords" is defined as a subclass of "coords" so that
###  existing behaviour can be simply "inherited" from "coords".
###
###  This is done by making simply adding a "v" value to the existing
###  representation of coords objects and making the associated class
###  string be c("vcoords", "coords").  This means that such objects
###  are treated first and foremost as "vcoords" objects but can also
###  be treated as "coords" objects so that existing methods will
###  apply.
###
###  Some existing functionality, such as bounding boxes and length,
###  can be retained but some must be changed and new functionality
###  added.


###  A "vcoords" Constructor And Accessor
###
###  This is just like "coords" case, but an addition "v" component is
###  added to the object.  The existing "xcoords" and "ycoords"
###  accessors will still work so we only need to add a "value"
###  accessor to extract the values.

vcoords =
    function(x, y, v)
    {
        if (!is.numeric(x) || !is.numeric(y) ||
            !all(is.finite(x)) ||
            !all(is.finite(y)))
                stop("invalid coordinates")
        if(length(x) != length(y) ||
           length(x) != length(v))
            stop("argument lengths differ")
        pts = list(x = x, y = y, v = as.numeric(v))
        class(pts) = c("vcoords", "coords")
        pts
    }

values = function(obj) obj$v


###  Functions for replacing the "x", "y" and "values" values in
###  "coords" objects.
###
###  We don't really need to make the "value<-" method generic, but
###  will do so to make things symmetric.

`values<-` = function(x, value) UseMethod("values<-")

`xcoords<-.vcoords` =
    function(x, value) {
        if (!is.numeric(value) && length(value) != length(x))
            stop("invalid x replacement")
        vcoords(value, ycoords(x), values(x))
    }

`ycoords<-.vcoords` =
    function(x, value) {
        if (!is.numeric(value) && length(value) != length(x))
            stop("invalid y replacement")
        vcoords(xcoords(x), value, values(x))
    } 

`values<-.vcoords` =
    function(x, value) {
        if (!is.numeric(value) && length(value) != length(x))
            stop("invalid y replacement")
        vcoords(xcoords(x), ycoords(x), value)
    } 


###  A "print" Method for "vcoords" Objects
###
###  The existing print method for "coords" objects does not display
###  the values so we can't inherit that behaviour.  We define an
###  appropriate method specific to vcoords objects.

print.vcoords =
    function(obj)
    {
          print(paste("(",
                      format(xcoords(obj)),
                      ", ",
                      format(ycoords(obj)),
                      "; ",
                      format(values(obj)),
                      ")", sep = ""),
                quote = FALSE)
    }


###  Subsetting and Subset Replacement Methods
###
###  Defining these methods makes it possible to write expressions like
###      pts[i]
###  and
###      pts[i] = npts
###  All the standard ways of specifying subsets are supported as is
###  the the standard recycling mechanism.

`[.vcoords` = function(x, i)
    vcoords(xcoords(x)[i], ycoords(x)[i], values(x)[i])

`[<-.vcoords` =
    function(x, i, value) {
        if (!inherits(value, "vcoords"))
            stop("invalid right-hand side")
        xx = xcoords(x)
        xy = ycoords(x)
        xv = values(x)
        xx[i] = xcoords(value)
        xy[i] = ycoords(value)
        xv[i] = values(value)
        vcoords(xx, xy, xv)
    }

###  An "is.na" Method for "vcoords" Objects

is.na.vcoords = function(x) is.na(values(x))

###  Mathematical Methods for "vcoords" Objects
###
###  Since "vcoords" objects have numeric values it makes sense to
###  define mathematical transformations for them.  The transformation
###  is understood to apply just the value part of the objects.
###  Transformations are implemented by defining a group method for
###  the "Math" group.

Math.vcoords =
    function(x)
    vcoords(xcoords(x),
            ycoords(x),
            get(.Generic)(values(x)))

###  Arithmetic for "vcoords" Objects
###
###  We want to be able to carry out manipulations like
###      vpts / 2
###      1 / vpts
###      vpts1 + vpts2
###      vpts < 42
###  etc.  This can be done by defining a group method for the Ops
###  group.
###
###  When two "vcoords" objects are being operated on we will require
###  that the associated coordinates are the same.  This is defined
###  with helper function, but it could be done inline.
###
###  Note that 

sameloc =
    function(e1, e2)
    (length(values(e1)) == length(values(e2))
     && all(xcoords(e1) == xcoords(e2))
     && all(ycoords(e1) == ycoords(e2)))

Ops.vcoords =
    function(e1, e2) {
        if (missing(e2)) {
            x = xcoords(e1)
            y = ycoords(e1)
            v = get(.Generic)(values(e1))
        }
        else {
            is.vc1 = inherits(e1, "vcoords")
            is.vc2 = inherits(e2, "vcoords")
            if (is.vc1 && is.vc2) {
                if (!sameloc(e1, e1))
                    stop("different locations")
                x = xcoords(e1)
                y = ycoords(e1)
                v = get(.Generic)(values(e1),
                    values(e2))
            }
            else if (is.vc1) {
                if (length(e1) < length(e2))
                    stop("size mismatch")
                x = xcoords(e1)
                y = ycoords(e1)
                v = get(.Generic)(values(e1), e2)
            }
            else if (is.vc2) {
                if (length(e2) < length(e1))
                    stop("size mismatch")
                x = xcoords(e2)
                y = ycoords(e2)
                v = get(.Generic)(e1, values(e2))
            }
        }
        switch(.Generic,
               "!" =, "&" =, "|" =,
               "==" =, "!=" =,
               "<" =, "<=" =,
               ">" =, ">=" = v,
               vcoords(x, y, v))
    }


###  Some additional functionality that converts between "coords" and
###  "vcoords" objects.

as.coords = function(x) UseMethod("as.coords")
as.vcoords = function(x) UseMethod("as.vcoords")

as.coords.coords = function(x) x
as.vcoords.vcoords = function(x) x

as.coords.vcoords =
    function(x)
    coords(xcoords(x), ycoords(x))

as.vcoords.coords =
    function(x)
    vcoords(xcoords(x), ycoords(x), rep(NA, length(x)))


###  The following definition makes it possible to add values to an
###  existing "coords" object with a call of the form:
###      values(pts) = v
###  If "v" is too long, values are ignored and if "v" is too short
###  its values are recycled.

`values<-.coords` =
    function(x, value) {
        if (length(value) > length(x))
            warning("some values ignored")
        if (length(value) < length(x))
            warning("values recycled")
        vcoords(xcoords(x), ycoords(x), rep(value, length = length(x)))
    }
