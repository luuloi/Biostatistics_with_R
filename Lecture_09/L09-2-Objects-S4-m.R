###  The Joy of Multiple Inheritance
###
###  The point of this code module is to show how multiple inheritance
###  makes some things particularly easy.  We define a "coords" class
###  which holds x-y coordinates and then define a "vcoords" class
###  which inherits from both the "numeric" and "coords" classes.
###  This means that value properties can be inherited directly from
###  "numeric" and do not need to be implemented.  Hence there is no
###  need to define methods for "Math", "Math2", "Arith" and
###  "Compare".  Better yet, functions like is.na, is.finite, etc.
###  are immediately inherited from the "numeric" superclass.  This
###  makes the implementation of the class methods much simpler than
###  if a values slot was added.
###
###
###  The "coords" Class
###
###  Objects from this class represent locations in the x-y plane.
###  They are created with the "coords" constructor function and the
###  coordinates can be extracted with the "xcoords" and "ycoords"
###  accessor functions.
###
###  Suitable "print" and "length" methods are defined.
###
###  Functions which provide a means of changing the "x" and "y"
###  values within "coords" objects are defined.
###
###  Functions which make it possible to extract and replace subsets
###  of coords objects are provide.
###
###  A new generic function called "bbox" which obtains bounding boxes
###  is defined as is a method for coords objects.


###  The "coords" Class Definition

setClass("coords",
         representation(x = "numeric",
                        y = "numeric"))


###  The "coords" constructor and accessor functions.
###
###  These functions define how "coords" objects are created and their
###  component parts accessed.  All functions which maniplate this
###  kind of object should work by calling these functions rather than
###  using the low-level representation.  This makes it possible to
###  change the representation of the objects and to have all the
###  software which manipulates this kind of object continue to work
###  without change.
###
###  To get full generality the accessor function should be made
###  generic. This would make it possible to use xcoords and ycoords
###  in a different way in other situations.
###
###  Actually, the coords function should be generic, so that it can
###  be used as an accessor for the "vcoords" case. (See Examples).

coords =
    function(x, y) {
        if (length(x) != length(y))
            stop("equal length x and y required")
        if (!is.numeric(x) || !is.numeric(y) ||
            !all(is.finite(x)) ||
            !all(is.finite(y)))
            stop("invalid coordinates")
        new("coords",
            x = as.vector(x),
            y = as.vector(y))
    }

xcoords = function(object) object@x
ycoords = function(object) object@y


###  "show" and "length" method for "coords" Objects
###
###  Note that the "length" function predates the existence of the
###  object systems in R, so its formal argument is called "x",
###  whereas the "show" method is part of the S4 object system and has
###  a formal object called "object".

setMethod(show, signature(object = "coords"),
          function(object)
          print(paste("(",
                      format(xcoords(object)),
                      ", ",
                      format(ycoords(object)),
                      ")", sep = ""),
                quote = FALSE))

setMethod(length, signature(x = "coords"),
          function(x)
          length(xcoords(x)))


###  Bounding Box Generic and "coords" Method.
###
###  Here I've chosen to make the argument to the generic be called
###  "object" to match other generic functions.
###
###  The bounding box for a "coords" object is itself a "coords"
###  object containing the diagonally opposite corners of the bounding
###  box.

setGeneric("bbox",
           function(object)
           standardGeneric("bbox"))

setMethod("bbox", signature(object = "coords"),
          function(object)
          coords(x = range(xcoords(object)),
                 y = range(ycoords(object))))


###  Replacement for "xcoords" and "ycoords"
###
###  These are defined as generic functions. This is partly just to
###  show how it can be done, but it will also be useful when we
###  define subclasses of the coords class.

setGeneric("xcoords<-", function(object, value) standardGeneric("xcoords<-"))
setGeneric("ycoords<-", function(object, value) standardGeneric("ycoords<-"))

setMethod("xcoords<-", signature(object = "coords", value = "numeric"),
          function(object, value) {
              value = rep(value, length = length(object))
              coords(value, ycoords(object))
          })

setMethod("ycoords<-", signature(object = "coords", value = "numeric"),
          function(object, value) {
              y = ycoords(object)
              value = rep(value, length = length(object))
              coords(xcoords(object), value)
          })                 

###  Subsetting for "coords" Objects
###
###  These definitions make it possible to use expressions like
###      pts[i]
###      pts[i] = npts

setMethod("[", signature(x = "coords", i = "ANY",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop)
          coords(xcoords(x)[i], ycoords(x)[i]))

setMethod("[<-", signature(x = "coords", i = "ANY",
                           j = "missing", value = "coords"),
          function(x, i, j, value) {
              xx = xcoords(x)
              yy = ycoords(x)
              xx[i] = xcoords(value)
              yy[i] = ycoords(value)
              coords(xx, yy)
          })



###  The "vcoords" Class
###
###  The "vcoords" class inherits from "numeric" for its values and
###  "coords" for its locations.  This means that methods appropriate
###  for numerical manipulation of the associate values are simply
###  inherited rather than needing to be implemented.

setClass("vcoords", contains = c("numeric", "coords"))

###  A "vcoords" constructor and accessor for the new "slot".

vcoords =
    function(x, y, value)
    {
        if (length(x) != length(value) ||
            length(y) != length(value) ||
            !is.numeric(x) || !is.numeric(y) ||
            !all(is.finite(x) & is.finite(y)))
                stop("invalid arguments")
        new("vcoords", as.numeric(value), x = x, y = y)
    }

values = function(object) as.numeric(object)

setGeneric("values<-",
           function(object, value)
           standardGeneric("values<-"))

setMethod("values<-",
          signature(object = "vcoords", value = "numeric"),
          function(object, value)
          vcoords(xcoords(object), ycoords(object),
                  rep(value, length = length(object))))

### A "show" Method for "vcoords" Objects.

setMethod(show, signature(object = "vcoords"),
          function(object)
          print(paste("(",
                      format(xcoords(object)),
                      ", ",
                      format(ycoords(object)),
                      "; ",
                      format(values(object)),
                      ")", sep = ""),
                quote = FALSE))

###  Could mention at this point that we already have Math, Math2,
###  Arith and Compare by inheritance.

###  Subsetting and Subset Replacement for "vcoords" Objects.

setMethod("[", signature(x = "vcoords", i = "ANY",
                         j = "missing", drop = "missing"),
          function(x, i, j, drop)
          vcoords(xcoords(x)[i],
                  ycoords(x)[i],
                  values(x)[i]))

setMethod("[<-", signature(x = "vcoords", i = "ANY",
                         j = "missing", value = "vcoords"),
          function(x, i, value) {
              xx = xcoords(x)
              xy = ycoords(x)
              xv = values(x)
              xx[i] = xcoords(value)
              xy[i] = ycoords(value)
              xv[i] = values(value)
              vcoords(xx, xy, xv)
          })

###  Multiple inheritance gives us all mathematical transformations
###  and binary operations for the cases:
###     numeric + vcoords
###     vcoords + numeric
###  Binary operations are also defined in the
###     vcoords + coords
###  case, but without a check for equality of locations.  We have to
###  define the Arith, and Compare operators by hand in this case.
###  Notice that we can save a small amount of time here by calling
###  "callNextMethod" rather than "callGeneric" to avoid a small
###  amount of dispatch overhead.

###  A Check for Location Equality

sameloc =
    function(e1, e2)
    (length(values(e1)) == length(values(e2))
    && all(xcoords(e1) == xcoords(e2))
    && all(ycoords(e1) == ycoords(e2)))

###  Arith and Compare Group Methods

setMethod("Arith", signature(e1 = "vcoords",
                             e2 = "vcoords"),
    function(e1, e2)
    {
        if (!sameloc(e1, e2))
            stop("identical locations required")
        vcoords(xcoords(e1),
                ycoords(e1),
                callNextMethod(values(e1),
                               values(e2)))
    })

setMethod("Compare", signature(e1 = "vcoords",
                             e2 = "vcoords"),
    function(e1, e2)
    {
        if (!sameloc(e1, e2))
            stop("identical locations required")
        callNextMethod(values(e1), values(e2))
    })

###  And that's all we need to implement!
###
###  Just for fun, here's a way of converting a "coords" object to a
###  "vcoords" object by adding values to it.

setMethod("values<-", signature(object = "coords", value = "numeric"),
          function(object, value)
          vcoords(xcoords(object), ycoords(object),
                  rep(value, length = length(object))))

###  This allows coersion of a "coords" object to a "vcoords" object

setAs("coords", "vcoords",
      function(from)
      vcoords(xcoords(from), ycoords(from),
              rep(NA, length(from))))
