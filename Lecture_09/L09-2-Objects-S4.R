###  The "ccords" Class
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
                quote = FALSE)

setMethod(length, signature(x = "coords"),
          function(x)
          length(xcoords(x)))

###  Bounding Box Generic and "coords" Method

setGeneric("bbox",
           function(object)
           standardGeneric("bbox"))

setMethod("bbox", signature(object = "coords"),
          function(object)
          coords(range(xcoords(object)), range(ycoords(object)))

###  Replacement for "xcoords" and "ycoords"
###
###  These are defined as generic functions. This is really just to
###  show how it can be done. It would also have been possible to
###  define simple functions to do the job.

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
              coords(xcoords(object),value)
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



###  The "vccords" Class
###
###  This class adds a value slot to the "coords" class to create a
###  "vccords" class.  Here is the class definition.

setClass("vcoords",
         representation(value = "numeric"),
         contains = "coords")


###  A "vcoords" Constructor and Accessor for the new slot.

vcoords =
    function(x, y, value)
    {
        if (!is.numeric(x) ||
            !is.numeric(y) ||
            !is.numeric(value) ||
            length(x) != length(value) ||
            length(y) != length(value))
                stop("invalid arguments")
        new("vcoords", x = x, y = y, value = value)
    }


values = function(object) object@value

###  A Replacement Generic and Method for "values"

setGeneric("values<-", function(object, value) standardGeneric("values<-"))

setMethod("values<-", signature(object = "coords", value = "numeric"),
          function(object, value) {
              value = rep(value, length = length(object))
              vcoords(xcoords(object), ycoords(object), value)
          })

###  A "show" Method for "vcoords" Objects.

setMethod(show, signature(object = "vcoords"),
          function(object)
          print("(",
                format(xcoords(object)),
                ", ",
                format(ycoords(object)),
                "; ",
                format(values(object)),
                ")", sep = ""),
          quote = FALSE)


###  Math Group Method

setMethod("Math", signature(x = "vcoords"),
          function(x)
          vcoords(xcoords(x),
                  ycoords(x),
                  callGeneric(values(x))))



###  A Check for Location Equality

sameloc =
    function(e1, e2)
    (length(values(e1)) == length(values(e2))
    && all(xcoords(e1) == xcoords(e2))
    && all(ycoords(e1) == ycoords(e2)))


###  Arith Group Methods
###
###  We need three separate methods for: vcoords/vcoords,
###  numeric/vcoords and vcoords/numeric. This is a pain in the
###  posterior, but when numeric values are included as slots and we
###  want arithmetic defined on them, this is the only option.

setMethod("Arith", signature(e1 = "vcoords",
                             e2 = "vcoords"),
    function(e1, e2)
    {
        if (!sameloc(e1, e2))
            stop("identical locations required")
        vcoords(xcoords(e1),
                ycoords(e1),
                callGeneric(values(e1),
                            values(e2)))
    })

setMethod("Arith",
    signature(e1 = "numeric",
              e2 = "vcoords"),
   function(e1, e2) {
       if (length(e1) > length(values(e2)))
           stop("incompatible lengths")
       vcoords(xcoords(e2),
               ycoords(e2),
               callGeneric(as.vector(e1),
                           values(e2)))
          })

setMethod("Arith",
    signature(e1 = "vcoords",
              e2 = "numeric"),
   function(e1, e2) {
       if (length(values(e1)) < length(e2))
           stop("incompatible lengths")
       vcoords(xcoords(e1),
               ycoords(e1),
               callGeneric(values(e1),
                           as.vector(e2)))
          })


### Compare Group Methods
###
###  Again, we need three separate methods for: vcoords/vcoords,
###  numeric/vcoords and vcoords/numeric.

setMethod("Compare", signature(e1 = "vcoords",
                               e2 = "vcoords"),
    function(e1, e2)
    {
        if (!sameloc(e1, e2))
            stop("identical locations required")
        callGeneric(values(e1), values(e2))
    })

setMethod("Compare", signature(e1 = "numeric",
                               e2 = "vcoords"),
    function(e1, e2)
    {
       if (length(e1) > length(values(e2)))
           stop("incompatible lengths")
        callGeneric(e1, values(e2))
    })

setMethod("Compare", signature(e1 = "vcoords",
                               e2 = "numeric"),
    function(e1, e2)
    {
       if (length(values(e1)) < length(e2))
           stop("incompatible lengths")
        callGeneric(values(e1), e2)
    })


###  Subsetting Methods for "vcoords" Objects
###
###  The S4 generic function for subsetting is defined with an
###  argument list:
###      (x, i, j, ..., drop)
###  The replacement method has argument list
###      (x, i, j, value)

###  In the case of "coords" objects we won't be using "j" or "drop"
###  so we include them in the signature with the class "missing".
###  This says that those arguments must be absent from any call to
###  these methods for "coords" objects.

setMethod("[",
          signature(x = "vcoords", i = "ANY",
                    j = "missing", drop = "missing"),
          function(x, i, j, drop)
          vcoords(xcoords(x)[i],
                  ycoords(x)[i],
                  values(x)[i]))

setReplaceMethod("[", signature(x = "vcoords", i = "ANY",
                                j = "missing", value = "vcoords"),
                 function(x, i, j, value) {
                     xx = xcoords(x)
                     xy = ycoords(x)
                     xv = values(x)
                     xx[i] = xcoords(value)
                     xy[i] = ycoords(value)
                     xv[i] = values(value)
                     vcoords(xx, xy, xv)
                 })
