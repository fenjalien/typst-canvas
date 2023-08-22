#import "matrix.typ"
#import "vector.typ"
#import "draw.typ"
#import "cmd.typ"
#import "util.typ"
#import "coordinate.typ"
#import "styles.typ"
#import "path-util.typ"
#import "@preview/oxifmt:0.2.0": strfmt

// Compute bounding box of points
#let bounding-box(pts, init: none) = {
  let bounds = init
  if type(pts) == "array" {
    for (i, pt) in pts.enumerate() {
      if init == none and i == 0 {
        bounds = (l: pt.at(0), r: pt.at(0), t: pt.at(1), b: pt.at(1))
      }
      bounds.l = calc.min(bounds.l, pt.at(0))
      bounds.r = calc.max(bounds.r, pt.at(0))
      bounds.t = calc.min(bounds.t, pt.at(1))
      bounds.b = calc.max(bounds.b, pt.at(1))
    }
    } else if type(pts) == "dictionary" {
      if init == none {
        bounds = pts
      } else {
        bounds.l = calc.min(bounds.l, pts.l)
        bounds.r = calc.max(bounds.r, pts.r)
        bounds.t = calc.min(bounds.t, pts.t)
        bounds.b = calc.max(bounds.b, pts.b)
      }
    } else {
      panic("Expected array of vectors or bbox dictionary, got: " + repr(pts))
    }
  return bounds
}

#let draw-types = ("circle",)


#let process-element(element, ctx) = {
  let drawables = ()
  let bounds = none

  if element.type == "style" {

  }

  if element.type in draw-types and (ctx.style-update != none or ctx.transform-update) {

  }

  let coordinates = (if "coordinates" in element {
    for c in element.coordinates {
      c = coordinate.resolve(ctx, c)
      // if the first element is `false` don't update the previous point
      if c.first() == false {
        // the format here is `(false, x, y, z)` so get rid of the boolean
        c = c.slice(1)
      } else {
        ctx.prev.pt = c
      }

      c
    }
  },)

  if element.type == "circle" {
    let style = styles.resolve(ctx.style, element.style, root: "circle")
    let (cx, cy, z) = coordinates.first()
    let (r, _) = util.resolve-radius(style.radius).map(util.resolve-number.with(ctx))
    drawables.push(strfmt("<circle cx='{}' cy='{}' r='{}' style='{}'/>", cx, cy, r, styles.to-svg(style)))
    if ctx.debug {
      drawables.push(strfmt("<rect x='{}' y='{}' width='{}' height='{}' fill='none' stroke='red'/>", cx - r, cy - r, r*2, r*2))
    }
    bounds = bounding-box(
      ((cx - r, cy - r, z), (cx + r, cy + r, z)).map(util.apply-transform.with(ctx.transform)), 
      init: bounds
    )
  }

  return (bounds: bounds, ctx: ctx, drawables: drawables)
}


#let canvas(length: 1cm,        /* Length of 1.0 canvas units */
            background: none,   /* Background paint */
            debug: false, body) = layout(ly => style(st => {
  if body == none {
    return []
  }

  let length = length
  assert(type(length) in ("length", "ratio"),
         message: "length: Expected length, got " + type(length) + ".")
  if type(length) == "ratio" {
    // NOTE: Ratio length is based on width!
    length = ly.width * length
  } else {
    // HACK: To convert em sizes to absolute sizes, we
    //       measure a rect of that size.
    length = measure(line(length: length), st).width
  }

  // Canvas bounds
  let bounds = none

  // Canvas context object
  let ctx = (
    typst-style: st,
    length: length,

    debug: debug,

    // Previous element position & bbox
    prev: (pt: (0, 0, 0)),

    // Current content padding size (added around content boxes)
    content-padding: 0em,

    em-size: measure(box(width: 1em, height: 1em), st),

    style: styles.default,
    style-update: styles.default,

    // Current transform
    transform: matrix.mul-mat(
      matrix.transform-shear-z(.5),
      matrix.transform-scale((x: 1, y: -1, z: 1)),
    ),
    transform-update: true,
    // transform: matrix.transform-scale((x: 1, y: -1, z: 1)),

    // Nodes, stores anchors and paths
    nodes: (:),

    // group stack
    groups: (),
  )
  
  let draw-cmds = ()
  let boundaries = ()
  for element in body {
    let r = process-element(element, ctx)
    if r != none {
      if r.bounds != none {
        bounds = bounding-box(r.bounds, init: bounds)
      }
      boundaries.push(r.bounds)
      ctx = r.ctx
      draw-cmds += r.drawables
    }

  }

  if bounds == none {
    return []
  }
  // Final canvas size
  let width = calc.abs(bounds.r - bounds.l)
  let height = calc.abs(bounds.t - bounds.b)
  
  // Offset all element by canvas grow to the bottom/left
  let transform = matrix.transform-translate(
    -bounds.l, 
    -bounds.t, 
    0
  )

  let size = length / 1pt

  let str = {
      strfmt("<svg viewBox='{} {} {} {}' xmlns='http://www.w3.org/2000/svg'>", bounds.l, bounds.t, width, height)
      strfmt("<g stroke-width='{}' transform='{}'>", 1/size, matrix.to-svg(ctx.transform))
      // strfmt("<svg viewBox='{} {} {} {}' xmlns='http://www.w3.org/2000/svg'>", bounds.l*size, bounds.t*size, width*size, height*size)
      // strfmt("<g transform='scale({})' stroke-width='1pt'>", size, 1/size)
      // strfmt("<g transform='scale(1 -1)'>")//width/2, -height)
      // strfmt("<g transform='scale(1 -1) translate({} {})'>", 0, -5)//width/2, -height)
      for d in draw-cmds {
        d
      }

      "</g>"
      // "<circle cx='5' cy='5' r='1'/>"
      "</svg>"
    }
  str 
  linebreak()
  box(
    stroke: if debug {green},
    width: width * length,
    height: height * length,
    fill: background,
    {
      image.decode(str)
      place(line())
    }
    // align(
    //   top,
    //   for d in draw-cmds {
    //     d.segments = d.segments.map(s => {
    //       return (s.at(0),) + s.slice(1).map(v => {
    //         return util.apply-transform(transform, v)
    //           .slice(0,2).map(x => ctx.length * x)
    //       })
    //     })
    //     (d.draw)(d)
    //   }
    // )
  )
}))
