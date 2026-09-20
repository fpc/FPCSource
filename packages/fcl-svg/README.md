# fcl-svg

SVG 1.1 parsing and rendering for the Free Pascal tree, with a swappable rendering backend. 

## What is drawn

A bird's-eye view of the specification, feature by feature. Everything
listed as drawn is exercised by the tests and measured against the W3C
suite; the gaps beside it are the ones `TODO.md` keeps, and the readings
behind any of them are further down this file.

### Features:

* `svg` :  nested to any depth, `g`, `defs`, `symbol`, `use`, `switch` with `requiredFeatures`, `requiredExtensions` and `systemLanguage`, `desc`, `title`, `metadata`, `.svgz` and `data:` URIs 
* `path` : with the whole `d` grammar and arcs, `rect` with corners, `circle`, `ellipse`, `line`, `polyline`, `polygon`, `image` 
* **Coordinates** :  `viewBox`, `preserveAspectRatio` with every alignment and `slice`, `transform` lists, nested viewports, `overflow`, `clip`
* **Painting** : fill and stroke with every `fill-rule`, `stroke-linecap`, `stroke-linejoin`, `stroke-miterlimit`, dashes, `opacity` on a group, `display`, `visibility`, `color` and `currentColor` 
* **Paint servers** : `linearGradient`, `radialGradient` with every `spreadMethod`, both unit spaces, `gradientTransform`, stops inherited through `href`, `pattern` with `patternTransform` and `viewBox`
* **Clipping** : `clipPath` with `clip-rule` and both unit spaces, `mask` with luminance and alpha 
* **Markers** : `marker-start`, `marker-mid`, `marker-end`, `orient="auto"` and an angle, `markerUnits`, `refX`/`refY`, `overflow` on the marker 
* **Text** : `text`, `tspan`, `tref`, `textPath`, `altGlyph`, position and rotation lists, `text-anchor`, `text-decoration`, `dominant-baseline`, `alignment-baseline`, `baseline-shift`, `letter-spacing`, `word-spacing`, `writing-mode: tb`, `glyph-orientation-vertical` 
* **Fonts** : SVG fonts (`font`, `font-face`, `glyph`, `missing-glyph`, `hkern`, `vkern`, `font-face-uri`), system faces through freetype, `@font-face` with `src: url(...)`, matching on family, slant, variant and weight across a family list 
* **Filters** : every SVG 1.1 primitive: `feBlend`, `feColorMatrix`, `feComponentTransfer`, `feComposite`, `feConvolveMatrix`, `feDiffuseLighting`, `feDisplacementMap`, `feFlood`, `feGaussianBlur`, `feImage`, `feMerge`, `feMorphology`, `feOffset`, `feSpecularLighting`, `feTile`, `feTurbulence`, the three light sources, both unit spaces, `color-interpolation-filters` 
* **Style** : presentation attributes, `style` attributes, `style` elements through the FPC CSS parser with selectors and `@import`, `!important`, inheritance and `inherit` 
* **Animation** : SMIL: `set`, `animate`, `animateColor`, `animateTransform`, `animateMotion` with `mpath`, every `calcMode`, `keyTimes`, `keySplines`, `keyPoints`, `additive`, `accumulate`, the whole of interval timing, and syncbase and wallclock `begin` lists 
* **Linking** : `a`, `view`, `#name` and `#svgView(viewBox(...);preserveAspectRatio(...);transform(...))` fragments, references into a second document 

### Not (yet) implemented:
* right-to-left writing and the bidi algorithm, complex-script shaping
* `foreignObject`, `cursor`, `color-profile`
* **Scripting** : `script`, event attributes, the DOM 
* **Linking**: `zoomAndPan`, `viewTarget` 
* **Interactive**: timing that waits on a reader, which is interactivity
* **Filters**: the accumulation half of `enable-background` |
* **Fonts**' the `kerning` property, synthetic bold and oblique 


The backend interface takes user-space geometry and a matrix, so a backend that draws none of clipping, masking, group opacity, 
patterns, dashes or filters still renders every document: `fpsvg.render` works out what the backend says it cannot do. 
The current `TSVGSoftBackend` does all of it.

## Rendering a document

```pascal
uses fpsvg.dom, fpsvg.read, fpsvg.render, fpsvg.soft;

var
  lDocument: TSVGDocument;
  lBackend: TSVGSoftBackend;

begin
  lDocument := ReadSVGFile('drawing.svg');
  lBackend := TSVGSoftBackend.Create;
  try
    RenderSVGDocument(lDocument, lBackend);
    // lBackend.Image now holds the pixels.
  finally
    lBackend.Free;
    lDocument.Free;
  end;
end;
```

## Looking for external files:
The `<image>` elements need a resolver, which not implemented in the
renderer itself. Instead, you must give the renderer a file resolver, 
and link the `fcl-image` readers for the formats to accept. For example, if
you want to allow it to read PNG or JPEG, you'd add the following:

```pascal
uses fpreadpng, fpreadjpeg;

lRenderer.Images := TSVGFileImageResolver.Create(GetCurrentDir);
```

A reference naming a second file (a `use` naming an element in another
document, or an `image` naming a whole one) also needs a document resolver.
That there is no default resolver is deliberate safety feature.

To assign a document resolver, you'd add the following:

```pascal
lRenderer.Documents := TSVGFileDocumentResolver.Create(GetCurrentDir);
```

This resolver reads each file and caches it.

## Font support

Similarly, text needs a font provider, and each one lives in a unit of 
its own so that only a program wanting text takes on the dependency:

```pascal
uses fpsvg.fonts.provider;

lRenderer.Fonts := SVGPlatformFontProvider;
```

`SVGPlatformFontProvider` is the provider of the platform being built for,
created on first use and owned by `fpsvg.fonts.provider`. 
On macOS that is Core Text, which is part of the system; 
On Windows that is the GDI unit, which is also part of the system.
Everywhere else it is the freetype lib. You can force the use of the
freetype lib on all platforms using the `SVGFREETYPE` define.

The freetype support detects the the system font directories and the coverage 
source of `fpsvg.fonts.support`. `SVGPlatformFontEngine` tells you which
provider is in use of the two it is,

A program that wants the provider itself, to set its properties or to hold
more than one, addss the unit it wants:

| unit | draws with | needs |
| --- | --- | --- |
| `fpsvg.freetype` | freetype | `freetypeh`, and libfreetype at run time |
| `fpsvg.coretext` | Core Text | the `univint` package, macOS only |
| `fpsvg.gdi` | the GDI font calls | nothing beyond the RTL, Windows only |

```pascal
uses fpsvg.freetype;

lFonts := TSVGFreeTypeProvider.Create;
lFonts.AddSystemFonts;
lRenderer.Fonts := lFonts;
```

`TSVGPlatformFontProvider` of `fpsvg.fonts.provider` is an alias of whichever
class the target has, for a program that wants its own instance without
naming a platform.

Nothing else in the package uses either engine. A program that uses neither
unit links no freetype symbol and no framework, and every non-text path
works without them.

### Text on macOS and Windows without freetype

`fpsvg.coretext` resolves faces, metrics and outlines through Core Text, so
a Mac needs nothing installed: no Homebrew, no libfreetype. It is the
default there, and what it draws differs from the freetype path in three
places:

* Ligatures are not formed, Core Text building those while it lays out a line.
* Kerning pairs are not applied, Core Text offers no lookup for them, which matches `TSVGFreeTypeProvider.Kerning` being False by default; 
* a bold is never synthesised, a family without one resolving to the nearest weight Core Text has. 
* An italic a family lacks is leaned by hand, just as the freetype support leans it.

A font a `@font-face` rule selects is handed to Core Text for the life of the process, 
under the family the document specifies, so a document that declares its own faces draws with them either way.

`fpsvg.gdi` does the same for Windows, through the GDI font calls.
It differs from the freetype path in the same places: 

* no ligatures, GDI having no shaping of its own
* no glyph names. 

Two things it does better than Core Text: 

* GDI synthesises a bold as well as an italic.
* its kerning pairs are readable, so `TSVGGDIProvider.Kerning` works as the freetype one but is similarly off by default. 

Two things it does less well: 

* a character above U+FFFF has no glyph, GDI mapping one UTF-16 unit at a time
* Vertical text falls back to the ascent rather than reading the vertical face.

`drawsvg` takes `--font-engine=coretext`, `--font-engine=gdi` and
`--font-engine=freetype`, so the same document can be drawn each way and the
two pictures compared:

```
./build/drawsvg --font-engine=coretext doc.svg --output=ct.png
./build/drawsvg --font-engine=freetype doc.svg --output=ft.png
```

The `svgdiff` program takes the same switch, which measures a whole suite one engine at a time; 
`tests/README` says how far the two runs can be compared.

Registering a font file opens its face to read the family name and closes it again.
The face is opened for good only once a request actually decides that file must be used. 

`AddSystemFonts` finds some 2500 files on a desktop, so beware:
holding them all open costs a few hundred megabytes for a document that may draw no text at all.

`TSVGRenderer.Render` uses the size the document asks for. 

`RenderToFit` scales a document into a frame of the caller's choosing without changing its shape, 
`RenderToSize` fills the frame and stretches to do it, and `RenderInFrame` draws into a frame 
the caller has already begun drawing on (i.e. does not clear whatever is there). 


## Drawing one view of a document

A `<view>` element selects a framing — a `viewBox`, a `preserveAspectRatio`, or
both: they must be part of the document. An icon sheet declares one per icon, a plan one per region. 

SVG activates them through the fragment of a URL, `icons.svg#home`, 
and nothing here resolves URLs, so the caller must pass the fragment in:

```pascal
uses fpsvg.read, fpsvg.render;

if SVGViewOfFragment(lDocument, '#home', lView) then
  lRenderer.View := lView;
lRenderer.RenderToFit(lDocument, lBackend, 64, 64);
```

`SVGViewOfFragment` reads both spellings: 

* the name of a `view` element
* and the inline `#svgView(viewBox(0,0,50,50);preserveAspectRatio(xMinYMin))`. 

A fragment naming any other element finds no view and reframes nothing, 
which is conform the SVG 1.1 standard. 

The percent escapes of a URL are read first, so `viewBox(...)%3Btransform(...)` 
separates its parts the way a literal semicolon does. 

A `transform(...)` part moves what the view frames: 

it goes under the `viewBox`, so a `translate` in it counts in the units the view frames rather than in the ones on the page. 

Only a fragment gives one; 

a `view` element has no attribute for it. 

The `SVGViewsOf` method gives every view a document declares, in document order, for a program that wants to offer them:

```pascal
for lView in SVGViewsOf(lDocument) do
  WriteLn(lView.Name);
```

The view is a value the renderer gets, so it does not affect the document.
setting `TSVGRenderer.View` frames the root by it, and `TSVGView.None` puts 
the root back to its own attributes.

So one document can be rendered with different views in turn. 
A view with `HasRatio=False` leaves the `preserveAspectRatio` of the root alone.
A view with `HasViewBox=False` leaves its `viewBox` alone.

Only the root is framed. 

A nested `svg` keeps its own `viewBox`, and `zoomAndPan` and `viewTarget` are for an interactive viewer and are read past.

## Animating a document

The SVG elements `<set>`, `<animate>`, `<animateColor>`, `<animateTransform>` and
`<animateMotion>` specify how an attribute changes over time.

If a document contains such elements, they are read by `TSVGTimeline`. 
You can use `Seek` to set a timestamp for the frame you wish to render, and
then do an ordinary render:

```pascal
uses fpsvg.anim, fpsvg.render;

lTimeline := TSVGTimeline.Create(lDocument);
try
  if lTimeline.IsAnimated then
    lTimeline.Seek(1.5);
  lRenderer.Render(lDocument, lBackend);
  lTimeline.Reset;
finally
  lTimeline.Free;
end;
```

The values go into the attributes of the tree itself, which is why the
cascade, the render walk and every backend need to know nothing about a
clock. 

The `Reset` method resets the value of each attribute to its initial state, 
and empties them if they were not initially specified. So the document
resets to its initial state. 

It is important to build the timeline before the first `Seek`, 
because that is where the base values are read.

* `Seek` is idempotent and takes any time in any order.
* `Duration` is where the last animation ends. Set it to a negative value and animation runs infinite.
* `ChangeTimes` gives the moments at which anything changes, for an encoder that would rather write one frame per change than one per tick.

The timeline reads `begin` and `end` as lists of offsets or of times taken
from another animation, `dur`, `repeatCount`, `repeatDur`, `min`, `max`,
`restart`, `fill="freeze"`, all four `calcMode`s with `keyTimes` and
`keySplines`, `values`, `from`, `to` and `by`, and `additive` and
`accumulate`. 

Numbers, lengths, colours, lists of either, and the `d` of a path interpolate; 
anything else steps at the half way point of an interval. 
Using `SVGValueKindOf` you can determine which attribute is which. 

A length attribute holding a list (so for example the `x`, `y`, `dx` and `dy` of a text
element), interpolates when the two ends hold as many entries and each stands in the 
unit its counterpart does; so does the `rotate` of one.

An `attributeName` written with the XLink prefix names the attribute the
reader keeps under its local name, so `attributeName="xlink:href"` animates
the `href` an `image`, a `use` or an `a` is drawn from.

`currentColor` as a value of an animation is read as the colour the keyword
stands for on the target at that moment: the nearest `color` an ancestor
states, as an attribute or in its inline style, and black where none of them
does. 

The function `SVGCurrentColorOf` determines this color, and `Seek` writes every 
animation of `color` before any other (the cascade computes that property first, so an
animation using the keyword follows a `color` animated beside it). 

A `color` that only a style sheet sets is not seen here: working that out is the
cascade, which runs above the timeline. No document of the W3C suite sets it that way.

The `<number>` of a filter primitive interpolates beside the properties
that have one: `limitingConeAngle`, `specularExponent`, `surfaceScale`,
`diffuseConstant`, `specularConstant`, `azimuth`, `elevation`, the
`pointsAt` of a spot, `k1` to `k4`, `divisor`, `bias`, `scale`, `slope`,
`intercept`, `amplitude`, `exponent`, `seed` and `z`. 

The ones written as one number or two, `stdDeviation`, `baseFrequency`, `radius`, `order`,
`kernelUnitLength` and `filterRes`, are read as lists of numbers, as are
`tableValues`, `kernelMatrix` and the `values` of an `feColorMatrix`. 

The `<integer>` attributes step at the half way point instead

`begin="logo.end + 2s"` waits on another animation, which `TSVGTimeline`
works out when it reads the document, so `Seek` does no resolving of its
own. `logo.repeat(2)` waits on the third run of one. 

An animation that happens outside the clock is left out of the timeline 
altogether, and its attribute keeps the value the file was read with.

A `begin` can also be a timestamp (a so-called "real time"). 
To cater for this, you can set the start time of animations when creating the timeline:

```pascal
lTimeline := TSVGTimeline.Create(lDocument, Now);
```
Such timestamps will be checked as relative to the start time:

`wallclock(2000-06-10T12:00:00Z)` is then two seconds in for a document
whose clock started at 11:59:58 of that day. 

Without a start time set during creation, the timeline will simply ignore timestamps.

`calcMode="paced"` spaces the values by the distance between them, 
which is measured over the numbers a value holds: one for a number or a length,
the three channels of a colour, and every number of a list, the arguments
of a transform among them. 

`calcMode="spline"` eases each interval by the cubic Bézier that `keySplines` 
gives for it. A value with no distance to measure, and a spline without a curve 
for every interval, are run as `linear` instead.

`<animateTransform>` writes the function its `type` selects, so a `rotate`
of `45` reaches the document as `rotate(45 0 0)` and the render walk
reads it as any other `transform`. The arguments are filled out to the
count the function takes, which is what lets `from="1"` interpolate with
`to="0.5 1"` on a `scale`. `<animateColor>` is `<animate>` with every
value read as a colour.

`<animateMotion>` moves the element along the path of an `<mpath>` child
or of its `path` attribute, or between the points of its `values`, and
`rotate="auto"` turns the element with the path. It also reaches the
document as a `transform`, written before the one the element was read
with. `keyPoints` places each key time at a fraction of the length of the
path.

Timing on an event is a non-goal rather than a gap: an animation whose
`begin` contains `mouseover` never begins. `TODO.md` holds the one departure
that is left, which is the interval a syncbase selects.

## The messages

The package writes no messages to the console. 
If there is an error, an exception is raised.

Every exception message is a resourcestring in `fpsvg.strings`, 
so a program can translate them the way any FPC resourcestring is translated. 

## A drawing program to test with:

`examples/drawsvg.pp` renders a document to a raster image.

```
drawsvg --size=512x512 drawing.svg --output=drawing.png
```

Some characteristics:

* The extension of the output selects the format. 
* Without `--size` the document is rendered at the size it asks for, and without `--output` the result goes beside the document as a PNG. 
* A `--size` whose shape differs from the document's scales the drawing to fit and centres it, leaving the spare room empty; 
* `--stretch` fills the size instead. 
* `--background=white` paints onto a colour first. 
  This may change the result: The surface is transparent, and a viewer showing transparency as black hides black text and a black frame completely. 
  A format with no alpha channel takes white on its own.
* A PNG written without `--background` keeps its alpha channel, so the parts of the surface no shape reached stay transparent. 
* Note that `TFPWriterPNG` writes no alpha unless its `UseAlpha` is set, and that a surface is transparent black where nothing was painted: 
  a program leaving the property alone gets an opaque black ground rather than a transparent one.
* Fonts come from the system unless you specify a directory with `--fonts`.
* The `--no-fonts` renders no texts. 
* Images and referenced documents resolve beside the document being rendered.
* `--view=NAME` frames the drawing by one of the views the document declares, as a link to that name would. 
  It takes a leading `#`, or an `svgView(viewBox(...))` instead of a name:

```
./build/drawsvg --size=64x64 --view=home icons.svg --output=home.png
```

* If the view name does not exist in the document, you'll get an error, and the message will say which views do exist.

* `examples/svggallery.pp` draws a whole directory instead of one document and
  creates a HTML page with every drawing in the page beside the document it came from, 
  so you can compare what the browser does, and what fcl-SVG does.

```
svggallery examples/SVG build/gallery
```

Each image is drawn at its default size, clamped to 640 pixels (hor/ver).
If an image contains an error, it is reported on the page rather than stopping the run. 

Both pictures of a pair stand on a chequered background, to show transparency.

