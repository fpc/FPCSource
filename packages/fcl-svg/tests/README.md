# fcl-svg tests

## Running

```
fpc -Mobjfpc -Sh -Sc -Fusrc -Futests -Fu../../fcl-css/src -FEbuild tests/runtests.pp
./build/runtests -a
```

Run from the package root: 
the golden files and test documents are resolved relative to the working directory. 

The suites are:
* `types` for the geometry primitives
* `read` for the attribute grammars, 
* `dom` for the tree and the XML conversion, 
* `path` for the path grammar, arcs and shapes, 
* `shapes` for the shape elements of a parsed document, 
* `style` for colour and the property model, 
* `cascade` for the CSS cascade and computed values, 
* `use` for use expansion and cloning, 
* `geom` for flattening, 
* `raster` for the scanline rasterizer, 
* `dash` for dash patterns, 
* `stroke` for stroke outlines,
* `gradient` for paint servers, 
* `layer` for group opacity, 
* `soft` for the software backend, 
* `render` for the tree walk, 
* `clip`, `mask` and `image` for the backend primitives behind them/
* `clippath`, `maskelement`, `pattern` and `imageelement` for the elements that use them, 
* `refs` for their goldens, 
* `text` for text layout and glyph runs, 
* `fonts` for the font registry, 
* `anim` for the SMIL timeline
* `trace` for the operation-log goldens.

`./build/runtests --suite=dom` runs one of them alone.

The `text` suite draws in a stub font of square glyphs, so it does not need a installed font.

Only the `fonts` suite touches freetype, and it passes whether or not the library is there.

## Golden logs

`tests/goldens/*.log` hold the expected `fpsvg.trace` output: 
one operation per line, with clip and layer nesting indented by two spaces. 
A failing test reports the first differing line by number.

Regenerate after an intended format or behaviour change:

```
SVG_UPDATE_GOLDENS=1 ./build/runtests --suite=trace
```
or, if you use csh:
```
setenv SVG_UPDATE_GOLDENS 1 
./build/runtests --suite=trace
```

Please read the resulting diff before committing it. 
* A failing comparison also writes the produced text next to the golden as `.actual`. 
* `SVG_GOLDEN_DIR` overrides the directory holding the logs, 
* `SVG_DATA_DIR` the one holding the documents in `tests/data`.

* `tree-*.log` and `use-expansion.log` hold parsed trees rather than operation logs: 
   one node per line, attributes in the order the XML parser reported them.
* `cascade.log` holds computed property sets, four lines per element.
* `coverage.log`, `render.log`, `stroke.log`, `gradient.log` and `render-coverage.log` hold one character per pixel, 
  from a space for no coverage to `@` for full coverage. 
* `gradient.log` samples every fourth pixel of one channel.
* `render-walk.log`, `render-viewports.log`, `refs-walk.log`, `text-walk.log` and `anim-*.log` are operation logs 
   of a whole document rendered through `fpsvg.render`, rather than of backend calls made by hand. 
* The `anim-*.log` hold `anim.svg`, `animtransform.svg`, `animmotion.svg` and `animchain.svg` of `tests/data`, three moments of each, 
  so they are written by `--suite=anim` rather than `--suite=trace`.
* Numbers are written with at most six decimals through `Str`, which is locale-independent, 
  and negative zero is normalised to `0`. 

The same logs are expected from every target.

## Testing the font coverage of a platform

If a font misses a glyph for a character, fcl-svg will look for a replacement font. 
`fontprobe` reports what this system offers for a character.

It uses the coverage source built for the platform it was compiled on, 
then opens the font it returns (if any) and checks the font really holds the character.

On linux
```
fpc -Mobjfpc -Sh -Sc -O2 -Fusrc -Futests -Fu../../fcl-css/src -Fu../../libfontconfig/src -FEbuild tests/fontprobe.pp
./build/fontprobe
```
On mac
```
fpc -Mobjfpc -Sh -Sc -O2 -Fusrc -Futests -Fu../../fcl-css/src -Fu../../univint/src -FEbuild tests/fontprobe.pp
./build/fontprobe
```
On windows
```
fpc -Mobjfpc -Sh -Sc -O2 -Fusrc -Futests -Fu../../fcl-css/src -FEbuild tests/fontprobe.pp
./build/fontprobe
```

It exits 0 when every character was covered, 1 when the font resolver cannot answer on
this system at all, 2 when it answered nothing usable, and 3 when it
returned a font file for a character and the font turned out not to have the
character it.

## Pixel tests

`svgdiff` renders every document of a suite and compares it against the reference image beside it. 
The W3C SVG 1.1 suite is not included here due to licensing issues.

To fetch the suite and run the reference tests:
```
./fetch-w3c.sh
fpc -Mobjfpc -Sh -Sc -Fusrc -Futests -Fu$CSS -Fu$IMG -FEbuild tests/svgdiff.pp
./build/svgdiff --suite=w3c --tolerance=2 --threshold=0.5
```

The archive does not contain the web fonts that twenty of its documents
require in their `@font-face` rules, so the fetch script pulls those from the same
server one at a time into `svg/woffs`. 

`svgdiff` reads `svg/woffs` and `resources` on top of the fonts of the system 
or of a `--fonts` directory. 

A document may name a face without declaring it anywhere, (e.g. text-fonts-202-t) as
reader to install ZalamanderCaps and then view it.

A test passes when no more than `--threshold` percent of its pixels differ,
where a pixel differs when a channel is further than `--tolerance` apart. 

Both images are composited onto white first, so a transparent rendering and an
opaque white reference agree. The exit status is 0 when every test passed, 1
when one did not, and 2 when the suite could not be read.

`--write-diff` writes a map of each failure, red where the pixels differ over
a greyed copy of the reference. `--filter=TEXT` narrows the run to the tests
whose name contains TEXT, and `--list` shows what would run. `--help` gives
the rest.

### The draft watermark

When comparing the reference images with what fcl-svg produces, some snags
were hit, the first is the draft watermark.

A test of the W3C suite that was never approved has a red bar reading
DRAFT across its head. Ninety-one documents still have one that is
not commented out, so a renderer that follows the document exactly will draw it.

Several options to deal with this.

`--no-watermark` takes that group out before rendering. 
`--mask-boilerplate` can also be used.
`--mask-watermark` as well. 

Where a document has a watermark it paints the band the bar covers over both images before comparing them, 
so anything that was different in that area will not be taken into account.  
The band is read from the watermark's own rectangle and scaled to the image.

```
./build/svgdiff --suite=w3c --in-scope --mask-watermark --tolerance=2 --threshold=0.5
```

It is off by default.

### The revision line

The second problem is the revision. The revisions in reference images are not always in
line with the spec (and hence SVG document) revision:

The template also stamps `$Revision: 1.5 $` across the foot of every
document, and that line differs on nearly all of the images. 

That line is not masked. 
It is counted instead, and the report gives it a line of its own under the name: 
how much of each picture it accounts for. 

### Kerning a system face

The third encountered problem is font kerning:

A font the document declares is kerned from its own `hkern`. 
A face out of the system is not kerned this way, unless `--kern-faces` is specified: 
The reference images were drawn without it.
```
./build/svgdiff --suite=w3c --in-scope --kern-faces --tolerance=2
```

### Which engine draws the text

* `--font-engine=coretext` draws the text of every document with Core Text instead of freetype on macOS, 
* `--font-engine=gdi` does the same with the GDI font calls on Windows. 

The HTML page produced by `--html` displays at the top which of the two it used.

```
./build/svgdiff --suite=w3c --font-engine=coretext --tolerance=2
```

The two runs are not directly comparable as scores. 
`--fonts=DIR` limits freetype to the fonts in that directory, 
and the references were drawn over a limited set; 

Core Text and GDI always see the faces of the system as well, 
so a document naming Arial gets the real Arial rather than the Liberation
face (which has the same widths). 

### The frame of the template

Another problem is the frame drawn around the template.

Every document is drawn inside `<rect id="test-frame" x="1" y="1" ...>` with a stroke a unit wide. 

A stroke straddles the line it is drawn on, so that one covers user x from 0.5 to 1.5 and, at a scale of one, leaves two pixels half covered. 

That is what fcl-svg draws, and what Inkscape draws. The renderer that made the references put it on whole pixels instead: one pixel fully covered, its neighbour clear. 

It costs about 2% of every picture, and it costs it whatever the renderer here does, 
so it weighs most on the documents that are otherwise closest: 
two thirds of the difference of a document sitting at 3%. Over the in-scope
run it is 21.6% of everything that differs, more than the revision line.

Like the revision line it is counted rather than masked, on a line of its own under the name, 
and under both goes what is left: the actual difference of the document. 

The `--mask-frame` option will cover the band in both pictures, but this is off by default.

```
./build/svgdiff --suite=w3c --in-scope --mask-frame --tolerance=2
```

The band is worked out from the frame element: the rectangle it names,
scaled to the frame the document is drawn in, with half the stroke and a
pixel of slack either side of it. The revision box is held clear of it, so
that the two counts do not measure the same pixel twice and the actual
difference is what is left when both are taken off.

### Looking at a whole run

`--html=FILE` writes a page containing every document of the run, its rendering
beside the reference and how "far" the two are apart.

```
./build/svgdiff --suite=w3c --tolerance=2 --threshold=0.5 --html=build/report.html
```

Using `--html` turns on `--write-render`, since the page needs a rendering of every test
and not only of the ones that failed. 
The images are named where they lie relative to the page, so the page has to stay 
where it was written for them to load; 
`build/` is the natural place, beside the renderings themselves.

The rows are stacked in decreasing order of mismatch:
worst first, which is the order they are worth reading in. 

There is a search box to filter them by name, a button to put them back in alphabetical order, 
and an image opens at its own size when it is clicked. 

Both images sit on squares, so a rendering that is transparent where the reference is white can
be told from one that is white.

The page recorded in `build/report.html` is the in-scope run with the frame
masked and gradients read at the corner of a pixel:

```
./build/svgdiff --suite=w3c --in-scope --mask-frame --paint-sample=corner \
  --tolerance=2 --threshold=0.5 --html=build/report.html
```

## Animated documents

The 83 documents of the suite that animate are left out of the pixel
comparison tests. We do not know at what point their references were drawn. 

`tests/svgsmil.pp` is what can be used instead: it seeks each document through
`TSVGTimeline`, draws a frame every tenth of a second, writes them as one
animated GIF through `TFPWriterGIF`, and writes a page that shows each GIF
beside the SVG itself.

```
CSS=$FPCSRC/packages/fcl-css/src
IMG=$FPCSRC/packages/fcl-image/src
fpc -Mobjfpc -Sh -Sc -O2 -Fusrc -Futests -Fu../../fcl-css/src -Fu../../fcl-image/src -FEbuild tests/svgsmil.pp
./build/svgsmil
```

It needs an fcl-image that has the GIF writer in it.

The program writes the GIFs to `build/smil` and the page to `report-smil.html`.
It takes about three minutes: every frame is a render of its own.
Using `--rate` and `--seconds` you can control how many frames a second to draw 
and how long an animation to follow.

The `--filter` option can be used to limit the run to one document, and `--help` lists the rest.

The `--tail` option determines how long each document is followed past the end of its last animation (default 1.5 seconds).

The middle column of the page is the document itself, which the browser
animates with its own SMIL engine; the right one is the GIF. 

