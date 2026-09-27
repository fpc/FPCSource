# fcl-image tests

One fpcunit console runner, `testfpimage`, contains all tests. 

Each area is a separate suite.

## Build and run

From `packages/fcl-image`:

```sh
sh tests/build.sh
tests/build/testfpimage -a
tests/build/testfpimage --suite=png
```

* `tests/build.sh` compiles the runner and the fcl-image sources from `src`
  with range, overflow, I/O and stack checks and with heaptrc, into `tests/build`.
* The other packages come from the installed units. 
* Extra arguments are passed to `fpc`, e.g. `sh tests/build.sh -dFPIMAGE_KNOWNISSUES`,
  or `sh tests/build.sh -Cr- -Co-` for a build without range and overflow checks.

The Lazarus project `tests/testfpimage.lpi` builds the same runner.

## Suites

| Suite | Unit | Covers |
|---|---|---|
| color | tccolor.pp | TFPColor routines, AlphaBlend, CalculateGray, HTML colours, colour constants |
| palette | tcpalette.pp | TFPPalette and the standard palettes |
| memimage | tcmemimage.pp | TFPMemoryImage: size, pixels, palette mode, Assign, extra, resolution |
| compactimg | tccompactimg.pp | the TFPCompactImg* classes and GetMinimumFPCompactImg |
| handlers | tchandlers.pp | reader and writer registry, format detection, load and save by name |
| bmp | tcbmp.pp | BMP at every depth, RLE, headers, streams, hand-built variants |
| png | tcpng.pp | PNG writer options and chunks; reader against an independent encoder (filters, Adam7, depths, tRNS) |
| tga | tctga.pp | TARGA round trips, header, every image type, origin, depth and RLE |
| pcx | tcpcx.pp | PCX round trips, compression, header, 1/4/8/24-bit files |
| pnm | tcpnm.pp | PBM/PGM/PPM binary and text, 16-bit samples, depth guessing |
| xpm | tcxpm.pp | XPM colour sizes, code lengths, transparency, hand-written files |
| qoi | tcqoi.pp | QOI round trips; writer against a decoder from the specification; every operation |
| jpeg | tcjpeg.pp | JPEG quality floors, scaling, EXIF orientation, resolution, CMYK example |
| tiff | tctiff.pp | TIFF writer round trips; reader on hand-built files (byte orders, compressions, tiles, planar, orientations) |
| psd | tcpsd.pp | PSD reader on hand-built files of every mode and depth |
| xwd | tcxwd.pp | XWD reader on hand-built files of every depth and byte order |
| canvas | tccanvas.pp | drawing on TFPImageCanvas: lines, rectangles, ellipses, polygons, flood fill, pen modes, patterns, clipping, copying, gradients, transformations |
| pscanvas | tcpscanvas.pp | PostScript document structure and operators |
| ftfont | tcftfont.pp | FreeType text on a canvas (ignored when libfreetype is missing) |
| colorspace | tccolorspace.pp | fpcolorspace conversions against reference values |
| misc | tcmisc.pp | clipping, CRC, byte swapping, units of measure, paper sizes |
| qrcode | tcqrcode.pp | QR code generation and drawing |
| barcodedraw | tcbarcodedraw.pp | drawing barcodes on an image |
| interp | tcinterp.pp | StretchDraw with every interpolation |
| blur | tcgauss.pp | Gaussian blur |
| quantize | tcquantize.pp | colour quantizers, ditherers, colour hash |
| TTestBarcodes | tcbarcodes.pas | fpbarcode encodings |
| gifwrite, gifread | tcgifwrite.pp, tcgifread.pp | GIF writer and multi-frame reader |

Conventions regading fcl-imageo: 
- rectangles follow the canvas property RectangleMode 
  (the canvas tests use rmExclude, TTestCanvasRectangleMode covers both).
- lines include both end points. 
- The 16 pen modes follow the raster operations of GDI; 
- the most significant bit of a pattern is the leftmost pixel; 
- gray conversions store the luma; 
- readers start at the stream position and stop at the end of the image; 
- errors of readers and writers are FPImageException descendants.

## Test data

* The tests generate their images in memory, or build the bytes of a file by hand for variants that cannot be produced by the writer. 
* The PNG, QOI and TIFF tests also have small encoders or decoders of their own as an independent reference. 
* Three files of `examples` are read: `cmyk.jpg`, `float-tiff.tif` and `DejaVuLGCSans.ttf`.
* run the tests from `packages/fcl-image` or from `tests`.

## Known issues

Tests for defects that are known but not yet fixed are compiled only with `-dFPIMAGE_KNOWNISSUES`, so that the default run stays green.
(currently none are left, that is why the testsuite was created)
