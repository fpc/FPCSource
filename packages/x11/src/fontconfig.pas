{*
 * fontconfig/fontconfig/fontconfig.h
 *
 * Copyright © 2001 Keith Packard
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Keith Packard not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Keith Packard makes no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE AUTHOR(S) DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *}
unit fontconfig;

{$mode objfpc}{$H+}
{$packrecords c}

interface

uses
  Classes, SysUtils, X, XLib, Xutil, ctypes;

const
  {$IF Defined(DARWIN)}
    fclib = 'libfontconfig.dylib';
    {$LINKLIB libfontconfig}
  {$ELSE}
    fclib = 'libfontconfig.so';
  {$IFEND}

//#define FcPublic

type
  FcChar8 = cuchar;
  PFcChar8 = ^FcChar8;
  FcChar16 = cushort;
  FcChar32 = cuint;
  FcBool = cint;

const
{*
 * Current Fontconfig version number.  This same number
 * must appear in the fontconfig configure.in file. Yes,
 * it'a a pain to synchronize version numbers like this.
 *}

  FC_MAJOR	= 2;
  FC_MINOR	= 8;
  FC_REVISION = 0;

  FC_VERSION = ((FC_MAJOR * 10000) + (FC_MINOR * 100) + (FC_REVISION));

{*
 * Current font cache file format version
 * This is appended to the cache files so that multiple
 * versions of the library will peacefully coexist
 *
 * Change this value whenever the disk format for the cache file
 * changes in any non-compatible way.  Try to avoid such changes as
 * it means multiple copies of the font information.
 *}

  FC_CACHE_VERSION   = '3';

  FcTrue	=	1;
  FcFalse	=	0;

const
  FC_FAMILY =          'family';	//* String */
  FC_STYLE =           'style';		//* String */
  FC_SLANT =           'slant';		//* Int */
  FC_WEIGHT =	       'weight';	//* Int */
  FC_SIZE =	           'size';      //* Double */
  FC_ASPECT =	       'aspect';	//* Double */
  FC_PIXEL_SIZE =      'pixelsize';     //* Double */
  FC_SPACING =	       'spacing';	//* Int */
  FC_FOUNDRY =	       'foundry';	//* String */
  FC_ANTIALIAS =       'antialias';	//* Bool (depends) */
  FC_HINTING =	       'hinting';	//* Bool (true) */
  FC_HINT_STYLE	=      'hintstyle';	//* Int */
  FC_VERTICAL_LAYOUT = 'verticallayout';//* Bool (false) */
  FC_AUTOHINT =	       'autohint';	//* Bool (false) */
  FC_GLOBAL_ADVANCE =  'globaladvance';	//* Bool (true) */
  FC_WIDTH=	       'width';		//* Int */
  FC_FILE =	       'file';		//* String */
  FC_INDEX =	       'index';		//* Int */
  FC_FT_FACE =	       'ftface';	//* FT_Face */
  FC_RASTERIZER =      'rasterizer';	//* String */
  FC_OUTLINE =	       'outline';	//* Bool */
  FC_SCALABLE =	       'scalable';	//* Bool */
  FC_SCALE =	       'scale';		//* double */
  FC_DPI =             'dpi';		//* double */
  FC_RGBA =            'rgba';		//* Int */
  FC_MINSPACE =	       'minspace';	//* Bool use minimum line spacing */
  FC_SOURCE =	       'source';	//* String (X11, freetype) */
  FC_CHARSET =	       'charset';	//* CharSet */
  FC_LANG =            'lang';		//* String RFC 3066 langs */
  FC_FONTVERSION =     'fontversion';	//* Int from 'head' table */
  FC_FULLNAME =	       'fullname';	//* String */
  FC_FAMILYLANG =      'familylang';	//* String RFC 3066 langs */
  FC_STYLELANG =       'stylelang';	//* String RFC 3066 langs */
  FC_FULLNAMELANG =    'fullnamelang';	//* String RFC 3066 langs */
  FC_CAPABILITY =      'capability';	//* String */
  FC_FONTFORMAT =      'fontformat';	//* String */
  FC_EMBOLDEN =	       'embolden';	//* Bool - true if emboldening needed*/
  FC_EMBEDDED_BITMAP = 'embeddedbitmap';//* Bool - true to enable embedded bitmaps */
  FC_DECORATIVE =      'decorative';	//* Bool - true if style is a decorative variant */
  FC_LCD_FILTER =      'lcdfilter';	//* Int */

  //FC_CACHE_SUFFIX		    ".cache-"FC_CACHE_VERSION
  //FC_DIR_CACHE_FILE	    "fonts.cache-"FC_CACHE_VERSION
  //FC_USER_CACHE_FILE	    ".fonts.cache-"FC_CACHE_VERSION

  // Adjust outline rasterizer */
  FC_CHAR_WIDTH =      'charwidth'; // Int */
  FC_CHAR_HEIGHT =      'charheight'; // Int */
  FC_MATRIX =          'matrix';    //* FcMatrix */

  FC_WEIGHT_THIN = 0;
  FC_WEIGHT_EXTRALIGHT =    40;
  FC_WEIGHT_ULTRALIGHT =    FC_WEIGHT_EXTRALIGHT;
  FC_WEIGHT_LIGHT =	    50;
  FC_WEIGHT_BOOK =	    75;
  FC_WEIGHT_REGULAR =	    80;
  FC_WEIGHT_NORMAL =	    FC_WEIGHT_REGULAR;
  FC_WEIGHT_MEDIUM =	    100;
  FC_WEIGHT_DEMIBOLD =	    180;
  FC_WEIGHT_SEMIBOLD =	    FC_WEIGHT_DEMIBOLD;
  FC_WEIGHT_BOLD = 200;
  FC_WEIGHT_EXTRABOLD =	    205;
  FC_WEIGHT_ULTRABOLD =	    FC_WEIGHT_EXTRABOLD;
  FC_WEIGHT_BLACK =	    210;
  FC_WEIGHT_HEAVY =	    FC_WEIGHT_BLACK;
  FC_WEIGHT_EXTRABLACK =    215;
  FC_WEIGHT_ULTRABLACK =    FC_WEIGHT_EXTRABLACK;

  FC_SLANT_ROMAN =	    0;
  FC_SLANT_ITALIC =	    100;
  FC_SLANT_OBLIQUE =	    110;

  FC_WIDTH_ULTRACONDENSED =    50;
  FC_WIDTH_EXTRACONDENSED =    63;
  FC_WIDTH_CONDENSED =	    75;
  FC_WIDTH_SEMICONDENSED =    87;
  FC_WIDTH_NORMAL =	    100;
  FC_WIDTH_SEMIEXPANDED =   113;
  FC_WIDTH_EXPANDED =	    125;
  FC_WIDTH_EXTRAEXPANDED =  150;
  FC_WIDTH_ULTRAEXPANDED =  200;

  FC_PROPORTIONAL =	    0;
  FC_DUAL =		    90;
  FC_MONO = 100;
  FC_CHARCELL =	    110;

  //* sub-pixel order */
  FC_RGBA_UNKNOWN =	    0;
  FC_RGBA_RGB =	    1;
  FC_RGBA_BGR =	    2;
  FC_RGBA_VRGB =	    3;
  FC_RGBA_VBGR =	    4;
  FC_RGBA_NONE =	    5;

  // hinting style */
  FC_HINT_NONE =        0;
  FC_HINT_SLIGHT =      1;
  FC_HINT_MEDIUM =      2;
  FC_HINT_FULL =        3;

  // LCD filter */
  FC_LCD_NONE =	    0;
  FC_LCD_DEFAULT =	    1;
  FC_LCD_LIGHT =	    2;
  FC_LCD_LEGACY =	    3;

type
  TFcType = Byte; // In C was a enum

const
  FcTypeVoid         = 0;
  FcTypeInteger      = 1;
  FcTypeDouble       = 2;
  FcTypeString       = 3;
  FcTypeBool         = 4;
  FcTypeMatrix       = 5;
  FcTypeCharSet      = 6;
  FcTypeFTFace       = 7;
  FcTypeLangSet      = 8;

type
  TFcMatrix = record
    xx, xy, yx, yy: double;
  end;

//#define FcMatrixInit(m)	((m)->xx = (m)->yy = 1, \
//			 (m)->xy = (m)->yx = 0)

{*
 * A data structure to represent the available glyphs in a font.
 * This is represented as a sparse boolean btree.
 *}

  TFcCharSet = record
    dummy : integer;
  end;
  PFcCharSet = ^TFcCharSet;

  TFcObjectType = record
    object_: PChar;
    type_: TFcType;
  end;

  TFcConstant = record
    name: PFcChar8;
    object_: PChar;
    value: cint;
  end;

  TFcResult = (
    FcResultMatch, FcResultNoMatch, FcResultTypeMismatch, FcResultNoId,
    FcResultOutOfMemory);

  TFcPattern = record
    dummy : integer;
  end;

  PFcPattern = ^TFcPattern;
  PPFcPattern = ^PFcPattern;

  TFcLangSet = record
    dummy : integer;
  end;

  TFcValue = record
    type_: TFcType;
    (*union {
	const FcChar8	*s;
	int		i;
	FcBool		b;
	double		d;
	const FcMatrix	*m;
	const FcCharSet	*c;
	void		*f;
	const FcLangSet	*l;
    } u;*)
  end;

  TFcFontSet = packed record
    nfont : integer;
    sfont : integer;
    fonts : PPFcPattern;
  end;
  PFcFontSet = ^TFcFontSet;

implementation

end.

