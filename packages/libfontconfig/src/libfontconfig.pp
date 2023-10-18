
{$mode objfpc}
{$H+}
unit libfontconfig;
{
  Automatically converted by H2Pas 1.0.0 from fc.h
  The following command line parameters were used:
    -l
    libfontconfig.so
    -P
    -p
    -S
    -T
    -C
    -c
    fc.h
}

interface

uses
  ctypes;

Const
{$ifndef darwin}
  DefaultLibName = 'libfontconfig.so';
{$else}  
  DefaultLibName = 'libfontconfig.dylib';
{$endif}  

const
  FC_MAJOR = 2;    
  FC_MINOR = 11;    
  FC_REVISION = 94;    
  FC_VERSION	= ((FC_MAJOR * 10000) + (FC_MINOR * 100) + (FC_REVISION));
  FC_CACHE_VERSION_NUMBER = 6; 
  FC_CACHE_VERSION = '6'; // Stringify of FC_CACHE_VERSION_NUMBER     

  FcTrue = 1;      
  FcFalse = 0;      
  FC_FAMILY = 'family';      
  FC_STYLE = 'style';      
  FC_SLANT = 'slant';      
  FC_WEIGHT = 'weight';      
  FC_SIZE = 'size';      
  FC_ASPECT = 'aspect';      
  FC_PIXEL_SIZE = 'pixelsize';      
  FC_SPACING = 'spacing';      
  FC_FOUNDRY = 'foundry';      
  FC_ANTIALIAS = 'antialias';      
  FC_HINTING = 'hinting';      
  FC_HINT_STYLE = 'hintstyle';      
  FC_VERTICAL_LAYOUT = 'verticallayout';      
  FC_AUTOHINT = 'autohint';      
  FC_GLOBAL_ADVANCE = 'globaladvance';      
  FC_WIDTH = 'width';      
  FC_FILE = 'file';      
  FC_INDEX = 'index';      
  FC_FT_FACE = 'ftface';      
  FC_RASTERIZER = 'rasterizer';      
  FC_OUTLINE = 'outline';      
  FC_SCALABLE = 'scalable';      
  FC_COLOR = 'color';      
  FC_SCALE = 'scale';      
  FC_SYMBOL = 'symbol';      
  FC_DPI = 'dpi';      
  FC_RGBA = 'rgba';      
  FC_MINSPACE = 'minspace';      
  FC_SOURCE = 'source';      
  FC_CHARSET = 'charset';      
  FC_LANG = 'lang';      
  FC_FONTVERSION = 'fontversion';      
  FC_FULLNAME = 'fullname';      
  FC_FAMILYLANG = 'familylang';      
  FC_STYLELANG = 'stylelang';      
  FC_FULLNAMELANG = 'fullnamelang';      
  FC_CAPABILITY = 'capability';      
  FC_FONTFORMAT = 'fontformat';      
  FC_EMBOLDEN = 'embolden';      
  FC_EMBEDDED_BITMAP = 'embeddedbitmap';      
  FC_DECORATIVE = 'decorative';      
  FC_LCD_FILTER = 'lcdfilter';      
  FC_FONT_FEATURES = 'fontfeatures';      
  FC_NAMELANG = 'namelang';      
  FC_PRGNAME = 'prgname';      
  FC_HASH = 'hash';      
  FC_POSTSCRIPT_NAME = 'postscriptname';      

  FC_CACHE_SUFFIX = '.cache-'+ FC_CACHE_VERSION;
  FC_DIR_CACHE_FILE = 'fonts.cache-' + FC_CACHE_VERSION;
  FC_USER_CACHE_FILE= '.fonts.cache-' + FC_CACHE_VERSION;
  FC_CHAR_WIDTH = 'charwidth';      
  FC_CHAR_HEIGHT = 'charheight';      
  FC_MATRIX = 'matrix';      
  FC_WEIGHT_THIN = 0;      
  FC_WEIGHT_EXTRALIGHT = 40;      
  FC_WEIGHT_ULTRALIGHT = FC_WEIGHT_EXTRALIGHT;      
  FC_WEIGHT_LIGHT = 50;      
  FC_WEIGHT_DEMILIGHT = 55;      
  FC_WEIGHT_SEMILIGHT = FC_WEIGHT_DEMILIGHT;      
  FC_WEIGHT_BOOK = 75;      
  FC_WEIGHT_REGULAR = 80;      
  FC_WEIGHT_NORMAL = FC_WEIGHT_REGULAR;      
  FC_WEIGHT_MEDIUM = 100;      
  FC_WEIGHT_DEMIBOLD = 180;      
  FC_WEIGHT_SEMIBOLD = FC_WEIGHT_DEMIBOLD;      
  FC_WEIGHT_BOLD = 200;      
  FC_WEIGHT_EXTRABOLD = 205;      
  FC_WEIGHT_ULTRABOLD = FC_WEIGHT_EXTRABOLD;      
  FC_WEIGHT_BLACK = 210;      
  FC_WEIGHT_HEAVY = FC_WEIGHT_BLACK;      
  FC_WEIGHT_EXTRABLACK = 215;      
  FC_WEIGHT_ULTRABLACK = FC_WEIGHT_EXTRABLACK;      
  FC_SLANT_ROMAN = 0;      
  FC_SLANT_ITALIC = 100;      
  FC_SLANT_OBLIQUE = 110;      
  FC_WIDTH_ULTRACONDENSED = 50;      
  FC_WIDTH_EXTRACONDENSED = 63;      
  FC_WIDTH_CONDENSED = 75;      
  FC_WIDTH_SEMICONDENSED = 87;      
  FC_WIDTH_NORMAL = 100;      
  FC_WIDTH_SEMIEXPANDED = 113;      
  FC_WIDTH_EXPANDED = 125;      
  FC_WIDTH_EXTRAEXPANDED = 150;      
  FC_WIDTH_ULTRAEXPANDED = 200;      
  FC_PROPORTIONAL = 0;      
  FC_DUAL = 90;      
  FC_MONO = 100;      
  FC_CHARCELL = 110;      
  FC_RGBA_UNKNOWN = 0;      
  FC_RGBA_RGB = 1;      
  FC_RGBA_BGR = 2;      
  FC_RGBA_VRGB = 3;      
  FC_RGBA_VBGR = 4;      
  FC_RGBA_NONE = 5;      
  FC_HINT_NONE = 0;      
  FC_HINT_SLIGHT = 1;      
  FC_HINT_MEDIUM = 2;      
  FC_HINT_FULL = 3;      
  FC_LCD_NONE = 0;      
  FC_LCD_DEFAULT = 1;      
  FC_LCD_LIGHT = 2;      
  FC_LCD_LEGACY = 3;      

  FC_UTF8_MAX_LEN = 6;    

{
  Automatically converted by H2Pas 1.0.0 from fc.h
  The following command line parameters were used:
    -l
    libfontconfig.so
    -P
    -p
    -S
    -T
    -C
    -c
    fc.h
}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;
  
  PStat = pointer;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}
  PFcAtomic = ^TFcAtomic;
  TFCAtomic = record end;
  PPFcAtomic = ^PFcAtomic;
  
  PFcConfig = ^TFcConfig;
  TFcConfig = record end;
  PPFcConfig = ^PFcConfig;

  PFcFileCache = ^TFcFileCache;
  TFcFileCache = record end;
  PPFcFileCache = ^PFcFileCache;

  PFcBlanks = ^TFcBlanks;
  TFcBlanks = record end;
  PPFcBlanks = ^PFcBlanks;

  PFcPattern = ^TFcPattern;
  TFcPattern = record end;
  PPFcPattern = ^PFcPattern;
  
  PFcStrList = ^TFcStrList;
  TFcStrList = record end;
  PPFcStrList = ^PFcStrList;
  
  PFcStrSet = ^TFcStrSet;
  TFcStrSet = record end;
  PPFcStrSet = ^PFcStrSet;

  PFcCharSet = ^TFcCharSet;
  TFcCharSet = record end;
  PPFcCharSet = ^PFcCharSet;
  
  PFcLangSet = ^TFcLangSet;
  TFcLangSet = record end;
  PPFcLangSet = ^PFcLangSet;

  PFcRange = ^TFcRange;
  TFcRange = record end;
  PPFcRange = ^PFcRange;
  
  PFcCache = ^TFcCache;
  TFcCache = record end;
  PPFcCache = ^PFcCache;

  PFcChar8 = Pchar;
  TFcChar8 = char;
  PPFcChar8 = PPChar;

  PFcChar16 = PWideChar;
  TFcChar16 = WideChar;
  PPFcChar16 = ^PFcChar16;

  PFcChar32 = ^TFcChar32;
  TFcChar32 = cuint;

  PFcBool = ^TFcBool;
  TFcBool = cint;
  PFcType = ^TFcType;
  TFcType = (FcTypeUnknown := -(1),FcTypeVoid,FcTypeInteger,
    FcTypeDouble,FcTypeString,FcTypeBool,FcTypeMatrix,
    FcTypeCharSet,FcTypeFTFace,FcTypeLangSet,
    FcTypeRange);

  PFcMatrix = ^TFcMatrix;
  TFcMatrix = record
    xx : double;
    xy : double;
    yx : double;
    yy : double;
  end;
  PPFcMatrix = ^PFcMatrix;

  PFcObjectType = ^TFcObjectType;
  TFcObjectType = record
    _object : pcchar;
    _type : TFcType;
  end;
  PPFcObjectType = ^PFcObjectType;

  PFcConstant = ^TFcConstant;
  TFcConstant = record
    name : PFcChar8;
    _object : pcchar;
    value : cint;
  end;
  PPFcConstant = ^PFcConstant;

  PFcResult = ^TFcResult;
  TFcResult = (FcResultMatch,FcResultNoMatch,FcResultTypeMismatch, FcResultNoId,FcResultOutOfMemory);

  PFcValue = ^TFcValue;
  TFcValue = record
      _type : TFcType;
      u : record
          case longint of
            0 : ( s : PFcChar8 );
            1 : ( i : cint );
            2 : ( b : TFcBool );
            3 : ( d : double );
            4 : ( m : PFcMatrix );
            5 : ( c : PFcCharSet );
            6 : ( f : pointer );
            7 : ( l : PFcLangSet );
            8 : ( r : PFcRange );
          end;
    end;
  PPFcValue = ^PFcValue;
  
  PFcFontSet = ^TFcFontSet;
  TFcFontSet = record
      nfont : cint;
      sfont : cint;
      fonts : ^PFcPattern;
    end;
  PPFcFontSet = ^PFcFontSet;

  PFcObjectSet = ^TFcObjectSet;
  TFcObjectSet = record
      nobject : cint;
      sobject : cint;
      objects : ^pcchar;
    end;
  PPFcObjectSet = ^PFcObjectSet;
  
  PFcMatchKind = ^TFcMatchKind;
  TFcMatchKind = (FcMatchPattern,FcMatchFont,FcMatchScan);

  PFcLangResult = ^TFcLangResult;
  TFcLangResult = (FcLangEqual := 0,FcLangDifferentCountry := 1,
    FcLangDifferentTerritory := 1,FcLangDifferentLang := 2);

  PFcSetName = ^TFcSetName;
  TFcSetName = (FcSetSystem := 0,FcSetApplication := 1);

  PFcEndian = ^TFcEndian;
  TFcEndian = (FcEndianBig,FcEndianLittle);
  TFcGlobalCache = TFcFileCache;

  TFcCharMap = array[0..7] of TFcChar32;
  TUTF8Array = array[0..5] of TFcChar8;
  
var
  FcBlanksCreate : function:PFcBlanks;
  FcBlanksDestroy : procedure(b:PFcBlanks);
  FcBlanksAdd : function(b:PFcBlanks; ucs4:TFcChar32):TFcBool;
  FcBlanksIsMember : function(b:PFcBlanks; ucs4:TFcChar32):TFcBool;
  FcCacheDir : function(c:PFcCache):PFcChar8;
  FcCacheCopySet : function(c:PFcCache):PFcFontSet;
  FcCacheSubdir : function(c:PFcCache; i:cint):PFcChar8;
  FcCacheNumSubdir : function(c:PFcCache):cint;
  FcCacheNumFont : function(c:PFcCache):cint;
  FcDirCacheUnlink : function(dir:PFcChar8; config:PFcConfig):TFcBool;
  FcDirCacheValid : function(cache_file:PFcChar8):TFcBool;
  FcDirCacheClean : function(cache_dir:PFcChar8; verbose:TFcBool):TFcBool;
  FcCacheCreateTagFile : procedure(config:PFcConfig);
  FcConfigHome : function:PFcChar8;
  FcConfigEnableHome : function(enable:TFcBool):TFcBool;
  FcConfigFilename : function(name:PFcChar8):PFcChar8;
  FcConfigGetFilename : function(config:PFcConfig; name:PFcChar8):PFcChar8;
  FcConfigCreate : function:PFcConfig;
  FcConfigReference : function(config:PFcConfig):PFcConfig;
  FcConfigDestroy : procedure(config:PFcConfig);
  FcConfigSetCurrent : function(config:PFcConfig):TFcBool;
  FcConfigGetCurrent : function:PFcConfig;
  FcConfigUptoDate : function(config:PFcConfig):TFcBool;
  FcConfigBuildFonts : function(config:PFcConfig):TFcBool;
  FcConfigGetFontDirs : function(config:PFcConfig):PFcStrList;
  FcConfigGetConfigDirs : function(config:PFcConfig):PFcStrList;
  FcConfigGetConfigFiles : function(config:PFcConfig):PFcStrList;
  FcConfigGetCache : function(config:PFcConfig):PFcChar8;
  FcConfigGetBlanks : function(config:PFcConfig):PFcBlanks;
  FcConfigGetCacheDirs : function(config:PFcConfig):PFcStrList;
  FcConfigGetRescanInterval : function(config:PFcConfig):cint;
  FcConfigSetRescanInterval : function(config:PFcConfig; rescanInterval:cint):TFcBool;
  FcConfigGetFonts : function(config:PFcConfig; aset:TFcSetName):PFcFontSet;
  FcConfigAppFontAddFile : function(config:PFcConfig; afile:PFcChar8):TFcBool;
  FcConfigAppFontAddDir : function(config:PFcConfig; dir:PFcChar8):TFcBool;
  FcConfigAppFontClear : procedure(config:PFcConfig);
  FcConfigSubstituteWithPat : function(config:PFcConfig; p:PFcPattern; p_pat:PFcPattern; kind:TFcMatchKind):TFcBool;
  FcConfigSubstitute : function(config:PFcConfig; p:PFcPattern; kind:TFcMatchKind):TFcBool;
  FcConfigGetSysRoot : function(config:PFcConfig):PFcChar8;
  FcConfigSetSysRoot : procedure(config:PFcConfig; sysroot:PFcChar8);
  FcCharSetCreate : function:PFcCharSet;
  FcCharSetNew : function:PFcCharSet;
  FcCharSetDestroy : procedure(fcs:PFcCharSet);
  FcCharSetAddChar : function(fcs:PFcCharSet; ucs4:TFcChar32):TFcBool;
  FcCharSetDelChar : function(fcs:PFcCharSet; ucs4:TFcChar32):TFcBool;
  FcCharSetCopy : function(src:PFcCharSet):PFcCharSet;
  FcCharSetEqual : function(a:PFcCharSet; b:PFcCharSet):TFcBool;
  FcCharSetIntersect : function(a:PFcCharSet; b:PFcCharSet):PFcCharSet;
  FcCharSetUnion : function(a:PFcCharSet; b:PFcCharSet):PFcCharSet;
  FcCharSetSubtract : function(a:PFcCharSet; b:PFcCharSet):PFcCharSet;
  FcCharSetMerge : function(a:PFcCharSet; b:PFcCharSet; changed:PFcBool):TFcBool;
  FcCharSetHasChar : function(fcs:PFcCharSet; ucs4:TFcChar32):TFcBool;
  FcCharSetCount : function(a:PFcCharSet):TFcChar32;
  FcCharSetIntersectCount : function(a:PFcCharSet; b:PFcCharSet):TFcChar32;
  FcCharSetSubtractCount : function(a:PFcCharSet; b:PFcCharSet):TFcChar32;
  FcCharSetIsSubset : function(a:PFcCharSet; b:PFcCharSet):TFcBool;
  FcCharSetFirstPage : function(a:PFcCharSet; map:TFcCharMap; next:PFcChar32):TFcChar32;
  FcCharSetNextPage : function(a:PFcCharSet; map:TFcCharMap; next:PFcChar32):TFcChar32;
  FcCharSetCoverage : function(a:PFcCharSet; page:TFcChar32; result:PFcChar32):TFcChar32;
  FcValuePrint : procedure(v:TFcValue);
  FcPatternPrint : procedure(p:PFcPattern);
  FcFontSetPrint : procedure(s:PFcFontSet);
  FcGetDefaultLangs : function:PFcStrSet;
  FcDefaultSubstitute : procedure(pattern:PFcPattern);
  FcFileIsDir : function(afile:PFcChar8):TFcBool;
  FcFileScan : function(aset:PFcFontSet; dirs:PFcStrSet; cache:PFcFileCache; blanks:PFcBlanks; afile:PFcChar8; 
      force:TFcBool):TFcBool;
  FcDirScan : function(aset:PFcFontSet; dirs:PFcStrSet; cache:PFcFileCache; blanks:PFcBlanks; dir:PFcChar8; 
      force:TFcBool):TFcBool;
  FcDirSave : function(aset:PFcFontSet; dirs:PFcStrSet; dir:PFcChar8):TFcBool;
  FcDirCacheLoad : function(dir:PFcChar8; config:PFcConfig; cache_file:PPFcChar8):PFcCache;
  FcDirCacheRescan : function(dir:PFcChar8; config:PFcConfig):PFcCache;
  FcDirCacheRead : function(dir:PFcChar8; force:TFcBool; config:PFcConfig):PFcCache;
  FcDirCacheLoadFile : function(cache_file:PFcChar8; file_stat:Pstat):PFcCache;
  FcDirCacheUnload : procedure(cache:PFcCache);
  FcFreeTypeQuery : function(afile:PFcChar8; id:cint; blanks:PFcBlanks; count:pcint):PFcPattern;
  FcFontSetCreate : function:PFcFontSet;
  FcFontSetDestroy : procedure(s:PFcFontSet);
  FcFontSetAdd : function(s:PFcFontSet; font:PFcPattern):TFcBool;
  FcInitLoadConfig : function:PFcConfig;
  FcInitLoadConfigAndFonts : function:PFcConfig;
  FcInit : function:TFcBool;
  FcFini : procedure;
  FcGetVersion : function:cint;
  FcInitReinitialize : function:TFcBool;
  FcInitBringUptoDate : function:TFcBool;
  FcGetLangs : function:PFcStrSet;
  FcLangNormalize : function(lang:PFcChar8):PFcChar8;
  FcLangGetCharSet : function(lang:PFcChar8):PFcCharSet;
  FcLangSetCreate : function:PFcLangSet;
  FcLangSetDestroy : procedure(ls:PFcLangSet);
  FcLangSetCopy : function(ls:PFcLangSet):PFcLangSet;
  FcLangSetAdd : function(ls:PFcLangSet; lang:PFcChar8):TFcBool;
  FcLangSetDel : function(ls:PFcLangSet; lang:PFcChar8):TFcBool;
  FcLangSetHasLang : function(ls:PFcLangSet; lang:PFcChar8):TFcLangResult;
  FcLangSetCompare : function(lsa:PFcLangSet; lsb:PFcLangSet):TFcLangResult;
  FcLangSetContains : function(lsa:PFcLangSet; lsb:PFcLangSet):TFcBool;
  FcLangSetEqual : function(lsa:PFcLangSet; lsb:PFcLangSet):TFcBool;
  FcLangSetHash : function(ls:PFcLangSet):TFcChar32;
  FcLangSetGetLangs : function(ls:PFcLangSet):PFcStrSet;
  FcLangSetUnion : function(a:PFcLangSet; b:PFcLangSet):PFcLangSet;
  FcLangSetSubtract : function(a:PFcLangSet; b:PFcLangSet):PFcLangSet;
  FcObjectSetCreate : function:PFcObjectSet;
  FcObjectSetAdd : function(os:PFcObjectSet; _object:pcchar):TFcBool;
  FcObjectSetDestroy : procedure(os:PFcObjectSet);
  FcObjectSetBuild : function(first:pcchar; args:array of const):PFcObjectSet;
  FcFontSetList : function(config:PFcConfig; sets:PPFcFontSet; nsets:cint; p:PFcPattern; os:PFcObjectSet):PFcFontSet;
  FcFontList : function(config:PFcConfig; p:PFcPattern; os:PFcObjectSet):PFcFontSet;
  FcAtomicCreate : function(afile:PFcChar8):PFcAtomic;
  FcAtomicLock : function(atomic:PFcAtomic):TFcBool;
  FcAtomicNewFile : function(atomic:PFcAtomic):PFcChar8;
  FcAtomicOrigFile : function(atomic:PFcAtomic):PFcChar8;
  FcAtomicReplaceOrig : function(atomic:PFcAtomic):TFcBool;
  FcAtomicDeleteNew : procedure(atomic:PFcAtomic);
  FcAtomicUnlock : procedure(atomic:PFcAtomic);
  FcAtomicDestroy : procedure(atomic:PFcAtomic);
  FcFontSetMatch : function(config:PFcConfig; sets:PPFcFontSet; nsets:cint; p:PFcPattern; result:PFcResult):PFcPattern;
  FcFontMatch : function(config:PFcConfig; p:PFcPattern; result:PFcResult):PFcPattern;
  FcFontRenderPrepare : function(config:PFcConfig; pat:PFcPattern; font:PFcPattern):PFcPattern;
  FcFontSetSort : function(config:PFcConfig; sets:PPFcFontSet; nsets:cint; p:PFcPattern; trim:TFcBool; 
      csp:PPFcCharSet; result:PFcResult):PFcFontSet;
  FcFontSort : function(config:PFcConfig; p:PFcPattern; trim:TFcBool; csp:PPFcCharSet; result:PFcResult):PFcFontSet;
  FcFontSetSortDestroy : procedure(fs:PFcFontSet);
  FcMatrixCopy : function(mat:PFcMatrix):PFcMatrix;
  FcMatrixEqual : function(mat1:PFcMatrix; mat2:PFcMatrix):TFcBool;
  FcMatrixMultiply : procedure(result:PFcMatrix; a:PFcMatrix; b:PFcMatrix);
  FcMatrixRotate : procedure(m:PFcMatrix; c:double; s:double);
  FcMatrixScale : procedure(m:PFcMatrix; sx:double; sy:double);
  FcMatrixShear : procedure(m:PFcMatrix; sh:double; sv:double);
  FcNameRegisterObjectTypes : function(types:PFcObjectType; ntype:cint):TFcBool;
  FcNameUnregisterObjectTypes : function(types:PFcObjectType; ntype:cint):TFcBool;
  FcNameGetObjectType : function(_object:pcchar):PFcObjectType;
  FcNameRegisterConstants : function(consts:PFcConstant; nconsts:cint):TFcBool;
  FcNameUnregisterConstants : function(consts:PFcConstant; nconsts:cint):TFcBool;
  FcNameGetConstant : function(_string:PFcChar8):PFcConstant;
  FcNameConstant : function(_string:PFcChar8; result:pcint):TFcBool;
  FcNameParse : function(name:PFcChar8):PFcPattern;
  FcNameUnparse : function(pat:PFcPattern):PFcChar8;
  FcPatternCreate : function:PFcPattern;
  FcPatternDuplicate : function(p:PFcPattern):PFcPattern;
  FcPatternReference : procedure(p:PFcPattern);
  FcPatternFilter : function(p:PFcPattern; os:PFcObjectSet):PFcPattern;
  FcValueDestroy : procedure(v:TFcValue);
  FcValueEqual : function(va:TFcValue; vb:TFcValue):TFcBool;
  FcValueSave : function(v:TFcValue):TFcValue;
  FcPatternDestroy : procedure(p:PFcPattern);
  FcPatternEqual : function(pa:PFcPattern; pb:PFcPattern):TFcBool;
  FcPatternEqualSubset : function(pa:PFcPattern; pb:PFcPattern; os:PFcObjectSet):TFcBool;
  FcPatternHash : function(p:PFcPattern):TFcChar32;
  FcPatternAdd : function(p:PFcPattern; _object:pcchar; value:TFcValue; append:TFcBool):TFcBool;
  FcPatternAddWeak : function(p:PFcPattern; _object:pcchar; value:TFcValue; append:TFcBool):TFcBool;
  FcPatternGet : function(p:PFcPattern; _object:pcchar; id:cint; v:PFcValue):TFcResult;
  FcPatternDel : function(p:PFcPattern; _object:pcchar):TFcBool;
  FcPatternRemove : function(p:PFcPattern; _object:pcchar; id:cint):TFcBool;
  FcPatternAddInteger : function(p:PFcPattern; _object:pcchar; i:cint):TFcBool;
  FcPatternAddDouble : function(p:PFcPattern; _object:pcchar; d:double):TFcBool;
  FcPatternAddString : function(p:PFcPattern; _object:pcchar; s:PFcChar8):TFcBool;
  FcPatternAddMatrix : function(p:PFcPattern; _object:pcchar; s:PFcMatrix):TFcBool;
  FcPatternAddCharSet : function(p:PFcPattern; _object:pcchar; c:PFcCharSet):TFcBool;
  FcPatternAddBool : function(p:PFcPattern; _object:pcchar; b:TFcBool):TFcBool;
  FcPatternAddLangSet : function(p:PFcPattern; _object:pcchar; ls:PFcLangSet):TFcBool;
  FcPatternAddRange : function(p:PFcPattern; _object:pcchar; r:PFcRange):TFcBool;
  FcPatternGetInteger : function(p:PFcPattern; _object:pcchar; n:cint; i:pcint):TFcResult;
  FcPatternGetDouble : function(p:PFcPattern; _object:pcchar; n:cint; d:Pdouble):TFcResult;
  FcPatternGetString : function(p:PFcPattern; _object:pcchar; n:cint; s:PPFcChar8):TFcResult;
  FcPatternGetMatrix : function(p:PFcPattern; _object:pcchar; n:cint; s:PPFcMatrix):TFcResult;
  FcPatternGetCharSet : function(p:PFcPattern; _object:pcchar; n:cint; c:PPFcCharSet):TFcResult;
  FcPatternGetBool : function(p:PFcPattern; _object:pcchar; n:cint; b:PFcBool):TFcResult;
  FcPatternGetLangSet : function(p:PFcPattern; _object:pcchar; n:cint; ls:PPFcLangSet):TFcResult;
  FcPatternGetRange : function(p:PFcPattern; _object:pcchar; id:cint; r:PPFcRange):TFcResult;
  FcPatternBuild : function(p:PFcPattern; args:array of const):PFcPattern;
  FcPatternFormat : function(pat:PFcPattern; format:PFcChar8):PFcChar8;
  FcRangeCreateDouble : function(_begin:double; _end:double):PFcRange;
  FcRangeCreateInteger : function(_begin:TFcChar32; _end:TFcChar32):PFcRange;
  FcRangeDestroy : procedure(range:PFcRange);
  FcRangeCopy : function(r:PFcRange):PFcRange;
  FcRangeGetDouble : function(range:PFcRange; _begin:Pdouble; _end:Pdouble):TFcBool;
  FcWeightFromOpenType : function(ot_weight:cint):cint;
  FcWeightToOpenType : function(fc_weight:cint):cint;
  FcStrCopy : function(s:PFcChar8):PFcChar8;
  FcStrCopyFilename : function(s:PFcChar8):PFcChar8;
  FcStrPlus : function(s1:PFcChar8; s2:PFcChar8):PFcChar8;
  FcStrFree : procedure(s:PFcChar8);
  FcStrDowncase : function(s:PFcChar8):PFcChar8;
  FcStrCmpIgnoreCase : function(s1:PFcChar8; s2:PFcChar8):cint;
  FcStrCmp : function(s1:PFcChar8; s2:PFcChar8):cint;
  FcStrStrIgnoreCase : function(s1:PFcChar8; s2:PFcChar8):PFcChar8;
  FcStrStr : function(s1:PFcChar8; s2:PFcChar8):PFcChar8;
  FcUtf8ToUcs4 : function(src_orig:PFcChar8; dst:PFcChar32; len:cint):cint;
  FcUtf8Len : function(_string:PFcChar8; len:cint; nchar:pcint; wchar:pcint):TFcBool;
  FcUcs4ToUtf8 : function(ucs4:TFcChar32; dest: TUTF8Array):cint;
  FcUtf16ToUcs4 : function(src_orig:PFcChar8; endian:TFcEndian; dst:PFcChar32; len:cint):cint;
  FcUtf16Len : function(_string:PFcChar8; endian:TFcEndian; len:cint; nchar:pcint; wchar:pcint):TFcBool;
  FcStrDirname : function(afile:PFcChar8):PFcChar8;
  FcStrBasename : function(afile:PFcChar8):PFcChar8;
  FcStrSetCreate : function:PFcStrSet;
  FcStrSetMember : function(aset:PFcStrSet; s:PFcChar8):TFcBool;
  FcStrSetEqual : function(sa:PFcStrSet; sb:PFcStrSet):TFcBool;
  FcStrSetAdd : function(aset:PFcStrSet; s:PFcChar8):TFcBool;
  FcStrSetAddFilename : function(aset:PFcStrSet; s:PFcChar8):TFcBool;
  FcStrSetDel : function(aset:PFcStrSet; s:PFcChar8):TFcBool;
  FcStrSetDestroy : procedure(aset:PFcStrSet);
  FcStrListCreate : function(aset:PFcStrSet):PFcStrList;
  FcStrListFirst : procedure(list:PFcStrList);
  FcStrListNext : function(list:PFcStrList):PFcChar8;
  FcStrListDone : procedure(list:PFcStrList);
  FcConfigParseAndLoad : function(config:PFcConfig; afile:PFcChar8; complain:TFcBool):TFcBool;

// Macros
Procedure FcMatrixInit(out m : TFCMatrix);
function FC_CHARSET_DONE : TFcChar32;      
function FcIsUpper(c : longint) : Boolean;  
function FcIsLower(c : longint) : Boolean;  
function FcToLower(c : longint) : longint;  

Function LoadFontConfigLib(Const ALibName : String; RaiseError : Boolean = True) : Integer;
Function UnLoadFontConfigLib : Integer;
Function FontConfigLibLoaded : Boolean;

implementation

uses
  SysUtils;

Procedure FcMatrixInit(out m : TFCMatrix);

begin
  m.xx:=1;
  m.yy:=1;
end;

function FC_CHARSET_DONE : TFcChar32;
begin
  FC_CHARSET_DONE:=TFcChar32(-(1));
end;

function FcIsUpper(c : longint) : boolean;
begin
  FcIsUpper:=(Ord('A')<=c) and (c<=Ord('Z'));
end;

function FcIsLower(c : longint) : boolean;
begin
  FcIsLower:=(Ord('a')<=c) and (c<=Ord('z'));
end;

function FcToLower(c : longint) : longint;

var
  if_local1 : longint;
  
begin
  if FcIsUpper(c) then
    if_local1:=c-Ord('A')+Ord('a')
  else
    if_local1:=c;
  FcToLower:=if_local1;
end;



var
  hlib : tlibhandle;
  hcount : integer;
  loadedlib : string; 


Function FontConfigLibLoaded : Boolean;

begin
  Result:=hLib<>NilHandle;
end;

procedure Freefc;

begin
  if hlib<>NilHandle then
    FreeLibrary(hlib);
  hlib:=NilHandle;
  FcBlanksCreate:=nil;
  FcBlanksDestroy:=nil;
  FcBlanksAdd:=nil;
  FcBlanksIsMember:=nil;
  FcCacheDir:=nil;
  FcCacheCopySet:=nil;
  FcCacheSubdir:=nil;
  FcCacheNumSubdir:=nil;
  FcCacheNumFont:=nil;
  FcDirCacheUnlink:=nil;
  FcDirCacheValid:=nil;
  FcDirCacheClean:=nil;
  FcCacheCreateTagFile:=nil;
  FcConfigHome:=nil;
  FcConfigEnableHome:=nil;
  FcConfigFilename:=nil;
  FcConfigGetFilename:=nil;
  FcConfigCreate:=nil;
  FcConfigReference:=nil;
  FcConfigDestroy:=nil;
  FcConfigSetCurrent:=nil;
  FcConfigGetCurrent:=nil;
  FcConfigUptoDate:=nil;
  FcConfigBuildFonts:=nil;
  FcConfigGetFontDirs:=nil;
  FcConfigGetConfigDirs:=nil;
  FcConfigGetConfigFiles:=nil;
  FcConfigGetCache:=nil;
  FcConfigGetBlanks:=nil;
  FcConfigGetCacheDirs:=nil;
  FcConfigGetRescanInterval:=nil;
  FcConfigSetRescanInterval:=nil;
  FcConfigGetFonts:=nil;
  FcConfigAppFontAddFile:=nil;
  FcConfigAppFontAddDir:=nil;
  FcConfigAppFontClear:=nil;
  FcConfigSubstituteWithPat:=nil;
  FcConfigSubstitute:=nil;
  FcConfigGetSysRoot:=nil;
  FcConfigSetSysRoot:=nil;
  FcCharSetCreate:=nil;
  FcCharSetNew:=nil;
  FcCharSetDestroy:=nil;
  FcCharSetAddChar:=nil;
  FcCharSetDelChar:=nil;
  FcCharSetCopy:=nil;
  FcCharSetEqual:=nil;
  FcCharSetIntersect:=nil;
  FcCharSetUnion:=nil;
  FcCharSetSubtract:=nil;
  FcCharSetMerge:=nil;
  FcCharSetHasChar:=nil;
  FcCharSetCount:=nil;
  FcCharSetIntersectCount:=nil;
  FcCharSetSubtractCount:=nil;
  FcCharSetIsSubset:=nil;
  FcCharSetFirstPage:=nil;
  FcCharSetNextPage:=nil;
  FcCharSetCoverage:=nil;
  FcValuePrint:=nil;
  FcPatternPrint:=nil;
  FcFontSetPrint:=nil;
  FcGetDefaultLangs:=nil;
  FcDefaultSubstitute:=nil;
  FcFileIsDir:=nil;
  FcFileScan:=nil;
  FcDirScan:=nil;
  FcDirSave:=nil;
  FcDirCacheLoad:=nil;
  FcDirCacheRescan:=nil;
  FcDirCacheRead:=nil;
  FcDirCacheLoadFile:=nil;
  FcDirCacheUnload:=nil;
  FcFreeTypeQuery:=nil;
  FcFontSetCreate:=nil;
  FcFontSetDestroy:=nil;
  FcFontSetAdd:=nil;
  FcInitLoadConfig:=nil;
  FcInitLoadConfigAndFonts:=nil;
  FcInit:=nil;
  FcFini:=nil;
  FcGetVersion:=nil;
  FcInitReinitialize:=nil;
  FcInitBringUptoDate:=nil;
  FcGetLangs:=nil;
  FcLangNormalize:=nil;
  FcLangGetCharSet:=nil;
  FcLangSetCreate:=nil;
  FcLangSetDestroy:=nil;
  FcLangSetCopy:=nil;
  FcLangSetAdd:=nil;
  FcLangSetDel:=nil;
  FcLangSetHasLang:=nil;
  FcLangSetCompare:=nil;
  FcLangSetContains:=nil;
  FcLangSetEqual:=nil;
  FcLangSetHash:=nil;
  FcLangSetGetLangs:=nil;
  FcLangSetUnion:=nil;
  FcLangSetSubtract:=nil;
  FcObjectSetCreate:=nil;
  FcObjectSetAdd:=nil;
  FcObjectSetDestroy:=nil;
  FcObjectSetBuild:=nil;
  FcFontSetList:=nil;
  FcFontList:=nil;
  FcAtomicCreate:=nil;
  FcAtomicLock:=nil;
  FcAtomicNewFile:=nil;
  FcAtomicOrigFile:=nil;
  FcAtomicReplaceOrig:=nil;
  FcAtomicDeleteNew:=nil;
  FcAtomicUnlock:=nil;
  FcAtomicDestroy:=nil;
  FcFontSetMatch:=nil;
  FcFontMatch:=nil;
  FcFontRenderPrepare:=nil;
  FcFontSetSort:=nil;
  FcFontSort:=nil;
  FcFontSetSortDestroy:=nil;
  FcMatrixCopy:=nil;
  FcMatrixEqual:=nil;
  FcMatrixMultiply:=nil;
  FcMatrixRotate:=nil;
  FcMatrixScale:=nil;
  FcMatrixShear:=nil;
  FcNameRegisterObjectTypes:=nil;
  FcNameUnregisterObjectTypes:=nil;
  FcNameGetObjectType:=nil;
  FcNameRegisterConstants:=nil;
  FcNameUnregisterConstants:=nil;
  FcNameGetConstant:=nil;
  FcNameConstant:=nil;
  FcNameParse:=nil;
  FcNameUnparse:=nil;
  FcPatternCreate:=nil;
  FcPatternDuplicate:=nil;
  FcPatternReference:=nil;
  FcPatternFilter:=nil;
  FcValueDestroy:=nil;
  FcValueEqual:=nil;
  FcValueSave:=nil;
  FcPatternDestroy:=nil;
  FcPatternEqual:=nil;
  FcPatternEqualSubset:=nil;
  FcPatternHash:=nil;
  FcPatternAdd:=nil;
  FcPatternAddWeak:=nil;
  FcPatternGet:=nil;
  FcPatternDel:=nil;
  FcPatternRemove:=nil;
  FcPatternAddInteger:=nil;
  FcPatternAddDouble:=nil;
  FcPatternAddString:=nil;
  FcPatternAddMatrix:=nil;
  FcPatternAddCharSet:=nil;
  FcPatternAddBool:=nil;
  FcPatternAddLangSet:=nil;
  FcPatternAddRange:=nil;
  FcPatternGetInteger:=nil;
  FcPatternGetDouble:=nil;
  FcPatternGetString:=nil;
  FcPatternGetMatrix:=nil;
  FcPatternGetCharSet:=nil;
  FcPatternGetBool:=nil;
  FcPatternGetLangSet:=nil;
  FcPatternGetRange:=nil;
  FcPatternBuild:=nil;
  FcPatternBuild:=nil;
  FcPatternFormat:=nil;
  FcRangeCreateDouble:=nil;
  FcRangeCreateInteger:=nil;
  FcRangeDestroy:=nil;
  FcRangeCopy:=nil;
  FcRangeGetDouble:=nil;
  FcWeightFromOpenType:=nil;
  FcWeightToOpenType:=nil;
  FcStrCopy:=nil;
  FcStrCopyFilename:=nil;
  FcStrPlus:=nil;
  FcStrFree:=nil;
  FcStrDowncase:=nil;
  FcStrCmpIgnoreCase:=nil;
  FcStrCmp:=nil;
  FcStrStrIgnoreCase:=nil;
  FcStrStr:=nil;
  FcUtf8ToUcs4:=nil;
  FcUtf8Len:=nil;
  FcUcs4ToUtf8:=nil;
  FcUtf16ToUcs4:=nil;
  FcUtf16Len:=nil;
  FcStrDirname:=nil;
  FcStrBasename:=nil;
  FcStrSetCreate:=nil;
  FcStrSetMember:=nil;
  FcStrSetEqual:=nil;
  FcStrSetAdd:=nil;
  FcStrSetAddFilename:=nil;
  FcStrSetDel:=nil;
  FcStrSetDestroy:=nil;
  FcStrListCreate:=nil;
  FcStrListFirst:=nil;
  FcStrListNext:=nil;
  FcStrListDone:=nil;
  FcConfigParseAndLoad:=nil;
end;


Function Loadfc(lib : pchar) : Boolean;

begin
  Freefc;
  hlib:=LoadLibrary(lib);
  Result:=hlib<>NilHandle;
  If not Result then 
    exit;
  if (hlib=NilHandle) then
    raise EInOutError.CreateFmt('Could not load library: "%s"',[lib]);

  pointer(FcBlanksCreate):=GetProcAddress(hlib,'FcBlanksCreate');
  pointer(FcBlanksDestroy):=GetProcAddress(hlib,'FcBlanksDestroy');
  pointer(FcBlanksAdd):=GetProcAddress(hlib,'FcBlanksAdd');
  pointer(FcBlanksIsMember):=GetProcAddress(hlib,'FcBlanksIsMember');
  pointer(FcCacheDir):=GetProcAddress(hlib,'FcCacheDir');
  pointer(FcCacheCopySet):=GetProcAddress(hlib,'FcCacheCopySet');
  pointer(FcCacheSubdir):=GetProcAddress(hlib,'FcCacheSubdir');
  pointer(FcCacheNumSubdir):=GetProcAddress(hlib,'FcCacheNumSubdir');
  pointer(FcCacheNumFont):=GetProcAddress(hlib,'FcCacheNumFont');
  pointer(FcDirCacheUnlink):=GetProcAddress(hlib,'FcDirCacheUnlink');
  pointer(FcDirCacheValid):=GetProcAddress(hlib,'FcDirCacheValid');
  pointer(FcDirCacheClean):=GetProcAddress(hlib,'FcDirCacheClean');
  pointer(FcCacheCreateTagFile):=GetProcAddress(hlib,'FcCacheCreateTagFile');
  pointer(FcConfigHome):=GetProcAddress(hlib,'FcConfigHome');
  pointer(FcConfigEnableHome):=GetProcAddress(hlib,'FcConfigEnableHome');
  pointer(FcConfigFilename):=GetProcAddress(hlib,'FcConfigFilename');
  pointer(FcConfigGetFilename):=GetProcAddress(hlib,'FcConfigGetFilename');
  pointer(FcConfigCreate):=GetProcAddress(hlib,'FcConfigCreate');
  pointer(FcConfigReference):=GetProcAddress(hlib,'FcConfigReference');
  pointer(FcConfigDestroy):=GetProcAddress(hlib,'FcConfigDestroy');
  pointer(FcConfigSetCurrent):=GetProcAddress(hlib,'FcConfigSetCurrent');
  pointer(FcConfigGetCurrent):=GetProcAddress(hlib,'FcConfigGetCurrent');
  pointer(FcConfigUptoDate):=GetProcAddress(hlib,'FcConfigUptoDate');
  pointer(FcConfigBuildFonts):=GetProcAddress(hlib,'FcConfigBuildFonts');
  pointer(FcConfigGetFontDirs):=GetProcAddress(hlib,'FcConfigGetFontDirs');
  pointer(FcConfigGetConfigDirs):=GetProcAddress(hlib,'FcConfigGetConfigDirs');
  pointer(FcConfigGetConfigFiles):=GetProcAddress(hlib,'FcConfigGetConfigFiles');
  pointer(FcConfigGetCache):=GetProcAddress(hlib,'FcConfigGetCache');
  pointer(FcConfigGetBlanks):=GetProcAddress(hlib,'FcConfigGetBlanks');
  pointer(FcConfigGetCacheDirs):=GetProcAddress(hlib,'FcConfigGetCacheDirs');
  pointer(FcConfigGetRescanInterval):=GetProcAddress(hlib,'FcConfigGetRescanInterval');
  pointer(FcConfigSetRescanInterval):=GetProcAddress(hlib,'FcConfigSetRescanInterval');
  pointer(FcConfigGetFonts):=GetProcAddress(hlib,'FcConfigGetFonts');
  pointer(FcConfigAppFontAddFile):=GetProcAddress(hlib,'FcConfigAppFontAddFile');
  pointer(FcConfigAppFontAddDir):=GetProcAddress(hlib,'FcConfigAppFontAddDir');
  pointer(FcConfigAppFontClear):=GetProcAddress(hlib,'FcConfigAppFontClear');
  pointer(FcConfigSubstituteWithPat):=GetProcAddress(hlib,'FcConfigSubstituteWithPat');
  pointer(FcConfigSubstitute):=GetProcAddress(hlib,'FcConfigSubstitute');
  pointer(FcConfigGetSysRoot):=GetProcAddress(hlib,'FcConfigGetSysRoot');
  pointer(FcConfigSetSysRoot):=GetProcAddress(hlib,'FcConfigSetSysRoot');
  pointer(FcCharSetCreate):=GetProcAddress(hlib,'FcCharSetCreate');
  pointer(FcCharSetNew):=GetProcAddress(hlib,'FcCharSetNew');
  pointer(FcCharSetDestroy):=GetProcAddress(hlib,'FcCharSetDestroy');
  pointer(FcCharSetAddChar):=GetProcAddress(hlib,'FcCharSetAddChar');
  pointer(FcCharSetDelChar):=GetProcAddress(hlib,'FcCharSetDelChar');
  pointer(FcCharSetCopy):=GetProcAddress(hlib,'FcCharSetCopy');
  pointer(FcCharSetEqual):=GetProcAddress(hlib,'FcCharSetEqual');
  pointer(FcCharSetIntersect):=GetProcAddress(hlib,'FcCharSetIntersect');
  pointer(FcCharSetUnion):=GetProcAddress(hlib,'FcCharSetUnion');
  pointer(FcCharSetSubtract):=GetProcAddress(hlib,'FcCharSetSubtract');
  pointer(FcCharSetMerge):=GetProcAddress(hlib,'FcCharSetMerge');
  pointer(FcCharSetHasChar):=GetProcAddress(hlib,'FcCharSetHasChar');
  pointer(FcCharSetCount):=GetProcAddress(hlib,'FcCharSetCount');
  pointer(FcCharSetIntersectCount):=GetProcAddress(hlib,'FcCharSetIntersectCount');
  pointer(FcCharSetSubtractCount):=GetProcAddress(hlib,'FcCharSetSubtractCount');
  pointer(FcCharSetIsSubset):=GetProcAddress(hlib,'FcCharSetIsSubset');
  pointer(FcCharSetFirstPage):=GetProcAddress(hlib,'FcCharSetFirstPage');
  pointer(FcCharSetNextPage):=GetProcAddress(hlib,'FcCharSetNextPage');
  pointer(FcCharSetCoverage):=GetProcAddress(hlib,'FcCharSetCoverage');
  pointer(FcValuePrint):=GetProcAddress(hlib,'FcValuePrint');
  pointer(FcPatternPrint):=GetProcAddress(hlib,'FcPatternPrint');
  pointer(FcFontSetPrint):=GetProcAddress(hlib,'FcFontSetPrint');
  pointer(FcGetDefaultLangs):=GetProcAddress(hlib,'FcGetDefaultLangs');
  pointer(FcDefaultSubstitute):=GetProcAddress(hlib,'FcDefaultSubstitute');
  pointer(FcFileIsDir):=GetProcAddress(hlib,'FcFileIsDir');
  pointer(FcFileScan):=GetProcAddress(hlib,'FcFileScan');
  pointer(FcDirScan):=GetProcAddress(hlib,'FcDirScan');
  pointer(FcDirSave):=GetProcAddress(hlib,'FcDirSave');
  pointer(FcDirCacheLoad):=GetProcAddress(hlib,'FcDirCacheLoad');
  pointer(FcDirCacheRescan):=GetProcAddress(hlib,'FcDirCacheRescan');
  pointer(FcDirCacheRead):=GetProcAddress(hlib,'FcDirCacheRead');
  pointer(FcDirCacheLoadFile):=GetProcAddress(hlib,'FcDirCacheLoadFile');
  pointer(FcDirCacheUnload):=GetProcAddress(hlib,'FcDirCacheUnload');
  pointer(FcFreeTypeQuery):=GetProcAddress(hlib,'FcFreeTypeQuery');
  pointer(FcFontSetCreate):=GetProcAddress(hlib,'FcFontSetCreate');
  pointer(FcFontSetDestroy):=GetProcAddress(hlib,'FcFontSetDestroy');
  pointer(FcFontSetAdd):=GetProcAddress(hlib,'FcFontSetAdd');
  pointer(FcInitLoadConfig):=GetProcAddress(hlib,'FcInitLoadConfig');
  pointer(FcInitLoadConfigAndFonts):=GetProcAddress(hlib,'FcInitLoadConfigAndFonts');
  pointer(FcInit):=GetProcAddress(hlib,'FcInit');
  pointer(FcFini):=GetProcAddress(hlib,'FcFini');
  pointer(FcGetVersion):=GetProcAddress(hlib,'FcGetVersion');
  pointer(FcInitReinitialize):=GetProcAddress(hlib,'FcInitReinitialize');
  pointer(FcInitBringUptoDate):=GetProcAddress(hlib,'FcInitBringUptoDate');
  pointer(FcGetLangs):=GetProcAddress(hlib,'FcGetLangs');
  pointer(FcLangNormalize):=GetProcAddress(hlib,'FcLangNormalize');
  pointer(FcLangGetCharSet):=GetProcAddress(hlib,'FcLangGetCharSet');
  pointer(FcLangSetCreate):=GetProcAddress(hlib,'FcLangSetCreate');
  pointer(FcLangSetDestroy):=GetProcAddress(hlib,'FcLangSetDestroy');
  pointer(FcLangSetCopy):=GetProcAddress(hlib,'FcLangSetCopy');
  pointer(FcLangSetAdd):=GetProcAddress(hlib,'FcLangSetAdd');
  pointer(FcLangSetDel):=GetProcAddress(hlib,'FcLangSetDel');
  pointer(FcLangSetHasLang):=GetProcAddress(hlib,'FcLangSetHasLang');
  pointer(FcLangSetCompare):=GetProcAddress(hlib,'FcLangSetCompare');
  pointer(FcLangSetContains):=GetProcAddress(hlib,'FcLangSetContains');
  pointer(FcLangSetEqual):=GetProcAddress(hlib,'FcLangSetEqual');
  pointer(FcLangSetHash):=GetProcAddress(hlib,'FcLangSetHash');
  pointer(FcLangSetGetLangs):=GetProcAddress(hlib,'FcLangSetGetLangs');
  pointer(FcLangSetUnion):=GetProcAddress(hlib,'FcLangSetUnion');
  pointer(FcLangSetSubtract):=GetProcAddress(hlib,'FcLangSetSubtract');
  pointer(FcObjectSetCreate):=GetProcAddress(hlib,'FcObjectSetCreate');
  pointer(FcObjectSetAdd):=GetProcAddress(hlib,'FcObjectSetAdd');
  pointer(FcObjectSetDestroy):=GetProcAddress(hlib,'FcObjectSetDestroy');
  pointer(FcObjectSetBuild):=GetProcAddress(hlib,'FcObjectSetBuild');
  pointer(FcObjectSetBuild):=GetProcAddress(hlib,'FcObjectSetBuild');
  pointer(FcFontSetList):=GetProcAddress(hlib,'FcFontSetList');
  pointer(FcFontList):=GetProcAddress(hlib,'FcFontList');
  pointer(FcAtomicCreate):=GetProcAddress(hlib,'FcAtomicCreate');
  pointer(FcAtomicLock):=GetProcAddress(hlib,'FcAtomicLock');
  pointer(FcAtomicNewFile):=GetProcAddress(hlib,'FcAtomicNewFile');
  pointer(FcAtomicOrigFile):=GetProcAddress(hlib,'FcAtomicOrigFile');
  pointer(FcAtomicReplaceOrig):=GetProcAddress(hlib,'FcAtomicReplaceOrig');
  pointer(FcAtomicDeleteNew):=GetProcAddress(hlib,'FcAtomicDeleteNew');
  pointer(FcAtomicUnlock):=GetProcAddress(hlib,'FcAtomicUnlock');
  pointer(FcAtomicDestroy):=GetProcAddress(hlib,'FcAtomicDestroy');
  pointer(FcFontSetMatch):=GetProcAddress(hlib,'FcFontSetMatch');
  pointer(FcFontMatch):=GetProcAddress(hlib,'FcFontMatch');
  pointer(FcFontRenderPrepare):=GetProcAddress(hlib,'FcFontRenderPrepare');
  pointer(FcFontSetSort):=GetProcAddress(hlib,'FcFontSetSort');
  pointer(FcFontSort):=GetProcAddress(hlib,'FcFontSort');
  pointer(FcFontSetSortDestroy):=GetProcAddress(hlib,'FcFontSetSortDestroy');
  pointer(FcMatrixCopy):=GetProcAddress(hlib,'FcMatrixCopy');
  pointer(FcMatrixEqual):=GetProcAddress(hlib,'FcMatrixEqual');
  pointer(FcMatrixMultiply):=GetProcAddress(hlib,'FcMatrixMultiply');
  pointer(FcMatrixRotate):=GetProcAddress(hlib,'FcMatrixRotate');
  pointer(FcMatrixScale):=GetProcAddress(hlib,'FcMatrixScale');
  pointer(FcMatrixShear):=GetProcAddress(hlib,'FcMatrixShear');
  pointer(FcNameRegisterObjectTypes):=GetProcAddress(hlib,'FcNameRegisterObjectTypes');
  pointer(FcNameUnregisterObjectTypes):=GetProcAddress(hlib,'FcNameUnregisterObjectTypes');
  pointer(FcNameGetObjectType):=GetProcAddress(hlib,'FcNameGetObjectType');
  pointer(FcNameRegisterConstants):=GetProcAddress(hlib,'FcNameRegisterConstants');
  pointer(FcNameUnregisterConstants):=GetProcAddress(hlib,'FcNameUnregisterConstants');
  pointer(FcNameGetConstant):=GetProcAddress(hlib,'FcNameGetConstant');
  pointer(FcNameConstant):=GetProcAddress(hlib,'FcNameConstant');
  pointer(FcNameParse):=GetProcAddress(hlib,'FcNameParse');
  pointer(FcNameUnparse):=GetProcAddress(hlib,'FcNameUnparse');
  pointer(FcPatternCreate):=GetProcAddress(hlib,'FcPatternCreate');
  pointer(FcPatternDuplicate):=GetProcAddress(hlib,'FcPatternDuplicate');
  pointer(FcPatternReference):=GetProcAddress(hlib,'FcPatternReference');
  pointer(FcPatternFilter):=GetProcAddress(hlib,'FcPatternFilter');
  pointer(FcValueDestroy):=GetProcAddress(hlib,'FcValueDestroy');
  pointer(FcValueEqual):=GetProcAddress(hlib,'FcValueEqual');
  pointer(FcValueSave):=GetProcAddress(hlib,'FcValueSave');
  pointer(FcPatternDestroy):=GetProcAddress(hlib,'FcPatternDestroy');
  pointer(FcPatternEqual):=GetProcAddress(hlib,'FcPatternEqual');
  pointer(FcPatternEqualSubset):=GetProcAddress(hlib,'FcPatternEqualSubset');
  pointer(FcPatternHash):=GetProcAddress(hlib,'FcPatternHash');
  pointer(FcPatternAdd):=GetProcAddress(hlib,'FcPatternAdd');
  pointer(FcPatternAddWeak):=GetProcAddress(hlib,'FcPatternAddWeak');
  pointer(FcPatternGet):=GetProcAddress(hlib,'FcPatternGet');
  pointer(FcPatternDel):=GetProcAddress(hlib,'FcPatternDel');
  pointer(FcPatternRemove):=GetProcAddress(hlib,'FcPatternRemove');
  pointer(FcPatternAddInteger):=GetProcAddress(hlib,'FcPatternAddInteger');
  pointer(FcPatternAddDouble):=GetProcAddress(hlib,'FcPatternAddDouble');
  pointer(FcPatternAddString):=GetProcAddress(hlib,'FcPatternAddString');
  pointer(FcPatternAddMatrix):=GetProcAddress(hlib,'FcPatternAddMatrix');
  pointer(FcPatternAddCharSet):=GetProcAddress(hlib,'FcPatternAddCharSet');
  pointer(FcPatternAddBool):=GetProcAddress(hlib,'FcPatternAddBool');
  pointer(FcPatternAddLangSet):=GetProcAddress(hlib,'FcPatternAddLangSet');
  pointer(FcPatternAddRange):=GetProcAddress(hlib,'FcPatternAddRange');
  pointer(FcPatternGetInteger):=GetProcAddress(hlib,'FcPatternGetInteger');
  pointer(FcPatternGetDouble):=GetProcAddress(hlib,'FcPatternGetDouble');
  pointer(FcPatternGetString):=GetProcAddress(hlib,'FcPatternGetString');
  pointer(FcPatternGetMatrix):=GetProcAddress(hlib,'FcPatternGetMatrix');
  pointer(FcPatternGetCharSet):=GetProcAddress(hlib,'FcPatternGetCharSet');
  pointer(FcPatternGetBool):=GetProcAddress(hlib,'FcPatternGetBool');
  pointer(FcPatternGetLangSet):=GetProcAddress(hlib,'FcPatternGetLangSet');
  pointer(FcPatternGetRange):=GetProcAddress(hlib,'FcPatternGetRange');
  pointer(FcPatternBuild):=GetProcAddress(hlib,'FcPatternBuild');
  pointer(FcPatternBuild):=GetProcAddress(hlib,'FcPatternBuild');
  pointer(FcPatternFormat):=GetProcAddress(hlib,'FcPatternFormat');
  pointer(FcRangeCreateDouble):=GetProcAddress(hlib,'FcRangeCreateDouble');
  pointer(FcRangeCreateInteger):=GetProcAddress(hlib,'FcRangeCreateInteger');
  pointer(FcRangeDestroy):=GetProcAddress(hlib,'FcRangeDestroy');
  pointer(FcRangeCopy):=GetProcAddress(hlib,'FcRangeCopy');
  pointer(FcRangeGetDouble):=GetProcAddress(hlib,'FcRangeGetDouble');
  pointer(FcWeightFromOpenType):=GetProcAddress(hlib,'FcWeightFromOpenType');
  pointer(FcWeightToOpenType):=GetProcAddress(hlib,'FcWeightToOpenType');
  pointer(FcStrCopy):=GetProcAddress(hlib,'FcStrCopy');
  pointer(FcStrCopyFilename):=GetProcAddress(hlib,'FcStrCopyFilename');
  pointer(FcStrPlus):=GetProcAddress(hlib,'FcStrPlus');
  pointer(FcStrFree):=GetProcAddress(hlib,'FcStrFree');
  pointer(FcStrDowncase):=GetProcAddress(hlib,'FcStrDowncase');
  pointer(FcStrCmpIgnoreCase):=GetProcAddress(hlib,'FcStrCmpIgnoreCase');
  pointer(FcStrCmp):=GetProcAddress(hlib,'FcStrCmp');
  pointer(FcStrStrIgnoreCase):=GetProcAddress(hlib,'FcStrStrIgnoreCase');
  pointer(FcStrStr):=GetProcAddress(hlib,'FcStrStr');
  pointer(FcUtf8ToUcs4):=GetProcAddress(hlib,'FcUtf8ToUcs4');
  pointer(FcUtf8Len):=GetProcAddress(hlib,'FcUtf8Len');
  pointer(FcUcs4ToUtf8):=GetProcAddress(hlib,'FcUcs4ToUtf8');
  pointer(FcUtf16ToUcs4):=GetProcAddress(hlib,'FcUtf16ToUcs4');
  pointer(FcUtf16Len):=GetProcAddress(hlib,'FcUtf16Len');
  pointer(FcStrDirname):=GetProcAddress(hlib,'FcStrDirname');
  pointer(FcStrBasename):=GetProcAddress(hlib,'FcStrBasename');
  pointer(FcStrSetCreate):=GetProcAddress(hlib,'FcStrSetCreate');
  pointer(FcStrSetMember):=GetProcAddress(hlib,'FcStrSetMember');
  pointer(FcStrSetEqual):=GetProcAddress(hlib,'FcStrSetEqual');
  pointer(FcStrSetAdd):=GetProcAddress(hlib,'FcStrSetAdd');
  pointer(FcStrSetAddFilename):=GetProcAddress(hlib,'FcStrSetAddFilename');
  pointer(FcStrSetDel):=GetProcAddress(hlib,'FcStrSetDel');
  pointer(FcStrSetDestroy):=GetProcAddress(hlib,'FcStrSetDestroy');
  pointer(FcStrListCreate):=GetProcAddress(hlib,'FcStrListCreate');
  pointer(FcStrListFirst):=GetProcAddress(hlib,'FcStrListFirst');
  pointer(FcStrListNext):=GetProcAddress(hlib,'FcStrListNext');
  pointer(FcStrListDone):=GetProcAddress(hlib,'FcStrListDone');
  pointer(FcConfigParseAndLoad):=GetProcAddress(hlib,'FcConfigParseAndLoad');
end;

Function LoadFontConfigLib(Const ALibName : String; RaiseError : Boolean = true) : Integer;

Var
  FN : String;

begin
  Result:=-1;
  FN:=ALibName;
  if FN='' then
    FN:=DefaultLibName;
  if (HCount>0) then
    begin
    if (FN=LoadedLib) then
      Inc(HCount)
    else if RaiseError then  
      Raise EInoutError.CreateFmt('Cannot load "%s", FontConfig library already loaded as "%s"',[FN,LoadedLib])
    else 
      Exit;  
    end
  else
    begin
    if LoadFC(PChar(FN)) then
      begin
      inc(HCount);
      LoadedLib:=FN;
      end
    else if RaiseError then
      raise EInOutError.CreateFmt('Could not load library: "%s"',[FN])
    else
      Exit;  
    end;
  Result:=HCount;
end;

Function UnLoadFontConfigLib : Integer;

begin
  if (hCount>0) then
    begin
    Dec(HCount);
    if (HCount=0) then
      FreeFC;
    end;
  Result:=HCount;
end;

end.
