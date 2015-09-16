type
  Bool = WordBool;
  
  UINT = Word;
  LONG = LongInt;
  
  WPARAM = UINT;
  LPARAM = LONG;
  LRESULT = LONG;
  
  { The Win16 C headers define the P-prefixed types - PSTR, etc. as near pointers.
    Borland Pascal 7 defines them as far pointers (in other words, the same as the
    LP-prefixed type - LPSTR)  We define them as the default pointer type for the
    current memory model. This means we'll be BP7 compatible in the large memory
    model (which is the only memory model supported by BP7).

    Also, using memory models other than 'large' under win16 is somewhat nasty and
    is better to be avoided. }
  PSTR = ^Char;
  NPSTR = ^Char; near;
//  LPSTR = ^Char; far;
  LPCSTR = ^Char; far;
  
  { PBYTE is already defined in system }
  LPBYTE = ^Byte; far;
  
  PINT = ^SmallInt;
  LPINT = ^SmallInt; far;
  
  { PWORD is already defined in system }
  LPWORD = ^Word; far;

  PUINT = ^UINT;
  LPUINT = ^UINT; far;
  
  PLONG = ^LONG;
  LPLONG = ^LONG; far;
  
  { PDWORD is already defined in system }
  LPDWORD = ^DWORD; far;
  
  LPVOID = FarPointer;
  
  FARPROC = FarPointer;
  TFarProc = FARPROC;

  HANDLE = THandle;
  PHANDLE = ^THandle;
  SPHANDLE = ^THandle; near;
  LPHANDLE = ^THandle; far;
  
//  HGLOBAL = THandle;
  HLOCAL = THandle;
  
  TGlobalHandle = THandle;
  TLocalHandle = THandle;
  
  ATOM = UINT;
  TAtom = ATOM;
  
  HINST = THandle; { instead of HINSTANCE, to avoid conflict with var hInstance }
//  HMODULE = HINST;
  
const
  { GetWinFlags result mask values }
  WF_PMODE      = $0001;
  WF_CPU286     = $0002;
  WF_CPU386     = $0004;
  WF_CPU486     = $0008;
  WF_STANDARD   = $0010;
  WF_WIN286     = $0010;
  WF_ENHANCED   = $0020;
  WF_WIN386     = $0020;
  WF_CPU086     = $0040;
  WF_CPU186     = $0080;
  WF_LARGEFRAME = $0100;
  WF_SMALLFRAME = $0200;
  WF_80x87      = $0400;
  WF_PAGING     = $0800;
  WF_WLO        = $8000;

{ ExitWindows values }
  EW_RESTARTWINDOWS = $42;

{ SetErrorMode() constants }
  SEM_FAILCRITICALERRORS = $0001;
  SEM_NOGPFAULTERRORBOX  = $0002;
  SEM_NOOPENFILEERRORBOX = $8000;

type
  LPCATCHBUF = ^CATCHBUF; far;
  CATCHBUF = array [0..8] of SmallInt;
  PCatchBuf = ^TCatchBuf;
  TCatchBuf = CATCHBUF;

const
  HINSTANCE_ERROR = HINST(32);

{ Windows Exit Procedure flag values }
  WEP_SYSTEM_EXIT = 1;
  WEP_FREE_DLL    = 0;

type
  LPSEGINFO = ^SEGINFO; far;
  SEGINFO = record
    offSegment: UINT;
    cbSegment: UINT;
    flags: UINT;
    cbAlloc: UINT;
    h: HGLOBAL;
    alignShift: UINT;
    reserved: array [0..1] of UINT;
  end;
  PSegInfo = ^TSegInfo;
  TSegInfo = SEGINFO;

  HTASK = THandle;

const
{ Global Memory Flags }
  GMEM_FIXED       = $0000;
  GMEM_MOVEABLE    = $0002;
  GMEM_NOCOMPACT   = $0010;
  GMEM_NODISCARD   = $0020;
  GMEM_ZEROINIT    = $0040;
  GMEM_MODIFY      = $0080;
  GMEM_DISCARDABLE = $0100;
  GMEM_NOT_BANKED  = $1000;
  GMEM_SHARE       = $2000;
  GMEM_DDESHARE    = $2000;
  GMEM_NOTIFY      = $4000;
  GMEM_LOWER       = GMEM_NOT_BANKED;

  GHND             = GMEM_MOVEABLE or GMEM_ZEROINIT;
  GPTR             = GMEM_FIXED or GMEM_ZEROINIT;

{ GlobalFlags return flags (in addition to GMEM_DISCARDABLE) }
  GMEM_DISCARDED   = $4000;
  GMEM_LOCKCOUNT   = $00FF;

{ Low system memory notification message }
  WM_COMPACTING    = $0041;

type
  GNOTIFYPROC = function(hGlbl: HGLOBAL): BOOL; far;

const
{ Local Memory Flags }
  LMEM_FIXED       = $0000;
  LMEM_MOVEABLE    = $0002;
  LMEM_NOCOMPACT   = $0010;
  LMEM_NODISCARD   = $0020;
  LMEM_ZEROINIT    = $0040;
  LMEM_MODIFY      = $0080;
  LMEM_DISCARDABLE = $0F00;

  LHND             = LMEM_MOVEABLE or LMEM_ZEROINIT;
  LPTR             = LMEM_FIXED or LMEM_ZEROINIT;

  NONZEROLHND      = LMEM_MOVEABLE;
  NONZEROLPTR      = LMEM_FIXED;

{ LocalFlags return flags (in addition to LMEM_DISCARDABLE) }
  LMEM_DISCARDED   = $4000;
  LMEM_LOCKCOUNT   = $00FF;

{ File I/O }
type
  HFILE = SmallInt;

const
  HFILE_ERROR = HFILE(-1);

type
{ OpenFile() Structure }
  POFSTRUCT = ^OFSTRUCT;
  NPOFSTRUCT = ^OFSTRUCT; near;
  LPOFSTRUCT = ^OFSTRUCT; far;
  OFSTRUCT = record
    cBytes: BYTE;
    fFixedDisk: BYTE;
    nErrCode: UINT;
    reserved: array [0..3] of BYTE;
    szPathName: array [0..127] of char;
  end;
  TOFStruct = OFSTRUCT;

const
{ OpenFile() Flags }
  OF_READ             = $0000;
  OF_WRITE            = $0001;
  OF_READWRITE        = $0002;
  OF_SHARE_COMPAT     = $0000;
  OF_SHARE_EXCLUSIVE  = $0010;
  OF_SHARE_DENY_WRITE = $0020;
  OF_SHARE_DENY_READ  = $0030;
  OF_SHARE_DENY_NONE  = $0040;
  OF_PARSE            = $0100;
  OF_DELETE           = $0200;
  OF_VERIFY           = $0400;      { Used with OF_REOPEN }
  OF_SEARCH           = $0400;      { Used without OF_REOPEN }
  OF_CANCEL           = $0800;
  OF_CREATE           = $1000;
  OF_PROMPT           = $2000;
  OF_EXIST            = $4000;
  OF_REOPEN           = $8000;

{ _lopen() flags }
  _READ      = 0;
  _WRITE     = 1;
  READ_WRITE = 2;

{ _llseek origin values }
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

{ GetTempFileName() Flags }
  TF_FORCEDRIVE = BYTE($80);

{ GetDriveType return values }
  DRIVE_REMOVABLE = 2;
  DRIVE_FIXED     = 3;
  DRIVE_REMOTE    = 4;

{ Network support }
{ Errors }
  WN_SUCCESS           = $0000;
  WN_NOT_SUPPORTED     = $0001;
  WN_NET_ERROR         = $0002;
  WN_MORE_DATA         = $0003;
  WN_BAD_POINTER       = $0004;
  WN_BAD_VALUE         = $0005;
  WN_BAD_PASSWORD      = $0006;
  WN_ACCESS_DENIED     = $0007;
  WN_FUNCTION_BUSY     = $0008;
  WN_WINDOWS_ERROR     = $0009;
  WN_BAD_USER          = $000A;
  WN_OUT_OF_MEMORY     = $000B;
  WN_CANCEL            = $000C;
  WN_CONTINUE          = $000D;

{ Connection errors }
  WN_NOT_CONNECTED     = $0030;
  WN_OPEN_FILES        = $0031;
  WN_BAD_NETNAME       = $0032;
  WN_BAD_LOCALNAME     = $0033;
  WN_ALREADY_CONNECTED = $0034;
  WN_DEVICE_ERROR      = $0035;
  WN_CONNECTION_CLOSED = $0036;

type
{ Resource Management }
  HRSRC = THandle;
  RSRCHDLRPROC = function(hMem: HGLOBAL; hInstance: HINST; hResInfo: HRSRC): HGLOBAL;far;
  MAKEINTRESOURCE = LPCSTR;

const
{ Predefined Resource Types }
  RT_CURSOR       = MAKEINTRESOURCE(1);
  RT_BITMAP       = MAKEINTRESOURCE(2);
  RT_ICON         = MAKEINTRESOURCE(3);
  RT_MENU         = MAKEINTRESOURCE(4);
  RT_DIALOG       = MAKEINTRESOURCE(5);
  RT_STRING       = MAKEINTRESOURCE(6);
  RT_FONTDIR      = MAKEINTRESOURCE(7);
  RT_FONT         = MAKEINTRESOURCE(8);
  RT_ACCELERATOR  = MAKEINTRESOURCE(9);
  RT_RCDATA       = MAKEINTRESOURCE(10);

  RT_GROUP_CURSOR = MAKEINTRESOURCE(12);
  RT_GROUP_ICON   = MAKEINTRESOURCE(14);

{ OEM Resource Ordinal Numbers }
  OBM_CLOSE       = 32754;
  OBM_UPARROW     = 32753;
  OBM_DNARROW     = 32752;
  OBM_RGARROW     = 32751;
  OBM_LFARROW     = 32750;
  OBM_REDUCE      = 32749;
  OBM_ZOOM        = 32748;
  OBM_RESTORE     = 32747;
  OBM_REDUCED     = 32746;
  OBM_ZOOMD       = 32745;
  OBM_RESTORED    = 32744;
  OBM_UPARROWD    = 32743;
  OBM_DNARROWD    = 32742;
  OBM_RGARROWD    = 32741;
  OBM_LFARROWD    = 32740;
  OBM_MNARROW     = 32739;
  OBM_COMBO       = 32738;

  OBM_OLD_CLOSE   = 32767;
  OBM_SIZE        = 32766;
  OBM_OLD_UPARROW = 32765;
  OBM_OLD_DNARROW = 32764;
  OBM_OLD_RGARROW = 32763;
  OBM_OLD_LFARROW = 32762;
  OBM_BTSIZE      = 32761;
  OBM_CHECK       = 32760;
  OBM_CHECKBOXES  = 32759;
  OBM_BTNCORNERS  = 32758;
  OBM_OLD_REDUCE  = 32757;
  OBM_OLD_ZOOM    = 32756;
  OBM_OLD_RESTORE = 32755;

  OCR_NORMAL      = 32512;
  OCR_IBEAM       = 32513;
  OCR_WAIT        = 32514;
  OCR_CROSS       = 32515;
  OCR_UP          = 32516;
  OCR_SIZE        = 32640;
  OCR_ICON        = 32641;
  OCR_SIZENWSE    = 32642;
  OCR_SIZENESW    = 32643;
  OCR_SIZEWE      = 32644;
  OCR_SIZENS      = 32645;
  OCR_SIZEALL     = 32646;
  OCR_ICOCUR      = 32647;

  OIC_SAMPLE      = 32512;
  OIC_HAND        = 32513;
  OIC_QUES        = 32514;
  OIC_BANG        = 32515;
  OIC_NOTE        = 32516;

type
{ Atom Management }
  MAKEINTATOM = LPCSTR;

const
{ WIN.INI Support }
  WM_WININICHANGE = $001A;

type
{ GDI typedefs, structures, and functions }
  HDC      = THandle;
  HGDIOBJ  = THandle;
  HBITMAP  = THandle;
  HPEN     = THandle;
  HBRUSH   = THandle;
  HRGN     = THandle;
  HPALETTE = THandle;
  HFONT    = THandle;

  PRECT = ^RECT;
  NPRECT = ^RECT; near;
  LPRECT = ^RECT; far;
  RECT = record
    left: SmallInt;
    top: SmallInt;
    right: SmallInt;
    bottom: SmallInt;
  end;
  TRect = RECT;

  PPOINT = ^POINT;
  NPPOINT = ^POINT; near;
  LPPOINT = ^POINT; far;
  POINT = record
    x: SmallInt;
    y: SmallInt;
  end;
  TPoint = POINT;

  MAKEPOINT = POINT;

{ Device Capabilities }
const
{ Device Parameters for GetDeviceCaps() }
  DRIVERVERSION = 0;
  TECHNOLOGY    = 2;
  HORZSIZE      = 4;
  VERTSIZE      = 6;
  HORZRES       = 8;
  VERTRES       = 10;
  BITSPIXEL     = 12;
  PLANES        = 14;
  NUMBRUSHES    = 16;
  NUMPENS       = 18;
  NUMMARKERS    = 20;
  NUMFONTS      = 22;
  NUMCOLORS     = 24;
  PDEVICESIZE   = 26;
  CURVECAPS     = 28;
  LINECAPS      = 30;
  POLYGONALCAPS = 32;
  TEXTCAPS      = 34;
  CLIPCAPS      = 36;
  RASTERCAPS    = 38;
  ASPECTX       = 40;
  ASPECTY       = 42;
  ASPECTXY      = 44;

  LOGPIXELSX    = 88;
  LOGPIXELSY    = 90;

  SIZEPALETTE   = 104;
  NUMRESERVED   = 106;
  COLORRES      = 108;

{ GetDeviceCaps() return value masks }

{ TECHNOLOGY }
  DT_PLOTTER          = 0;
  DT_RASDISPLAY       = 1;
  DT_RASPRINTER       = 2;
  DT_RASCAMERA        = 3;
  DT_CHARSTREAM       = 4;
  DT_METAFILE         = 5;
  DT_DISPFILE         = 6;

{ CURVECAPS }
  CC_NONE             = $0000;
  CC_CIRCLES          = $0001;
  CC_PIE              = $0002;
  CC_CHORD            = $0004;
  CC_ELLIPSES         = $0008;
  CC_WIDE             = $0010;
  CC_STYLED           = $0020;
  CC_WIDESTYLED       = $0040;
  CC_INTERIORS        = $0080;
  CC_ROUNDRECT        = $0100;

{ LINECAPS }
  LC_NONE             = $0000;
  LC_POLYLINE         = $0002;
  LC_MARKER           = $0004;
  LC_POLYMARKER       = $0008;
  LC_WIDE             = $0010;
  LC_STYLED           = $0020;
  LC_WIDESTYLED       = $0040;
  LC_INTERIORS        = $0080;

{ POLYGONALCAPS }
  PC_NONE             = $0000;
  PC_POLYGON          = $0001;
  PC_RECTANGLE        = $0002;
  PC_WINDPOLYGON      = $0004;
  PC_SCANLINE         = $0008;
  PC_WIDE             = $0010;
  PC_STYLED           = $0020;
  PC_WIDESTYLED       = $0040;
  PC_INTERIORS        = $0080;

{ TEXTCAPS }
  TC_OP_CHARACTER     = $0001;
  TC_OP_STROKE        = $0002;
  TC_CP_STROKE        = $0004;
  TC_CR_90            = $0008;
  TC_CR_ANY           = $0010;
  TC_SF_X_YINDEP      = $0020;
  TC_SA_DOUBLE        = $0040;
  TC_SA_INTEGER       = $0080;
  TC_SA_CONTIN        = $0100;
  TC_EA_DOUBLE        = $0200;
  TC_IA_ABLE          = $0400;
  TC_UA_ABLE          = $0800;
  TC_SO_ABLE          = $1000;
  TC_RA_ABLE          = $2000;
  TC_VA_ABLE          = $4000;
  TC_RESERVED         = $8000;

{ CLIPCAPS }
{$ifndef SYSTEMUNIT}
  CP_NONE             = $0000;
{$endif SYSTEMUNIT}
  CP_RECTANGLE        = $0001;
  CP_REGION           = $0002;

{ RASTERCAPS }
  RC_NONE             = $0000;
  RC_BITBLT           = $0001;
  RC_BANDING          = $0002;
  RC_SCALING          = $0004;
  RC_BITMAP64         = $0008;
  RC_GDI20_OUTPUT     = $0010;
  RC_GDI20_STATE      = $0020;
  RC_SAVEBITMAP       = $0040;
  RC_DI_BITMAP        = $0080;
  RC_PALETTE          = $0100;
  RC_DIBTODEV         = $0200;
  RC_BIGFONT          = $0400;
  RC_STRETCHBLT       = $0800;
  RC_FLOODFILL        = $1000;
  RC_STRETCHDIB       = $2000;
  RC_OP_DX_OUTPUT     = $4000;
  RC_DEVBITS          = $8000;

{ Coordinate transformation support }

{ Map modes }
  MM_TEXT        = 1;
  MM_LOMETRIC    = 2;
  MM_HIMETRIC    = 3;
  MM_LOENGLISH   = 4;
  MM_HIENGLISH   = 5;
  MM_TWIPS       = 6;
  MM_ISOTROPIC   = 7;
  MM_ANISOTROPIC = 8;

{ Coordinate Modes }
  ABSOLUTE = 1;
  RELATIVE = 2;

{ Color support }
type
  PCOLORREF = ^COLORREF;
  LPCOLORREF = ^COLORREF; far;
  COLORREF = DWORD;
  TColorRef = COLORREF;

const
  COLOR_SCROLLBAR       = 0;
  COLOR_BACKGROUND      = 1;
  COLOR_ACTIVECAPTION   = 2;
  COLOR_INACTIVECAPTION = 3;
  COLOR_MENU            = 4;
  COLOR_WINDOW          = 5;
  COLOR_WINDOWFRAME     = 6;
  COLOR_MENUTEXT        = 7;
  COLOR_WINDOWTEXT      = 8;
  COLOR_CAPTIONTEXT     = 9;
  COLOR_ACTIVEBORDER    = 10;
  COLOR_INACTIVEBORDER  = 11;
  COLOR_APPWORKSPACE    = 12;
  COLOR_HIGHLIGHT       = 13;
  COLOR_HIGHLIGHTTEXT   = 14;
  COLOR_BTNFACE         = 15;
  COLOR_BTNSHADOW       = 16;
  COLOR_GRAYTEXT        = 17;
  COLOR_BTNTEXT         = 18;

  WM_SYSCOLORCHANGE = $0015;

type
{ GDI Object Support }
  GOBJENUMPROC = function(lpLogObject: FarPointer; lpData: LPARAM): SmallInt; far;

const
{ Object types for EnumObjects() }
  OBJ_PEN   = 1;
  OBJ_BRUSH = 2;

{ Pen support }

{ Logical Pen }
type
  PLOGPEN = ^LOGPEN;
  NPLOGPEN = ^LOGPEN; near;
  LPLOGPEN = ^LOGPEN; far;
  LOGPEN = record
    lopnStyle: UINT;
    lopnWidth: POINT;
    lopnColor: COLORREF;
  end;
  TLogPen = LOGPEN;

{ Pen Styles }
const
  PS_SOLID       = 0;
  PS_DASH        = 1;
  PS_DOT         = 2;
  PS_DASHDOT     = 3;
  PS_DASHDOTDOT  = 4;
  PS_NULL        = 5;
  PS_INSIDEFRAME = 6;

{ Stock pens for use with GetStockObject(); }
  WHITE_PEN = 6;
  BLACK_PEN = 7;
  NULL_PEN  = 8;

{ Brush support }

{ Brush Styles }
  BS_SOLID      = 0;
  BS_NULL       = 1;
  BS_HOLLOW     = BS_NULL;
  BS_HATCHED    = 2;
  BS_PATTERN    = 3;
  BS_INDEXED    = 4;
  BS_DIBPATTERN = 5;

{ Hatch Styles }
  HS_HORIZONTAL = 0;
  HS_VERTICAL   = 1;
  HS_FDIAGONAL  = 2;
  HS_BDIAGONAL  = 3;
  HS_CROSS      = 4;
  HS_DIAGCROSS  = 5;

{ Logical Brush (or Pattern) }
type
  PLOGBRUSH = ^LOGBRUSH;
  NPLOGBRUSH = ^LOGBRUSH; near;
  LPLOGBRUSH = ^LOGBRUSH; far;
  LOGBRUSH = record
    lbStyle: UINT;
    lbColor: COLORREF;
    lbHatch: SmallInt;
  end;
  TLogBrush = LOGBRUSH;

  PPATTERN = ^PATTERN;
  NPPATTERN = ^PATTERN; near;
  LPPATTERN = ^PATTERN; far;
  PATTERN = LOGBRUSH;
  TPattern = PATTERN;

{ Stock brushes for use with GetStockObject() }
const
  WHITE_BRUSH  = 0;
  LTGRAY_BRUSH = 1;
  GRAY_BRUSH   = 2;
  DKGRAY_BRUSH = 3;
  BLACK_BRUSH  = 4;
  NULL_BRUSH   = 5;
  HOLLOW_BRUSH = NULL_BRUSH;

{ Region support }

{ Region type flags }
{$ifndef SYSTEMUNIT}
  ERROR         = 0;
{$endif SYSTEMUNIT}
  NULLREGION    = 1;
  SIMPLEREGION  = 2;
  COMPLEXREGION = 3;

{ CombineRgn() command values }
  RGN_AND  = 1;
  RGN_OR   = 2;
  RGN_XOR  = 3;
  RGN_DIFF = 4;
  RGN_COPY = 5;

{ Color palette Support }
type
  PPALETTEENTRY = ^PALETTEENTRY;
  LPPALETTEENTRY = ^PALETTEENTRY; far;
  PALETTEENTRY = record
    peRed: BYTE;
    peGreen: BYTE;
    peBlue: BYTE;
    peFlags: BYTE;
  end;
  TPaletteEntry = PALETTEENTRY;

{ Palette entry flags }
const
  PC_RESERVED   = $01;  { palette index used for animation }
  PC_EXPLICIT   = $02;  { palette index is explicit to device }
  PC_NOCOLLAPSE = $04;  { do not match color to system palette }

{ Logical Palette }
type
  PLOGPALETTE = ^LOGPALETTE;
  NPLOGPALETTE = ^LOGPALETTE; near;
  LPLOGPALETTE = ^LOGPALETTE; far;
  LOGPALETTE = record
    palVersion: WORD;
    palNumEntries: WORD;
    palPalEntry: array [0..0] of PALETTEENTRY;
  end;
  TLogPalette = LOGPALETTE;

{ Get/SetSystemPaletteUse() values }
const
  SYSPAL_STATIC   = 1;
  SYSPAL_NOSTATIC = 2;

{ Palette window messages }
  WM_QUERYNEWPALETTE   = $030F;
  WM_PALETTEISCHANGING = $0310;
  WM_PALETTECHANGED    = $0311;

{ General drawing support }
type
  LINEDDAPROC = procedure(xPos, yPos: SmallInt; lpData: LPARAM); far;

const
{ PolyFill Modes }
  ALTERNATE = 1;
  WINDING   = 2;

{ ExtFloodFill style flags }
  FLOODFILLBORDER  = 0;
  FLOODFILLSURFACE = 1;

{ Text support }
  ETO_GRAYED  = $0001;
  ETO_OPAQUE  = $0002;
  ETO_CLIPPED = $0004;

{ DrawText() Format Flags }
  DT_TOP             = $0000;
  DT_LEFT            = $0000;
  DT_CENTER          = $0001;
  DT_RIGHT           = $0002;
  DT_VCENTER         = $0004;
  DT_BOTTOM          = $0008;
  DT_WORDBREAK       = $0010;
  DT_SINGLELINE      = $0020;
  DT_EXPANDTABS      = $0040;
  DT_TABSTOP         = $0080;
  DT_NOCLIP          = $0100;
  DT_EXTERNALLEADING = $0200;
  DT_CALCRECT        = $0400;
  DT_NOPREFIX        = $0800;
  DT_INTERNAL        = $1000;

{ Background Modes }
  TRANSPARENT = 1;
  OPAQUE      = 2;

{ Text Alignment Options }
  TA_NOUPDATECP = $0000;
  TA_UPDATECP   = $0001;
  TA_LEFT       = $0000;
  TA_RIGHT      = $0002;
  TA_CENTER     = $0006;
  TA_TOP        = $0000;
  TA_BOTTOM     = $0008;
  TA_BASELINE   = $0018;

type
  GRAYSTRINGPROC = function(hdc: HDC; lpData: LPARAM; cch: SmallInt): BOOL; far;

{ Font support }

const
{ Logical Font }
  LF_FACESIZE = 32;

type
  PLOGFONT = ^LOGFONT;
  NPLOGFONT = ^LOGFONT; near;
  LPLOGFONT = ^LOGFONT; far;
  LOGFONT = record
    lfHeight: SmallInt;
    lfWidth: SmallInt;
    lfEscapement: SmallInt;
    lfOrientation: SmallInt;
    lfWeight: SmallInt;
    lfItalic: BYTE;
    lfUnderline: BYTE;
    lfStrikeOut: BYTE;
    lfCharSet: BYTE;
    lfOutPrecision: BYTE;
    lfClipPrecision: BYTE;
    lfQuality: BYTE;
    lfPitchAndFamily: BYTE;
    lfFaceName: array [0..LF_FACESIZE-1] of char;
  end;
  TLogFont = LOGFONT;

const
{ weight values }
  FW_DONTCARE   = 0;
  FW_THIN       = 100;
  FW_EXTRALIGHT = 200;
  FW_LIGHT      = 300;
  FW_NORMAL     = 400;
  FW_MEDIUM     = 500;
  FW_SEMIBOLD   = 600;
  FW_BOLD       = 700;
  FW_EXTRABOLD  = 800;
  FW_HEAVY      = 900;

  FW_ULTRALIGHT = FW_EXTRALIGHT;
  FW_REGULAR    = FW_NORMAL;
  FW_DEMIBOLD   = FW_SEMIBOLD;
  FW_ULTRABOLD  = FW_EXTRABOLD;
  FW_BLACK      = FW_HEAVY;

{ CharSet values }
  ANSI_CHARSET        = 0;
  DEFAULT_CHARSET     = 1;
  SYMBOL_CHARSET      = 2;
  SHIFTJIS_CHARSET    = 128;
  HANGEUL_CHARSET     = 129;
  CHINESEBIG5_CHARSET = 136;
  OEM_CHARSET         = 255;

{ OutPrecision values }
  OUT_DEFAULT_PRECIS   = 0;
  OUT_STRING_PRECIS    = 1;
  OUT_CHARACTER_PRECIS = 2;
  OUT_STROKE_PRECIS    = 3;

{ ClipPrecision values }
  CLIP_DEFAULT_PRECIS   = $00;
  CLIP_CHARACTER_PRECIS = $01;
  CLIP_STROKE_PRECIS    = $02;
  CLIP_MASK             = $0F;

{ Quality values }
  DEFAULT_QUALITY = 0;
  DRAFT_QUALITY   = 1;
  PROOF_QUALITY   = 2;

{ PitchAndFamily pitch values (low 4 bits) }
  DEFAULT_PITCH  = $00;
  FIXED_PITCH    = $01;
  VARIABLE_PITCH = $02;

{ PitchAndFamily family values (high 4 bits) }
  FF_DONTCARE   = $00;
  FF_ROMAN      = $10;
  FF_SWISS      = $20;
  FF_MODERN     = $30;
  FF_SCRIPT     = $40;
  FF_DECORATIVE = $50;

{ Stock fonts for use with GetStockObject() }
  OEM_FIXED_FONT      = 10;
  ANSI_FIXED_FONT     = 11;
  ANSI_VAR_FONT       = 12;
  SYSTEM_FONT         = 13;
  DEVICE_DEFAULT_FONT = 14;
  DEFAULT_PALETTE     = 15;
  SYSTEM_FIXED_FONT   = 16;

  ASPECT_FILTERING    = $00000001;

  WM_FONTCHANGE       = $001D;

type
  PTEXTMETRIC = ^TEXTMETRIC;
  NPTEXTMETRIC = ^TEXTMETRIC; near;
  LPTEXTMETRIC = ^TEXTMETRIC; far;
  TEXTMETRIC = record
    tmHeight: SmallInt;
    tmAscent: SmallInt;
    tmDescent: SmallInt;
    tmInternalLeading: SmallInt;
    tmExternalLeading: SmallInt;
    tmAveCharWidth: SmallInt;
    tmMaxCharWidth: SmallInt;
    tmWeight: SmallInt;
    tmItalic: BYTE;
    tmUnderlined: BYTE;
    tmStruckOut: BYTE;
    tmFirstChar: BYTE;
    tmLastChar: BYTE;
    tmDefaultChar: BYTE;
    tmBreakChar: BYTE;
    tmPitchAndFamily: BYTE;
    tmCharSet: BYTE;
    tmOverhang: SmallInt;
    tmDigitizedAspectX: SmallInt;
    tmDigitizedAspectY: SmallInt;
  end;
  TTextMetric = TEXTMETRIC;

const
{ tmPitchAndFamily values }
  TMPF_FIXED_PITCH = $01;
  TMPF_VECTOR      = $02;
  TMPF_DEVICE      = $08;

type
  OLDFONTENUMPROC = function(lplf: LPLOGFONT; lptm: LPTEXTMETRIC; FontType: SmallInt; lpData: LPARAM): SmallInt; far;

const
{ EnumFonts font type values }
  RASTER_FONTTYPE = $0001;
  DEVICE_FONTTYPE = $0002;

type
{ Bitmap support }
  PBITMAP = ^BITMAP;
  NPBITMAP = ^BITMAP; near;
  LPBITMAP = ^BITMAP; far;
  BITMAP = record
    bmType: SmallInt;
    bmWidth: SmallInt;
    bmHeight: SmallInt;
    bmWidthBytes: SmallInt;
    bmPlanes: BYTE;
    bmBitsPixel: BYTE;
    bmBits: FarPointer;
  end;
  TBitmap = BITMAP;

{ Bitmap Header structures }
  PRGBTRIPLE = ^RGBTRIPLE;
  LPRGBTRIPLE = ^RGBTRIPLE; far;
  RGBTRIPLE = record
    rgbtBlue: BYTE;
    rgbtGreen: BYTE;
    rgbtRed: BYTE;
  end;
  TRGBTriple = RGBTRIPLE;

  PRGBQUAD = ^RGBQUAD;
  LPRGBQUAD = ^RGBQUAD; far;
  RGBQUAD = record
    rgbBlue: BYTE;
    rgbGreen: BYTE;
    rgbRed: BYTE;
    rgbReserved: BYTE;
  end;
  TRGBQuad = RGBQUAD;

{ structures for defining DIBs }
  PBITMAPCOREHEADER = ^BITMAPCOREHEADER;
  LPBITMAPCOREHEADER = ^BITMAPCOREHEADER; far;
  BITMAPCOREHEADER = record
    bcSize: DWORD;
    bcWidth: SmallInt;
    bcHeight: SmallInt;
    bcPlanes: WORD;
    bcBitCount: WORD;
  end;
  TBitmapCoreHeader = BITMAPCOREHEADER;

  PBITMAPINFOHEADER = ^BITMAPINFOHEADER;
  LPBITMAPINFOHEADER = ^BITMAPINFOHEADER; far;
  BITMAPINFOHEADER = record
    biSize: DWORD;
    biWidth: LONG;
    biHeight: LONG;
    biPlanes: WORD;
    biBitCount: WORD;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: LONG;
    biYPelsPerMeter: LONG;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;
  TBitmapInfoHeader = BITMAPINFOHEADER;

const
{ constants for the biCompression field }
  BI_RGB  = 0;
  BI_RLE8 = 1;
  BI_RLE4 = 2;

type
  PBITMAPINFO = ^BITMAPINFO;
  LPBITMAPINFO = ^BITMAPINFO; far;
  BITMAPINFO = record
    bmiHeader: BITMAPINFOHEADER;
    bmiColors: array [0..0] of RGBQUAD;
  end;
  TBitmapInfo = BITMAPINFO;

  PBITMAPCOREINFO = ^BITMAPCOREINFO;
  LPBITMAPCOREINFO = ^BITMAPCOREINFO; far;
  BITMAPCOREINFO = record
    bmciHeader: BITMAPCOREHEADER;
    bmciColors: array [0..0] of RGBTRIPLE;
  end;
  TBitmapCoreInfo = BITMAPCOREINFO;

  PBITMAPFILEHEADER = ^BITMAPFILEHEADER;
  LPBITMAPFILEHEADER = ^BITMAPFILEHEADER; far;
  BITMAPFILEHEADER = record
    bfType: UINT;
    bfSize: DWORD;
    bfReserved1: UINT;
    bfReserved2: UINT;
    bfOffBits: DWORD;
  end;
  TBitmapFileHeader = BITMAPFILEHEADER;

const
{ DIB color table identifiers }
  DIB_RGB_COLORS = 0;
  DIB_PAL_COLORS = 1;

{ constants for CreateDIBitmap }
  CBM_INIT = $00000004;

{ Binary raster ops }
  R2_BLACK       = 1;
  R2_NOTMERGEPEN = 2;
  R2_MASKNOTPEN  = 3;
  R2_NOTCOPYPEN  = 4;
  R2_MASKPENNOT  = 5;
  R2_NOT         = 6;
  R2_XORPEN      = 7;
  R2_NOTMASKPEN  = 8;
  R2_MASKPEN     = 9;
  R2_NOTXORPEN   = 10;
  R2_NOP         = 11;
  R2_MERGENOTPEN = 12;
  R2_COPYPEN     = 13;
  R2_MERGEPENNOT = 14;
  R2_MERGEPEN    = 15;
  R2_WHITE       = 16;

{ Ternary raster operations }
  SRCCOPY     = $00CC0020;
  SRCPAINT    = $00EE0086;
  SRCAND      = $008800C6;
  SRCINVERT   = $00660046;
  SRCERASE    = $00440328;
  NOTSRCCOPY  = $00330008;
  NOTSRCERASE = $001100A6;
  MERGECOPY   = $00C000CA;
  MERGEPAINT  = $00BB0226;
  PATCOPY     = $00F00021;
  PATPAINT    = $00FB0A09;
  PATINVERT   = $005A0049;
  DSTINVERT   = $00550009;
  BLACKNESS   = $00000042;
  WHITENESS   = $00FF0062;

{ StretchBlt() Modes }
  BLACKONWHITE = 1;
  WHITEONBLACK = 2;
  COLORONCOLOR = 3;

{ new StretchBlt() Modes (simpler names) }
  STRETCH_ANDSCANS    = 1;
  STRETCH_ORSCANS     = 2;
  STRETCH_DELETESCANS = 3;

type
{ Metafile support }
  HMETAFILE = THandle;

{ Clipboard Metafile Picture Structure }
  PMETAFILEPICT = ^METAFILEPICT;
  LPMETAFILEPICT = ^METAFILEPICT; far;
  METAFILEPICT = record
    mm: SmallInt;
    xExt: SmallInt;
    yExt: SmallInt;
    hMF: HMETAFILE;
  end;
  TMetaFilePict = METAFILEPICT;

  PMETAHEADER = ^METAHEADER;
  LPMETAHEADER = ^METAHEADER; far;
  METAHEADER = record
    mtType: UINT;
    mtHeaderSize: UINT;
    mtVersion: UINT;
    mtSize: DWORD;
    mtNoObjects: UINT;
    mtMaxRecord: DWORD;
    mtNoParameters: UINT;
  end;
  TMetaHeader = METAHEADER;

  PHANDLETABLE = ^HANDLETABLE;
  LPHANDLETABLE = ^HANDLETABLE; far;
  HANDLETABLE = record
    objectHandle: array [0..0] of HGDIOBJ;
  end;
  THandleTable = HANDLETABLE;

  PMETARECORD = ^METARECORD;
  LPMETARECORD = ^METARECORD; far;
  METARECORD = record
    rdSize: DWORD;
    rdFunction: UINT;
    rdParm: array [0..0] of UINT;
  end;
  TMetaRecord = METARECORD;

const
{ Metafile Functions }
  META_SETBKCOLOR            = $0201;
  META_SETBKMODE             = $0102;
  META_SETMAPMODE            = $0103;
  META_SETROP2               = $0104;
  META_SETRELABS             = $0105;
  META_SETPOLYFILLMODE       = $0106;
  META_SETSTRETCHBLTMODE     = $0107;
  META_SETTEXTCHAREXTRA      = $0108;
  META_SETTEXTCOLOR          = $0209;
  META_SETTEXTJUSTIFICATION  = $020A;
  META_SETWINDOWORG          = $020B;
  META_SETWINDOWEXT          = $020C;
  META_SETVIEWPORTORG        = $020D;
  META_SETVIEWPORTEXT        = $020E;
  META_OFFSETWINDOWORG       = $020F;
  META_SCALEWINDOWEXT        = $0410;
  META_OFFSETVIEWPORTORG     = $0211;
  META_SCALEVIEWPORTEXT      = $0412;
  META_LINETO                = $0213;
  META_MOVETO                = $0214;
  META_EXCLUDECLIPRECT       = $0415;
  META_INTERSECTCLIPRECT     = $0416;
  META_ARC                   = $0817;
  META_ELLIPSE               = $0418;
  META_FLOODFILL             = $0419;
  META_PIE                   = $081A;
  META_RECTANGLE             = $041B;
  META_ROUNDRECT             = $061C;
  META_PATBLT                = $061D;
  META_SAVEDC                = $001E;
  META_SETPIXEL              = $041F;
  META_OFFSETCLIPRGN         = $0220;
  META_TEXTOUT               = $0521;
  META_BITBLT                = $0922;
  META_STRETCHBLT            = $0B23;
  META_POLYGON               = $0324;
  META_POLYLINE              = $0325;
  META_ESCAPE                = $0626;
  META_RESTOREDC             = $0127;
  META_FILLREGION            = $0228;
  META_FRAMEREGION           = $0429;
  META_INVERTREGION          = $012A;
  META_PAINTREGION           = $012B;
  META_SELECTCLIPREGION      = $012C;
  META_SELECTOBJECT          = $012D;
  META_SETTEXTALIGN          = $012E;
  META_DRAWTEXT              = $062F;

  META_CHORD                 = $0830;
  META_SETMAPPERFLAGS        = $0231;
  META_EXTTEXTOUT            = $0a32;
  META_SETDIBTODEV           = $0d33;
  META_SELECTPALETTE         = $0234;
  META_REALIZEPALETTE        = $0035;
  META_ANIMATEPALETTE        = $0436;
  META_SETPALENTRIES         = $0037;
  META_POLYPOLYGON           = $0538;
  META_RESIZEPALETTE         = $0139;

  META_DIBBITBLT             = $0940;
  META_DIBSTRETCHBLT         = $0b41;
  META_DIBCREATEPATTERNBRUSH = $0142;
  META_STRETCHDIB            = $0f43;

  META_EXTFLOODFILL          = $0548;

  META_RESETDC               = $014C;
  META_STARTDOC              = $014D;
  META_STARTPAGE             = $004F;
  META_ENDPAGE               = $0050;
  META_ABORTDOC              = $0052;
  META_ENDDOC                = $005E;

  META_DELETEOBJECT          = $01f0;

  META_CREATEPALETTE         = $00f7;
  META_CREATEBRUSH           = $00F8;
  META_CREATEPATTERNBRUSH    = $01F9;
  META_CREATEPENINDIRECT     = $02FA;
  META_CREATEFONTINDIRECT    = $02FB;
  META_CREATEBRUSHINDIRECT   = $02FC;
  META_CREATEBITMAPINDIRECT  = $02FD;
  META_CREATEBITMAP          = $06FE;
  META_CREATEREGION          = $06FF;

type
  MFENUMPROC = function(hdc: HDC; lpht: LPHANDLETABLE; lpmr: LPMETARECORD; cObj: SmallInt; lParam: LPARAM): SmallInt; far;

{ Printing support }
  ABORTPROC = function(hdc: HDC; error: SmallInt): BOOL; far;

const
{ Spooler Error Codes }
  SP_NOTREPORTED   = $4000;
  SP_ERROR         = (-1);
  SP_APPABORT      = (-2);
  SP_USERABORT     = (-3);
  SP_OUTOFDISK     = (-4);
  SP_OUTOFMEMORY   = (-5);

  PR_JOBSTATUS     = $0000;

{ Spooler status notification message }
  WM_SPOOLERSTATUS = $002A;

{ GDI Escape support }
{ GDI Escapes }
  NEWFRAME               = 1;
  ABORTDOC               = 2;
  NEXTBAND               = 3;
  SETCOLORTABLE          = 4;
  GETCOLORTABLE          = 5;
  FLUSHOUTPUT            = 6;
  DRAFTMODE              = 7;
  QUERYESCSUPPORT        = 8;
  SETABORTPROC           = 9;
  STARTDOC               = 10;
  ENDDOC                 = 11;
  GETPHYSPAGESIZE        = 12;
  GETPRINTINGOFFSET      = 13;
  GETSCALINGFACTOR       = 14;
  MFCOMMENT              = 15;
  GETPENWIDTH            = 16;
  SETCOPYCOUNT           = 17;
  SELECTPAPERSOURCE      = 18;
  DEVICEDATA             = 19;
  PASSTHROUGH            = 19;
  GETTECHNOLGY           = 20;
  GETTECHNOLOGY          = 20;
  SETLINECAP             = 21;
  SETLINEJOIN            = 22;
  SETMITERLIMIT          = 23;
  BANDINFO               = 24;
  DRAWPATTERNRECT        = 25;
  GETVECTORPENSIZE       = 26;
  GETVECTORBRUSHSIZE     = 27;
  ENABLEDUPLEX           = 28;
  GETSETPAPERBINS        = 29;
  GETSETPRINTORIENT      = 30;
  ENUMPAPERBINS          = 31;
  SETDIBSCALING          = 32;
  EPSPRINTING            = 33;
  ENUMPAPERMETRICS       = 34;
  GETSETPAPERMETRICS     = 35;
  POSTSCRIPT_DATA        = 37;
  POSTSCRIPT_IGNORE      = 38;
  MOUSETRAILS            = 39;

  GETEXTENDEDTEXTMETRICS = 256;
  GETEXTENTTABLE         = 257;
  GETPAIRKERNTABLE       = 258;
  GETTRACKKERNTABLE      = 259;
  GDIEXTTEXTOUT          = 512; { renamed to avoid conflict with the ExtTextOut function }
  GETFACENAME            = 513;
  ENABLERELATIVEWIDTHS   = 768;
  ENABLEPAIRKERNING      = 769;
  SETKERNTRACK           = 770;
  SETALLJUSTVALUES       = 771;
  SETCHARSET             = 772;

  GDISTRETCHBLT          = 2048; { renamed to avoid conflict with the StretchBlt function }

  GETSETSCREENPARAMS     = 3072;

  BEGIN_PATH             = 4096;
  CLIP_TO_PATH           = 4097;
  END_PATH               = 4098;
  EXT_DEVICE_CAPS        = 4099;
  RESTORE_CTM            = 4100;
  SAVE_CTM               = 4101;
  SET_ARC_DIRECTION      = 4102;
  SET_BACKGROUND_COLOR   = 4103;
  SET_POLY_MODE          = 4104;
  SET_SCREEN_ANGLE       = 4105;
  SET_SPREAD             = 4106;
  TRANSFORM_CTM          = 4107;
  SET_CLIP_BOX           = 4108;
  SET_BOUNDS             = 4109;

type
{ USER typedefs, structures, and functions }
  HWND = THandle;
  HMENU = THandle;
  HICON = THandle;
  HCURSOR = HICON;  { HICONs & HCURSORs are polymorphic }

{ System Metrics }
const
{ GetSystemMetrics() codes }
  SM_CXSCREEN      = 0;
  SM_CYSCREEN      = 1;
  SM_CXVSCROLL     = 2;
  SM_CYHSCROLL     = 3;
  SM_CYCAPTION     = 4;
  SM_CXBORDER      = 5;
  SM_CYBORDER      = 6;
  SM_CXDLGFRAME    = 7;
  SM_CYDLGFRAME    = 8;
  SM_CYVTHUMB      = 9;
  SM_CXHTHUMB      = 10;
  SM_CXICON        = 11;
  SM_CYICON        = 12;
  SM_CXCURSOR      = 13;
  SM_CYCURSOR      = 14;
  SM_CYMENU        = 15;
  SM_CXFULLSCREEN  = 16;
  SM_CYFULLSCREEN  = 17;
  SM_CYKANJIWINDOW = 18;
  SM_MOUSEPRESENT  = 19;
  SM_CYVSCROLL     = 20;
  SM_CXHSCROLL     = 21;
  SM_DEBUG         = 22;
  SM_SWAPBUTTON    = 23;
  SM_RESERVED1     = 24;
  SM_RESERVED2     = 25;
  SM_RESERVED3     = 26;
  SM_RESERVED4     = 27;
  SM_CXMIN         = 28;
  SM_CYMIN         = 29;
  SM_CXSIZE        = 30;
  SM_CYSIZE        = 31;
  SM_CXFRAME       = 32;
  SM_CYFRAME       = 33;
  SM_CXMINTRACK    = 34;
  SM_CYMINTRACK    = 35;
  SM_CMETRICS      = 36;

  WM_DEVMODECHANGE = $001B;
  WM_TIMECHANGE    = $001E;

{ Window message support }
  WM_NULL = $0000;

{ NOTE: All messages below $0400 are RESERVED by Windows }
  WM_USER = $0400;

type
{ Queued message structure }
  PMSG = ^MSG;
  NPMSG = ^MSG; near;
  LPMSG = ^MSG; far;
  MSG = record
    hwnd: HWND;
    message: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
    time: DWORD;
    pt: POINT;
  end;
  TMsg = MSG;

const
{ PeekMessage() options }
  PM_NOREMOVE = $0000;
  PM_REMOVE   = $0001;
  PM_NOYIELD  = $0002;

{ Special HWND value for use with PostMessage() and SendMessage() }
  HWND_BROADCAST = HWND($ffff);

  WH_GETMESSAGE   = 3;

  WH_CALLWNDPROC  = 4;

  WH_MSGFILTER    = (-1);
  WH_SYSMSGFILTER = 6;

{ CallMsgFilter() and WH_SYS/MSGFILTER context codes }
  MSGF_DIALOGBOX  = 0;
  MSGF_MENU       = 2;
  MSGF_MOVE       = 3;
  MSGF_SIZE       = 4;
  MSGF_SCROLLBAR  = 5;
  MSGF_NEXTWINDOW = 6;
  MSGF_MAINLOOP   = 8;
  MSGF_USER       = 4096;

{ Standard window messages }
{ PenWindows specific messages }
  WM_PENWINFIRST = $0380;
  WM_PENWINLAST  = $038F;

{ Coalescing messages }
  WM_COALESCE_FIRST = $0390;
  WM_COALESCE_LAST  = $039F;
