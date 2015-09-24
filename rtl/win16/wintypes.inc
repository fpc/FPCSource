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
  
  PBOOL = ^BOOL;
  LPBOOL = ^BOOL; far;
  
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

{ Application termination }
  WM_QUERYENDSESSION = $0011;
  WM_ENDSESSION      = $0016;

  WM_QUIT            = $0012;

  WM_SYSTEMERROR     = $0017;

type
{ Window class management }
  WNDPROC = function(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; far;

  PWNDCLASS = ^WNDCLASS;
  NPWNDCLASS = ^WNDCLASS; near;
  LPWNDCLASS = ^WNDCLASS; far;
  WNDCLASS = record
    style: UINT;
    lpfnWndProc: WNDPROC;
    cbClsExtra: SmallInt;
    cbWndExtra: SmallInt;
    hInstance: HINST;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: LPCSTR;
    lpszClassName: LPCSTR;
  end;
  TWndClass = WNDCLASS;

const
{ Class styles }
  CS_VREDRAW         = $0001;
  CS_HREDRAW         = $0002;

  CS_OWNDC           = $0020;
  CS_CLASSDC         = $0040;
  CS_PARENTDC        = $0080;

  CS_SAVEBITS        = $0800;

  CS_DBLCLKS         = $0008;

  CS_BYTEALIGNCLIENT = $1000;
  CS_BYTEALIGNWINDOW = $2000;

  CS_NOCLOSE         = $0200;

  CS_KEYCVTWINDOW    = $0004;
  CS_NOKEYCVT        = $0100;

  CS_GLOBALCLASS     = $4000;

{ Class field offsets for GetClassLong() and GetClassWord() }
  GCL_MENUNAME      = (-8);
  GCW_HBRBACKGROUND = (-10);
  GCW_HCURSOR       = (-12);
  GCW_HICON         = (-14);
  GCW_HMODULE       = (-16);
  GCW_CBWNDEXTRA    = (-18);
  GCW_CBCLSEXTRA    = (-20);
  GCL_WNDPROC       = (-24);
  GCW_STYLE         = (-26);

{ Window creation/destroy }

{ Window Styles }

{ Basic window types }
  WS_OVERLAPPED        = $00000000;
  WS_POPUP             = $80000000;
  WS_CHILD             = $40000000;

{ Clipping styles }
  WS_CLIPSIBLINGS      = $04000000;
  WS_CLIPCHILDREN      = $02000000;

{ Generic window states }
  WS_VISIBLE           = $10000000;
  WS_DISABLED          = $08000000;

{ Main window states }
  WS_MINIMIZE          = $20000000;
  WS_MAXIMIZE          = $01000000;

{ Main window styles }
  WS_CAPTION           = $00C00000;     { WS_BORDER | WS_DLGFRAME }
  WS_BORDER            = $00800000;
  WS_DLGFRAME          = $00400000;
  WS_VSCROLL           = $00200000;
  WS_HSCROLL           = $00100000;
  WS_SYSMENU           = $00080000;
  WS_THICKFRAME        = $00040000;
  WS_MINIMIZEBOX       = $00020000;
  WS_MAXIMIZEBOX       = $00010000;

{ Control window styles }
  WS_GROUP             = $00020000;
  WS_TABSTOP           = $00010000;

{ Common Window Styles }
  WS_OVERLAPPEDWINDOW  = (WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_THICKFRAME or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
  WS_POPUPWINDOW       = (WS_POPUP or WS_BORDER or WS_SYSMENU);
  WS_CHILDWINDOW       = (WS_CHILD);

{ Extended Window Styles }
  WS_EX_DLGMODALFRAME  = $00000001;
  WS_EX_NOPARENTNOTIFY = $00000004;

{ Obsolete style names }
  WS_TILED             = WS_OVERLAPPED;
  WS_ICONIC            = WS_MINIMIZE;
  WS_SIZEBOX           = WS_THICKFRAME;
  WS_TILEDWINDOW       = WS_OVERLAPPEDWINDOW;



{ Special value for CreateWindow, et al. }
  HWND_DESKTOP          = HWND(0);

  WM_CREATE   = $0001;
  WM_NCCREATE = $0081;

type
{ WM_CREATE/WM_NCCREATE lParam struct }
  PCREATESTRUCT = ^CREATESTRUCT;
  LPCREATESTRUCT = ^CREATESTRUCT; far;
  CREATESTRUCT = record
    lpCreateParams: FarPointer;
    hInstance: HINST;
    hMenu: HMENU;
    hwndParent: HWND;
    cy: SmallInt;
    cx: SmallInt;
    y: SmallInt;
    x: SmallInt;
    style: LONG;
    lpszName: LPCSTR;
    lpszClass: LPCSTR;
    dwExStyle: DWORD;
  end;
  TCreateStruct = CREATESTRUCT;

const
  WM_DESTROY          = $0002;
  WM_NCDESTROY        = $0082;

  SW_HIDE             = 0;
  SW_SHOWNORMAL       = 1;
  SW_NORMAL           = 1;
  SW_SHOWMINIMIZED    = 2;
  SW_SHOWMAXIMIZED    = 3;
  SW_MAXIMIZE         = 3;
  SW_SHOWNOACTIVATE   = 4;
  SW_SHOW             = 5;
  SW_MINIMIZE         = 6;
  SW_SHOWMINNOACTIVE  = 7;
  SW_SHOWNA           = 8;
  SW_RESTORE          = 9;

{ Obsolete ShowWindow() command names }
  HIDE_WINDOW         = 0;
  SHOW_OPENWINDOW     = 1;
  SHOW_ICONWINDOW     = 2;
  SHOW_FULLSCREEN     = 3;
  SHOW_OPENNOACTIVATE = 4;

  WM_SHOWWINDOW       = $0018;

{ WM_SHOWWINDOW wParam codes }
  SW_PARENTCLOSING    = 1;
  SW_OTHERMAXIMIZED   = 2;
  SW_PARENTOPENING    = 3;
  SW_OTHERRESTORED    = 4;

{ Obsolete constant names }
  SW_OTHERZOOM        = SW_OTHERMAXIMIZED;
  SW_OTHERUNZOOM      = SW_OTHERRESTORED;

  WM_SETREDRAW        = $000B;

{ Enabled state }
  WM_ENABLE           = $000A;

{ Window text }
  WM_SETTEXT          = $000C;
  WM_GETTEXT          = $000D;
  WM_GETTEXTLENGTH    = $000E;

{ Window field offsets for GetWindowLong() and GetWindowWord() }
  GWL_WNDPROC         = (-4);
  GWW_HINSTANCE       = (-6);
  GWW_HWNDPARENT      = (-8);
  GWW_ID              = (-12);
  GWL_STYLE           = (-16);
  GWL_EXSTYLE         = (-20);

{ Window size, position, Z-order, and visibility }

  CW_USEDEFAULT      = SmallInt($8000);

{ SetWindowPos() and WINDOWPOS flags }
  SWP_NOSIZE         = $0001;
  SWP_NOMOVE         = $0002;
  SWP_NOZORDER       = $0004;
  SWP_NOREDRAW       = $0008;
  SWP_NOACTIVATE     = $0010;
  SWP_FRAMECHANGED   = $0020;  { The frame changed: send WM_NCCALCSIZE }
  SWP_SHOWWINDOW     = $0040;
  SWP_HIDEWINDOW     = $0080;
  SWP_NOCOPYBITS     = $0100;
  SWP_NOOWNERZORDER  = $0200;  { Don't do owner Z ordering }

  SWP_DRAWFRAME      = SWP_FRAMECHANGED;
  SWP_NOREPOSITION   = SWP_NOOWNERZORDER;

  SWP_NOSENDCHANGING = $0400;
  SWP_DEFERERASE     = $2000;

{ SetWindowPos() hwndInsertAfter field values }
  HWND_TOP           = HWND(0);
  HWND_BOTTOM        = HWND(1);
  HWND_TOPMOST       = HWND(-1);
  HWND_NOTOPMOST     = HWND(-2);

type
  HDWP = THandle;

const
  WM_MOVE            = $0003;
  WM_SIZE            = $0005;

{ WM_SIZE message wParam values }
  SIZE_RESTORED      = 0;
  SIZE_MINIMIZED     = 1;
  SIZE_MAXIMIZED     = 2;
  SIZE_MAXSHOW       = 3;
  SIZE_MAXHIDE       = 4;

{ Obsolete constant names }
  SIZENORMAL         = SIZE_RESTORED;
  SIZEICONIC         = SIZE_MINIMIZED;
  SIZEFULLSCREEN     = SIZE_MAXIMIZED;
  SIZEZOOMSHOW       = SIZE_MAXSHOW;
  SIZEZOOMHIDE       = SIZE_MAXHIDE;

{ Main window support }

  WM_QUERYOPEN = $0013;
  WM_CLOSE     = $0010;

type
{ Struct pointed to by WM_GETMINMAXINFO lParam }
  PMINMAXINFO = ^MINMAXINFO;
  LPMINMAXINFO = ^MINMAXINFO; far;
  MINMAXINFO = record
    ptReserved: POINT;
    ptMaxSize: POINT;
    ptMaxPosition: POINT;
    ptMinTrackSize: POINT;
    ptMaxTrackSize: POINT;
  end;
  TMinMaxInfo = MINMAXINFO;

const
  WM_GETMINMAXINFO = $0024;

type
{ Window query and enumeration }
  WNDENUMPROC = function(hwnd: HWND; lParam: LPARAM): BOOL; far;

const
{ GetWindow() constants }
  GW_HWNDFIRST = 0;
  GW_HWNDLAST  = 1;
  GW_HWNDNEXT  = 2;
  GW_HWNDPREV  = 3;
  GW_OWNER     = 4;
  GW_CHILD     = 5;

type
{ Window property support }
  PROPENUMPROC = function(hwnd: HWND; lpsz: LPCSTR; hData: HANDLE): BOOL; far;

const
{ Window repainting }
  WM_PAINT          = $000F;
  WM_ERASEBKGND     = $0014;
  WM_ICONERASEBKGND = $0027;

type
{ BeginPaint() return structure }
  PPAINTSTRUCT = ^PAINTSTRUCT;
  NPPAINTSTRUCT = ^PAINTSTRUCT; near;
  LPPAINTSTRUCT = ^PAINTSTRUCT; far;
  PAINTSTRUCT = record
    hdc: HDC;
    fErase: BOOL;
    rcPaint: RECT;
    fRestore: BOOL;
    fIncUpdate: BOOL;
    rgbReserved: array [0..15] of BYTE;
  end;
  TPaintStruct = PAINTSTRUCT;

const
  RDW_INVALIDATE      = $0001;
  RDW_INTERNALPAINT   = $0002;
  RDW_ERASE           = $0004;

  RDW_VALIDATE        = $0008;
  RDW_NOINTERNALPAINT = $0010;
  RDW_NOERASE         = $0020;

  RDW_NOCHILDREN      = $0040;
  RDW_ALLCHILDREN     = $0080;

  RDW_UPDATENOW       = $0100;
  RDW_ERASENOW        = $0200;

  RDW_FRAME           = $0400;
  RDW_NOFRAME         = $0800;

{ Non-client window area management }

  WM_NCPAINT    = $0085;

  WM_NCCALCSIZE = $0083;

type
{ WM_NCCALCSIZE parameter structure }
  PNCCALCSIZE_PARAMS = ^NCCALCSIZE_PARAMS;
  LPNCCALCSIZE_PARAMS = ^NCCALCSIZE_PARAMS; far;
  NCCALCSIZE_PARAMS = record
    rgrc: array [0..1] of RECT;
  end;
  TNCCalcSize_Params = NCCALCSIZE_PARAMS;

const
  WM_NCHITTEST  = $0084;

{ WM_NCHITTEST return codes }
  HTERROR       = (-2);
  HTTRANSPARENT = (-1);
  HTNOWHERE     = 0;
  HTCLIENT      = 1;
  HTCAPTION     = 2;
  HTSYSMENU     = 3;
  HTSIZE        = 4;
  HTMENU        = 5;
  HTHSCROLL     = 6;
  HTVSCROLL     = 7;
  HTMINBUTTON   = 8;
  HTMAXBUTTON   = 9;
  HTLEFT        = 10;
  HTRIGHT       = 11;
  HTTOP         = 12;
  HTTOPLEFT     = 13;
  HTTOPRIGHT    = 14;
  HTBOTTOM      = 15;
  HTBOTTOMLEFT  = 16;
  HTBOTTOMRIGHT = 17;
  HTBORDER      = 18;
  HTGROWBOX     = HTSIZE;
  HTREDUCE      = HTMINBUTTON;
  HTZOOM        = HTMAXBUTTON;

{ Drag-and-drop support }

  WM_QUERYDRAGICON = $0037;
  WM_DROPFILES     = $0233;

{ Window activation }
{ WM_ACTIVATE state values }
  WA_INACTIVE    = 0;
  WA_ACTIVE      = 1;
  WA_CLICKACTIVE = 2;

  WM_ACTIVATE    = $0006;
  WM_ACTIVATEAPP = $001C;
  WM_NCACTIVATE  = $0086;

type
{ Keyboard input support }
  PKeyboardState = ^TKeyboardState;
  LPKeyboardState = ^TKeyboardState; far;
  TKeyboardState = array [0..255] of Char;

const
  WM_SETFOCUS    = $0007;
  WM_KILLFOCUS   = $0008;

  WM_KEYDOWN     = $0100;
  WM_KEYUP       = $0101;

  WM_CHAR        = $0102;
  WM_DEADCHAR    = $0103;

  WM_SYSKEYDOWN  = $0104;
  WM_SYSKEYUP    = $0105;

  WM_SYSCHAR     = $0106;
  WM_SYSDEADCHAR = $0107;


{ Keyboard message range }
  WM_KEYFIRST    = $0100;
  WM_KEYLAST     = $0108;

{ WM_KEYUP/DOWN/CHAR HIWORD(lParam) flags }
  KF_EXTENDED    = $0100;
  KF_DLGMODE     = $0800;
  KF_MENUMODE    = $1000;
  KF_ALTDOWN     = $2000;
  KF_REPEAT      = $4000;
  KF_UP          = $8000;

{ Virtual key codes }
  VK_LBUTTON     = $01;
  VK_RBUTTON     = $02;
  VK_CANCEL      = $03;
  VK_MBUTTON     = $04;
  VK_BACK        = $08;
  VK_TAB         = $09;
  VK_CLEAR       = $0C;
  VK_RETURN      = $0D;
  VK_SHIFT       = $10;
  VK_CONTROL     = $11;
  VK_MENU        = $12;
  VK_PAUSE       = $13;
  VK_CAPITAL     = $14;
  VK_ESCAPE      = $1B;
  VK_SPACE       = $20;
  VK_PRIOR       = $21;
  VK_NEXT        = $22;
  VK_END         = $23;
  VK_HOME        = $24;
  VK_LEFT        = $25;
  VK_UP          = $26;
  VK_RIGHT       = $27;
  VK_DOWN        = $28;
  VK_SELECT      = $29;
  VK_PRINT       = $2A;
  VK_EXECUTE     = $2B;
  VK_SNAPSHOT    = $2C;
  VK_INSERT      = $2D;
  VK_DELETE      = $2E;
  VK_HELP        = $2F;
  VK_NUMPAD0     = $60;
  VK_NUMPAD1     = $61;
  VK_NUMPAD2     = $62;
  VK_NUMPAD3     = $63;
  VK_NUMPAD4     = $64;
  VK_NUMPAD5     = $65;
  VK_NUMPAD6     = $66;
  VK_NUMPAD7     = $67;
  VK_NUMPAD8     = $68;
  VK_NUMPAD9     = $69;
  VK_MULTIPLY    = $6A;
  VK_ADD         = $6B;
  VK_SEPARATOR   = $6C;
  VK_SUBTRACT    = $6D;
  VK_DECIMAL     = $6E;
  VK_DIVIDE      = $6F;
  VK_F1          = $70;
  VK_F2          = $71;
  VK_F3          = $72;
  VK_F4          = $73;
  VK_F5          = $74;
  VK_F6          = $75;
  VK_F7          = $76;
  VK_F8          = $77;
  VK_F9          = $78;
  VK_F10         = $79;
  VK_F11         = $7A;
  VK_F12         = $7B;
  VK_F13         = $7C;
  VK_F14         = $7D;
  VK_F15         = $7E;
  VK_F16         = $7F;
  VK_F17         = $80;
  VK_F18         = $81;
  VK_F19         = $82;
  VK_F20         = $83;
  VK_F21         = $84;
  VK_F22         = $85;
  VK_F23         = $86;
  VK_F24         = $87;
  VK_NUMLOCK     = $90;
  VK_SCROLL      = $91;

{ VK_A thru VK_Z are the same as their ASCII equivalents: 'A' thru 'Z' }
{ VK_0 thru VK_9 are the same as their ASCII equivalents: '0' thru '0' }

{ SetWindowsHook() keyboard hook }
  WH_KEYBOARD    = 2;

{ Mouse input support }
{ Mouse input messages }
  WM_MOUSEMOVE       = $0200;
  WM_LBUTTONDOWN     = $0201;
  WM_LBUTTONUP       = $0202;
  WM_LBUTTONDBLCLK   = $0203;
  WM_RBUTTONDOWN     = $0204;
  WM_RBUTTONUP       = $0205;
  WM_RBUTTONDBLCLK   = $0206;
  WM_MBUTTONDOWN     = $0207;
  WM_MBUTTONUP       = $0208;
  WM_MBUTTONDBLCLK   = $0209;

{ Mouse input message range }
  WM_MOUSEFIRST      = $0200;
  WM_MOUSELAST       = $0209;

{ Mouse message wParam key states }
  MK_LBUTTON         = $0001;
  MK_RBUTTON         = $0002;
  MK_SHIFT           = $0004;
  MK_CONTROL         = $0008;
  MK_MBUTTON         = $0010;

{ Non-client mouse messages }
  WM_NCMOUSEMOVE     = $00A0;
  WM_NCLBUTTONDOWN   = $00A1;
  WM_NCLBUTTONUP     = $00A2;
  WM_NCLBUTTONDBLCLK = $00A3;
  WM_NCRBUTTONDOWN   = $00A4;
  WM_NCRBUTTONUP     = $00A5;
  WM_NCRBUTTONDBLCLK = $00A6;
  WM_NCMBUTTONDOWN   = $00A7;
  WM_NCMBUTTONUP     = $00A8;
  WM_NCMBUTTONDBLCLK = $00A9;

{ Mouse click activation support }
  WM_MOUSEACTIVATE   = $0021;

{ WM_MOUSEACTIVATE return codes }
  MA_ACTIVATE        = 1;
  MA_ACTIVATEANDEAT  = 2;
  MA_NOACTIVATE      = 3;

{ SetWindowsHook() mouse hook }
  WH_MOUSE           = 7;

type
  PMOUSEHOOKSTRUCT = ^MOUSEHOOKSTRUCT;
  LPMOUSEHOOKSTRUCT = ^MOUSEHOOKSTRUCT; far;
  MOUSEHOOKSTRUCT = record
    pt: POINT;
    hwnd: HWND;
    wHitTestCode: UINT;
    dwExtraInfo: DWORD;
  end;
  TMouseHookStruct = MOUSEHOOKSTRUCT;

const
{ Mode control }
  WM_CANCELMODE = $001F;

type
{ Timer support }
  TIMERPROC = procedure(hwnd: HWND; msg, idTimer: UINT; dwTime: DWORD); far;

const
  WM_TIMER = $0113;

type
{ Accelerator support }
  HACCEL = THandle;

{ Menu support }

{ Menu template header }
  PMENUITEMTEMPLATEHEADER = ^MENUITEMTEMPLATEHEADER;
  LPMENUITEMTEMPLATEHEADER = ^MENUITEMTEMPLATEHEADER; far;
  MENUITEMTEMPLATEHEADER = record
    versionNumber: UINT;
    offset: UINT;
  end;
  TMenuItemTemplateHeader = MENUITEMTEMPLATEHEADER;

{ Menu template item struct }
  PMENUITEMTEMPLATE = ^MENUITEMTEMPLATE;
  LPMENUITEMTEMPLATE = ^MENUITEMTEMPLATE; far;
  MENUITEMTEMPLATE = record
    mtOption: UINT;
    mtID: UINT;
    mtString: array [0..0] of Char;
  end;
  TMenuItemTemplate = MENUITEMTEMPLATE;

const
  MF_INSERT          = $0000;
  MF_CHANGE          = $0080;
  MF_APPEND          = $0100;
  MF_DELETE          = $0200;
  MF_REMOVE          = $1000;

{ Menu flags for Add/Check/EnableMenuItem() }
  MF_BYCOMMAND       = $0000;
  MF_BYPOSITION      = $0400;

  MF_SEPARATOR       = $0800;

  MF_ENABLED         = $0000;
  MF_GRAYED          = $0001;
  MF_DISABLED        = $0002;

  MF_UNCHECKED       = $0000;
  MF_CHECKED         = $0008;
  MF_USECHECKBITMAPS = $0200;

  MF_STRING          = $0000;
  MF_BITMAP          = $0004;
  MF_OWNERDRAW       = $0100;

  MF_POPUP           = $0010;
  MF_MENUBARBREAK    = $0020;
  MF_MENUBREAK       = $0040;

  MF_UNHILITE        = $0000;
  MF_HILITE          = $0080;

  MF_SYSMENU         = $2000;
  MF_HELP            = $4000;
  MF_MOUSESELECT     = $8000;


  MF_END             = $0080;  { Only valid in menu resource templates }

{ Flags for TrackPopupMenu }
  TPM_LEFTBUTTON     = $0000;

{ Menu messages }
  WM_INITMENU        = $0116;
  WM_INITMENUPOPUP   = $0117;

  WM_MENUSELECT      = $011F;
  WM_MENUCHAR        = $0120;

{ Menu and control command messages }
  WM_COMMAND         = $0111;

{ Scroll bar support }
  WM_HSCROLL        = $0114;
  WM_VSCROLL        = $0115;

{ WM_H/VSCROLL commands }
  SB_LINEUP         = 0;
  SB_LINELEFT       = 0;
  SB_LINEDOWN       = 1;
  SB_LINERIGHT      = 1;
  SB_PAGEUP         = 2;
  SB_PAGELEFT       = 2;
  SB_PAGEDOWN       = 3;
  SB_PAGERIGHT      = 3;
  SB_THUMBPOSITION  = 4;
  SB_THUMBTRACK     = 5;
  SB_TOP            = 6;
  SB_LEFT           = 6;
  SB_BOTTOM         = 7;
  SB_RIGHT          = 7;
  SB_ENDSCROLL      = 8;

{ Scroll bar selection constants }
  SB_HORZ           = 0;
  SB_VERT           = 1;
  SB_CTL            = 2;
  SB_BOTH           = 3;

{ EnableScrollBar() flags }
  ESB_ENABLE_BOTH   = $0000;
  ESB_DISABLE_BOTH  = $0003;

  ESB_DISABLE_LEFT  = $0001;
  ESB_DISABLE_RIGHT = $0002;

  ESB_DISABLE_UP    = $0001;
  ESB_DISABLE_DOWN  = $0002;

  ESB_DISABLE_LTUP  = ESB_DISABLE_LEFT;
  ESB_DISABLE_RTDN  = ESB_DISABLE_RIGHT;

{ Clipboard manager }

{ Predefined Clipboard Formats }
  CF_TEXT             = 1;
  CF_BITMAP           = 2;
  CF_METAFILEPICT     = 3;
  CF_SYLK             = 4;
  CF_DIF              = 5;
  CF_TIFF             = 6;
  CF_OEMTEXT          = 7;
  CF_DIB              = 8;
  CF_PALETTE          = 9;
  CF_PENDATA          = 10;
  CF_RIFF             = 11;
  CF_WAVE             = 12;

  CF_OWNERDISPLAY     = $0080;
  CF_DSPTEXT          = $0081;
  CF_DSPBITMAP        = $0082;
  CF_DSPMETAFILEPICT  = $0083;

{ "Private" formats don't get GlobalFree()'d }
  CF_PRIVATEFIRST     = $0200;
  CF_PRIVATELAST      = $02FF;

{ "GDIOBJ" formats do get DeleteObject()'d }
  CF_GDIOBJFIRST      = $0300;
  CF_GDIOBJLAST       = $03FF;

{ Clipboard command messages }
  WM_CUT              = $0300;
  WM_COPY             = $0301;
  WM_PASTE            = $0302;
  WM_CLEAR            = $0303;
  WM_UNDO             = $0304;

{ Clipboard owner messages }
  WM_RENDERFORMAT     = $0305;
  WM_RENDERALLFORMATS = $0306;
  WM_DESTROYCLIPBOARD = $0307;

{ Clipboard viewer messages }
  WM_DRAWCLIPBOARD    = $0308;
  WM_PAINTCLIPBOARD   = $0309;
  WM_SIZECLIPBOARD    = $030B;
  WM_VSCROLLCLIPBOARD = $030A;
  WM_HSCROLLCLIPBOARD = $030E;
  WM_ASKCBFORMATNAME  = $030C;
  WM_CHANGECBCHAIN    = $030D;

{ Mouse cursor support }
{ Standard cursor resource IDs }
  IDC_ARROW    = MAKEINTRESOURCE(32512);
  IDC_IBEAM    = MAKEINTRESOURCE(32513);
  IDC_WAIT     = MAKEINTRESOURCE(32514);
  IDC_CROSS    = MAKEINTRESOURCE(32515);
  IDC_UPARROW  = MAKEINTRESOURCE(32516);
  IDC_SIZE     = MAKEINTRESOURCE(32640);
  IDC_ICON     = MAKEINTRESOURCE(32641);
  IDC_SIZENWSE = MAKEINTRESOURCE(32642);
  IDC_SIZENESW = MAKEINTRESOURCE(32643);
  IDC_SIZEWE   = MAKEINTRESOURCE(32644);
  IDC_SIZENS   = MAKEINTRESOURCE(32645);

  WM_SETCURSOR = $0020;

{ Icon support }
{ Standard icon resource IDs }
  IDI_APPLICATION = MAKEINTRESOURCE(32512);
  IDI_HAND        = MAKEINTRESOURCE(32513);
  IDI_QUESTION    = MAKEINTRESOURCE(32514);
  IDI_EXCLAMATION = MAKEINTRESOURCE(32515);
  IDI_ASTERISK    = MAKEINTRESOURCE(32516);

{ Message Box support }
  MB_OK               = $0000;
  MB_OKCANCEL         = $0001;
  MB_ABORTRETRYIGNORE = $0002;
  MB_YESNOCANCEL      = $0003;
  MB_YESNO            = $0004;
  MB_RETRYCANCEL      = $0005;
  MB_TYPEMASK         = $000F;

  MB_ICONHAND         = $0010;
  MB_ICONQUESTION     = $0020;
  MB_ICONEXCLAMATION  = $0030;
  MB_ICONASTERISK     = $0040;
  MB_ICONMASK         = $00F0;

  MB_ICONINFORMATION  = MB_ICONASTERISK;
  MB_ICONSTOP         = MB_ICONHAND;

  MB_DEFBUTTON1       = $0000;
  MB_DEFBUTTON2       = $0100;
  MB_DEFBUTTON3       = $0200;
  MB_DEFMASK          = $0F00;

  MB_APPLMODAL        = $0000;
  MB_SYSTEMMODAL      = $1000;
  MB_TASKMODAL        = $2000;

  MB_NOFOCUS          = $8000;

{ WM_SYSCOMMAND support }

  WM_SYSCOMMAND = $0112;

{ System Menu Command Values }
  SC_SIZE       = $F000;
  SC_MOVE       = $F010;
  SC_MINIMIZE   = $F020;
  SC_MAXIMIZE   = $F030;
  SC_NEXTWINDOW = $F040;
  SC_PREVWINDOW = $F050;
  SC_CLOSE      = $F060;
  SC_VSCROLL    = $F070;
  SC_HSCROLL    = $F080;
  SC_MOUSEMENU  = $F090;
  SC_KEYMENU    = $F100;
  SC_ARRANGE    = $F110;
  SC_RESTORE    = $F120;
  SC_TASKLIST   = $F130;
  SC_SCREENSAVE = $F140;
  SC_HOTKEY     = $F150;

{ Obsolete names }
  SC_ICON = SC_MINIMIZE;
  SC_ZOOM = SC_MAXIMIZE;

{ MDI Support }

type
{ CreateWindow lpParams structure for creating MDI client }
  PCLIENTCREATESTRUCT = ^CLIENTCREATESTRUCT;
  LPCLIENTCREATESTRUCT = ^CLIENTCREATESTRUCT; far;
  CLIENTCREATESTRUCT = record
    hWindowMenu: HMENU;
    idFirstChild: UINT;
  end;
  TClientCreateStruct = CLIENTCREATESTRUCT;

const
{ MDI messages }
  WM_MDICREATE      = $0220;
  WM_MDIDESTROY     = $0221;
  WM_MDIACTIVATE    = $0222;
  WM_MDIRESTORE     = $0223;
  WM_MDINEXT        = $0224;
  WM_MDIMAXIMIZE    = $0225;
  WM_MDITILE        = $0226;
  WM_MDICASCADE     = $0227;
  WM_MDIICONARRANGE = $0228;
  WM_MDIGETACTIVE   = $0229;
  WM_MDISETMENU     = $0230;

type
{ WM_MDICREATE message structure }
  PMDICREATESTRUCT = ^MDICREATESTRUCT;
  LPMDICREATESTRUCT = ^MDICREATESTRUCT; far;
  MDICREATESTRUCT = record
    szClass: LPCSTR;
    szTitle: LPCSTR;
    hOwner: HINST;
    x: SmallInt;
    y: SmallInt;
    cx: SmallInt;
    cy: SmallInt;
    style: DWORD;
    lParam: LPARAM;
  end;
  TMDICreateStruct = MDICREATESTRUCT;

const
  WM_CHILDACTIVATE  = $0022;

{ Dialog and Control Management }

{ Dialog window class }
  WC_DIALOG = MAKEINTATOM($8002);

{ cbWndExtra bytes needed by dialog manager for dialog classes }
  DLGWINDOWEXTRA = 30;

{ Dialog styles }
  DS_ABSALIGN          = $01;
  DS_SYSMODAL          = $02;
  DS_LOCALEDIT         = $20;
  DS_SETFONT           = $40;
  DS_MODALFRAME        = $80;
  DS_NOIDLEMSG         = $100;

{ Dialog messages }
  DM_GETDEFID          = (WM_USER+0);
  DM_SETDEFID          = (WM_USER+1);

{ Returned in HIWORD() of DM_GETDEFID result if msg is supported }
  DC_HASDEFID          = $534B;

{ Dialog notification messages }
  WM_INITDIALOG        = $0110;
  WM_NEXTDLGCTL        = $0028;

  WM_PARENTNOTIFY      = $0210;

  WM_ENTERIDLE         = $0121;

type
  DLGPROC = function(hwndDlg: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; far;

const
{ Get/SetWindowWord/Long offsets for use with WC_DIALOG windows }
  DWL_MSGRESULT        = 0;
  DWL_DLGPROC          = 4;
  DWL_USER             = 8;

  WM_GETDLGCODE        = $0087;

{ dialog codes }
  DLGC_WANTARROWS      = $0001;
  DLGC_WANTTAB         = $0002;
  DLGC_WANTALLKEYS     = $0004;
  DLGC_WANTMESSAGE     = $0004;
  DLGC_HASSETSEL       = $0008;
  DLGC_DEFPUSHBUTTON   = $0010;
  DLGC_UNDEFPUSHBUTTON = $0020;
  DLGC_RADIOBUTTON     = $0040;
  DLGC_WANTCHARS       = $0080;
  DLGC_STATIC          = $0100;
  DLGC_BUTTON          = $2000;

  WM_CTLCOLOR          = $0019;

{ WM_CTLCOLOR control IDs }
  CTLCOLOR_MSGBOX      = 0;
  CTLCOLOR_EDIT        = 1;
  CTLCOLOR_LISTBOX     = 2;
  CTLCOLOR_BTN         = 3;
  CTLCOLOR_DLG         = 4;
  CTLCOLOR_SCROLLBAR   = 5;
  CTLCOLOR_STATIC      = 6;

  WM_SETFONT           = $0030;
  WM_GETFONT           = $0031;

{ Standard dialog button IDs }
  IDOK                 = 1;
  IDCANCEL             = 2;
  IDABORT              = 3;
  IDRETRY              = 4;
  IDIGNORE             = 5;
  IDYES                = 6;
  IDNO                 = 7;

{ Owner draw control support }

{ Owner draw control types }
  ODT_MENU       = 1;
  ODT_LISTBOX    = 2;
  ODT_COMBOBOX   = 3;
  ODT_BUTTON     = 4;

{ Owner draw actions }
  ODA_DRAWENTIRE = $0001;
  ODA_SELECT     = $0002;
  ODA_FOCUS      = $0004;

{ Owner draw state }
  ODS_SELECTED   = $0001;
  ODS_GRAYED     = $0002;
  ODS_DISABLED   = $0004;
  ODS_CHECKED    = $0008;
  ODS_FOCUS      = $0010;

  WM_DRAWITEM    = $002B;

type
  PDRAWITEMSTRUCT = ^DRAWITEMSTRUCT;
  NPDRAWITEMSTRUCT = ^DRAWITEMSTRUCT; near;
  LPDRAWITEMSTRUCT = ^DRAWITEMSTRUCT; far;
  DRAWITEMSTRUCT = record
    CtlType: UINT;
    CtlID: UINT;
    itemID: UINT;
    itemAction: UINT;
    itemState: UINT;
    hwndItem: HWND;
    hDC: HDC;
    rcItem: RECT;
    itemData: DWORD;
  end;
  TDrawItemStruct = DRAWITEMSTRUCT;

const
  WM_MEASUREITEM = $002C;

type
  PMEASUREITEMSTRUCT = ^MEASUREITEMSTRUCT;
  NPMEASUREITEMSTRUCT = ^MEASUREITEMSTRUCT; near;
  LPMEASUREITEMSTRUCT = ^MEASUREITEMSTRUCT; far;
  MEASUREITEMSTRUCT = record
    CtlType: UINT;
    CtlID: UINT;
    itemID: UINT;
    itemWidth: UINT;
    itemHeight: UINT;
    itemData: DWORD;
  end;
  TMeasureItemStruct = MEASUREITEMSTRUCT;

const
  WM_DELETEITEM  = $002D;

type
  PDELETEITEMSTRUCT = ^DELETEITEMSTRUCT;
  NPDELETEITEMSTRUCT = ^DELETEITEMSTRUCT; near;
  LPDELETEITEMSTRUCT = ^DELETEITEMSTRUCT; far;
  DELETEITEMSTRUCT = record
    CtlType: UINT;
    CtlID: UINT;
    itemID: UINT;
    hwndItem: HWND;
    itemData: DWORD;
  end;
  TDeleteItemStruct = DELETEITEMSTRUCT;

const
  WM_COMPAREITEM = $0039;

type
  PCOMPAREITEMSTRUCT = ^COMPAREITEMSTRUCT;
  NPCOMPAREITEMSTRUCT = ^COMPAREITEMSTRUCT; near;
  LPCOMPAREITEMSTRUCT = ^COMPAREITEMSTRUCT; far;
  COMPAREITEMSTRUCT = record
    CtlType: UINT;
    CtlID: UINT;
    hwndItem: HWND;
    itemID1: UINT;
    itemData1: DWORD;
    itemID2: UINT;
    itemData2: DWORD;
  end;
  TCompareItemStruct = COMPAREITEMSTRUCT;

{ Static control }

const
{ Static Control Styles }
  SS_LEFT           = $00000000;
  SS_CENTER         = $00000001;
  SS_RIGHT          = $00000002;
  SS_ICON           = $00000003;
  SS_BLACKRECT      = $00000004;
  SS_GRAYRECT       = $00000005;
  SS_WHITERECT      = $00000006;
  SS_BLACKFRAME     = $00000007;
  SS_GRAYFRAME      = $00000008;
  SS_WHITEFRAME     = $00000009;
  SS_SIMPLE         = $0000000B;
  SS_LEFTNOWORDWRAP = $0000000C;
  SS_NOPREFIX       = $00000080;

{ Button control }

{ Button Control Styles }
  BS_PUSHBUTTON      = $00000000;
  BS_DEFPUSHBUTTON   = $00000001;
  BS_CHECKBOX        = $00000002;
  BS_AUTOCHECKBOX    = $00000003;
  BS_RADIOBUTTON     = $00000004;
  BS_3STATE          = $00000005;
  BS_AUTO3STATE      = $00000006;
  BS_GROUPBOX        = $00000007;
  BS_USERBUTTON      = $00000008;
  BS_AUTORADIOBUTTON = $00000009;
  BS_OWNERDRAW       = $0000000B;
  BS_LEFTTEXT        = $00000020;

{ Button Control Messages  }
  BM_GETCHECK        = (WM_USER+0);
  BM_SETCHECK        = (WM_USER+1);
  BM_GETSTATE        = (WM_USER+2);
  BM_SETSTATE        = (WM_USER+3);
  BM_SETSTYLE        = (WM_USER+4);

{ User Button Notification Codes }
  BN_CLICKED         = 0;
  BN_PAINT           = 1;
  BN_HILITE          = 2;
  BN_UNHILITE        = 3;
  BN_DISABLE         = 4;
  BN_DOUBLECLICKED   = 5;

{ Edit control }

{ Edit control styles }
  ES_LEFT            = $00000000;
  ES_CENTER          = $00000001;
  ES_RIGHT           = $00000002;
  ES_MULTILINE       = $00000004;
  ES_UPPERCASE       = $00000008;
  ES_LOWERCASE       = $00000010;
  ES_PASSWORD        = $00000020;
  ES_AUTOVSCROLL     = $00000040;
  ES_AUTOHSCROLL     = $00000080;
  ES_NOHIDESEL       = $00000100;
  ES_OEMCONVERT      = $00000400;

{ Edit control messages }
  EM_GETSEL          = (WM_USER+0);
  EM_SETSEL          = (WM_USER+1);
  EM_GETRECT         = (WM_USER+2);
  EM_SETRECT         = (WM_USER+3);
  EM_SETRECTNP       = (WM_USER+4);
  EM_LINESCROLL      = (WM_USER+6);
  EM_GETMODIFY       = (WM_USER+8);
  EM_SETMODIFY       = (WM_USER+9);
  EM_GETLINECOUNT    = (WM_USER+10);
  EM_LINEINDEX       = (WM_USER+11);
  EM_SETHANDLE       = (WM_USER+12);
  EM_GETHANDLE       = (WM_USER+13);
  EM_LINELENGTH      = (WM_USER+17);
  EM_REPLACESEL      = (WM_USER+18);
  EM_SETFONT         = (WM_USER+19);  { NOT IMPLEMENTED: use WM_SETFONT }
  EM_GETLINE         = (WM_USER+20);
  EM_LIMITTEXT       = (WM_USER+21);
  EM_CANUNDO         = (WM_USER+22);
  EM_UNDO            = (WM_USER+23);
  EM_FMTLINES        = (WM_USER+24);
  EM_LINEFROMCHAR    = (WM_USER+25);
  EM_SETWORDBREAK    = (WM_USER+26);  { NOT IMPLEMENTED: use EM_SETWORDBREAK }
  EM_SETTABSTOPS     = (WM_USER+27);
  EM_SETPASSWORDCHAR = (WM_USER+28);
  EM_EMPTYUNDOBUFFER = (WM_USER+29);

{ Edit control notification codes }
  EN_SETFOCUS        = $0100;
  EN_KILLFOCUS       = $0200;
  EN_CHANGE          = $0300;
  EN_UPDATE          = $0400;
  EN_ERRSPACE        = $0500;
  EN_MAXTEXT         = $0501;
  EN_HSCROLL         = $0601;
  EN_VSCROLL         = $0602;

{ Scroll bar control }
{ Also see scrolling support }

{ Scroll bar styles }
  SBS_HORZ                    = $0000;
  SBS_VERT                    = $0001;
  SBS_TOPALIGN                = $0002;
  SBS_LEFTALIGN               = $0002;
  SBS_BOTTOMALIGN             = $0004;
  SBS_RIGHTALIGN              = $0004;
  SBS_SIZEBOXTOPLEFTALIGN     = $0002;
  SBS_SIZEBOXBOTTOMRIGHTALIGN = $0004;
  SBS_SIZEBOX                 = $0008;

{ Listbox control }

{ Listbox styles }
  LBS_NOTIFY             = $0001;
  LBS_SORT               = $0002;
  LBS_NOREDRAW           = $0004;
  LBS_MULTIPLESEL        = $0008;
  LBS_OWNERDRAWFIXED     = $0010;
  LBS_OWNERDRAWVARIABLE  = $0020;
  LBS_HASSTRINGS         = $0040;
  LBS_USETABSTOPS        = $0080;
  LBS_NOINTEGRALHEIGHT   = $0100;
  LBS_MULTICOLUMN        = $0200;
  LBS_WANTKEYBOARDINPUT  = $0400;
  LBS_EXTENDEDSEL        = $0800;
  LBS_STANDARD           = (LBS_NOTIFY or LBS_SORT or WS_VSCROLL or WS_BORDER);

{ Listbox messages }
  LB_ADDSTRING           = (WM_USER+1);
  LB_INSERTSTRING        = (WM_USER+2);
  LB_DELETESTRING        = (WM_USER+3);
  LB_RESETCONTENT        = (WM_USER+5);
  LB_SETSEL              = (WM_USER+6);
  LB_SETCURSEL           = (WM_USER+7);
  LB_GETSEL              = (WM_USER+8);
  LB_GETCURSEL           = (WM_USER+9);
  LB_GETTEXT             = (WM_USER+10);
  LB_GETTEXTLEN          = (WM_USER+11);
  LB_GETCOUNT            = (WM_USER+12);
  LB_SELECTSTRING        = (WM_USER+13);
  LB_DIR                 = (WM_USER+14);
  LB_GETTOPINDEX         = (WM_USER+15);
  LB_FINDSTRING          = (WM_USER+16);
  LB_GETSELCOUNT         = (WM_USER+17);
  LB_GETSELITEMS         = (WM_USER+18);
  LB_SETTABSTOPS         = (WM_USER+19);
  LB_GETHORIZONTALEXTENT = (WM_USER+20);
  LB_SETHORIZONTALEXTENT = (WM_USER+21);
  LB_SETCOLUMNWIDTH      = (WM_USER+22);
  LB_SETTOPINDEX         = (WM_USER+24);
  LB_GETITEMRECT         = (WM_USER+25);
  LB_GETITEMDATA         = (WM_USER+26);
  LB_SETITEMDATA         = (WM_USER+27);
  LB_SELITEMRANGE        = (WM_USER+28);
  LB_SETCARETINDEX       = (WM_USER+31);
  LB_GETCARETINDEX       = (WM_USER+32);

{ Listbox notification codes }
  LBN_ERRSPACE           = (-2);
  LBN_SELCHANGE          = 1;
  LBN_DBLCLK             = 2;
  LBN_SELCANCEL          = 3;
  LBN_SETFOCUS           = 4;
  LBN_KILLFOCUS          = 5;

{ Listbox notification messages }
  WM_VKEYTOITEM          = $002E;
  WM_CHARTOITEM          = $002F;

{ Listbox message return values }
  LB_OKAY                = 0;
  LB_ERR                 = (-1);
  LB_ERRSPACE            = (-2);

  LB_CTLCODE             = 0;

{ Dialog directory support }

{ DlgDirList, DlgDirListComboBox flags values }
  DDL_READWRITE = $0000;
  DDL_READONLY  = $0001;
  DDL_HIDDEN    = $0002;
  DDL_SYSTEM    = $0004;
  DDL_DIRECTORY = $0010;
  DDL_ARCHIVE   = $0020;

  DDL_POSTMSGS  = $2000;
  DDL_DRIVES    = $4000;
  DDL_EXCLUSIVE = $8000;

{ Combo box control }

{ Combo box styles }
  CBS_SIMPLE            = $0001;
  CBS_DROPDOWN          = $0002;
  CBS_DROPDOWNLIST      = $0003;
  CBS_OWNERDRAWFIXED    = $0010;
  CBS_OWNERDRAWVARIABLE = $0020;
  CBS_AUTOHSCROLL       = $0040;
  CBS_OEMCONVERT        = $0080;
  CBS_SORT              = $0100;
  CBS_HASSTRINGS        = $0200;
  CBS_NOINTEGRALHEIGHT  = $0400;

{ Combo box messages }
  CB_GETEDITSEL         = (WM_USER+0);
  CB_LIMITTEXT          = (WM_USER+1);
  CB_SETEDITSEL         = (WM_USER+2);
  CB_ADDSTRING          = (WM_USER+3);
  CB_DELETESTRING       = (WM_USER+4);
  CB_DIR                = (WM_USER+5);
  CB_GETCOUNT           = (WM_USER+6);
  CB_GETCURSEL          = (WM_USER+7);
  CB_GETLBTEXT          = (WM_USER+8);
  CB_GETLBTEXTLEN       = (WM_USER+9);
  CB_INSERTSTRING       = (WM_USER+10);
  CB_RESETCONTENT       = (WM_USER+11);
  CB_FINDSTRING         = (WM_USER+12);
  CB_SELECTSTRING       = (WM_USER+13);
  CB_SETCURSEL          = (WM_USER+14);
  CB_SHOWDROPDOWN       = (WM_USER+15);
  CB_GETITEMDATA        = (WM_USER+16);
  CB_SETITEMDATA        = (WM_USER+17);

{ Combo box notification codes }
  CBN_ERRSPACE          = (-1);
  CBN_SELCHANGE         = 1;
  CBN_DBLCLK            = 2;
  CBN_SETFOCUS          = 3;
  CBN_KILLFOCUS         = 4;
  CBN_EDITCHANGE        = 5;
  CBN_EDITUPDATE        = 6;
  CBN_DROPDOWN          = 7;

{ Combo box message return values }
  CB_OKAY               = 0;
  CB_ERR                = (-1);
  CB_ERRSPACE           = (-2);

type
{ Windows hook support }
  LPHHOOK = ^HHOOK; far;
  HHOOK = DWORD;
  HOOKPROC = function(code: SmallInt; wParam: WPARAM; lParam: LPARAM): LRESULT; far;
  THookProc = HOOKPROC;

const
{ Standard hook code }
  HC_ACTION     = 0;

{ Obsolete hook codes (NO LONGER SUPPORTED) }
  HC_GETLPLPFN  = (-3);
  HC_LPLPFNNEXT = (-2);
  HC_LPFNNEXT   = (-1);

{ Computer-based-training (CBT) support }

  WM_QUEUESYNC      = $0023;

{ SetWindowsHook() code }
  WH_CBT            = 5;

  HCBT_MOVESIZE     = 0;
  HCBT_MINMAX       = 1;
  HCBT_QS           = 2;
  HCBT_CREATEWND    = 3;
  HCBT_DESTROYWND   = 4;
  HCBT_ACTIVATE     = 5;
  HCBT_CLICKSKIPPED = 6;
  HCBT_KEYSKIPPED   = 7;
  HCBT_SYSCOMMAND   = 8;
  HCBT_SETFOCUS     = 9;

{ Journalling support }

  WH_JOURNALRECORD   = 0;
  WH_JOURNALPLAYBACK = 1;

{ Journalling hook codes }
  HC_GETNEXT         = 1;
  HC_SKIP            = 2;
  HC_NOREMOVE        = 3;
  HC_NOREM           = HC_NOREMOVE;
  HC_SYSMODALON      = 4;
  HC_SYSMODALOFF     = 5;

type
{ Journalling message structure }
  PEVENTMSG = ^EVENTMSG;
  NPEVENTMSG = ^EVENTMSG; near;
  LPEVENTMSG = ^EVENTMSG; far;
  EVENTMSG = record
    message: UINT;
    paramL: UINT;
    paramH: UINT;
    time: DWORD;
  end;
  TEventMsg = EVENTMSG;

{ Help support }

const
{ WinHelp() commands }
  HELP_CONTEXT      = $0001;
  HELP_QUIT         = $0002;
  HELP_INDEX        = $0003;
  HELP_CONTENTS     = $0003;
  HELP_HELPONHELP   = $0004;
  HELP_SETINDEX     = $0005;
  HELP_SETCONTENTS  = $0005;
  HELP_CONTEXTPOPUP = $0008;
  HELP_FORCEFILE    = $0009;
  HELP_KEY          = $0101;
  HELP_COMMAND      = $0102;
  HELP_PARTIALKEY   = $0105;
  HELP_MULTIKEY     = $0201;
  HELP_SETWINPOS    = $0203;

type
  PMULTIKEYHELP = ^MULTIKEYHELP;
  LPMULTIKEYHELP = ^MULTIKEYHELP; far;
  MULTIKEYHELP = record
    mkSize: UINT;
    mkKeylist: BYTE;
    szKeyphrase: array [0..0] of BYTE;
  end;
  TMultiKeyHelp = MULTIKEYHELP;

  PHELPWININFO = ^HELPWININFO;
  NPHELPWININFO = ^HELPWININFO; near;
  LPHELPWININFO = ^HELPWININFO; far;
  HELPWININFO = record
    wStructSize: SmallInt;
    x: SmallInt;
    y: SmallInt;
    dx: SmallInt;
    dy: SmallInt;
    wMax: SmallInt;
    rgchMember: array [0..1] of char;
  end;
  THelpWinInfo = HELPWININFO;

{ Sound support }

const
{ SetSoundNoise() Sources }
  S_PERIOD512    = 0;
  S_PERIOD1024   = 1;
  S_PERIOD2048   = 2;
  S_PERIODVOICE  = 3;
  S_WHITE512     = 4;
  S_WHITE1024    = 5;
  S_WHITE2048    = 6;
  S_WHITEVOICE   = 7;

{ WaitSoundState() constants }
  S_QUEUEEMPTY   = 0;
  S_THRESHOLD    = 1;
  S_ALLTHRESHOLD = 2;

{ Accent Modes }
  S_NORMAL       = 0;
  S_LEGATO       = 1;
  S_STACCATO     = 2;

{ Error return values }
  S_SERDVNA      = (-1);
  S_SEROFM       = (-2);
  S_SERMACT      = (-3);
  S_SERQFUL      = (-4);
  S_SERBDNT      = (-5);
  S_SERDLN       = (-6);
  S_SERDCC       = (-7);
  S_SERDTP       = (-8);
  S_SERDVL       = (-9);
  S_SERDMD       = (-10);
  S_SERDSH       = (-11);
  S_SERDPT       = (-12);
  S_SERDFQ       = (-13);
  S_SERDDR       = (-14);
  S_SERDSR       = (-15);
  S_SERDST       = (-16);
