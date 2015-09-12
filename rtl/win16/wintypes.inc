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
