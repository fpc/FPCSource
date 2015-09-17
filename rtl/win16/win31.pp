unit win31;

{$MODE objfpc}

{$if defined(FPC_MM_COMPACT) or defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}
  {$define VAR_PARAMS_ARE_FAR}
{$endif}

interface

uses
  wintypes;

const
  GFSR_SYSTEMRESOURCES = $0000;
  GFSR_GDIRESOURCES    = $0001;
  GFSR_USERRESOURCES   = $0002;

{****** LogParamError/LogError values *}

{ Error modifier bits }

  ERR_WARNING           = $8000;
  ERR_PARAM             = $4000;

  ERR_SIZE_MASK         = $3000;
  ERR_BYTE              = $1000;
  ERR_WORD              = $2000;
  ERR_DWORD             = $3000;

{****** LogParamError() values *}

{ Generic parameter values }
  ERR_BAD_VALUE         = $6001;
  ERR_BAD_FLAGS         = $6002;
  ERR_BAD_INDEX         = $6003;
  ERR_BAD_DVALUE        = $7004;
  ERR_BAD_DFLAGS        = $7005;
  ERR_BAD_DINDEX        = $7006;
  ERR_BAD_PTR           = $7007;
  ERR_BAD_FUNC_PTR      = $7008;
  ERR_BAD_SELECTOR      = $6009;
  ERR_BAD_STRING_PTR    = $700a;
  ERR_BAD_HANDLE        = $600b;

{ KERNEL parameter errors }
  ERR_BAD_HINSTANCE     = $6020;
  ERR_BAD_HMODULE       = $6021;
  ERR_BAD_GLOBAL_HANDLE = $6022;
  ERR_BAD_LOCAL_HANDLE  = $6023;
  ERR_BAD_ATOM          = $6024;
  ERR_BAD_HFILE         = $6025;

{ USER parameter errors }
  ERR_BAD_HWND          = $6040;
  ERR_BAD_HMENU         = $6041;
  ERR_BAD_HCURSOR       = $6042;
  ERR_BAD_HICON         = $6043;
  ERR_BAD_HDWP          = $6044;
  ERR_BAD_CID           = $6045;
  ERR_BAD_HDRVR         = $6046;

{ GDI parameter errors }
  ERR_BAD_COORDS        = $7060;
  ERR_BAD_GDI_OBJECT    = $6061;
  ERR_BAD_HDC           = $6062;
  ERR_BAD_HPEN          = $6063;
  ERR_BAD_HFONT         = $6064;
  ERR_BAD_HBRUSH        = $6065;
  ERR_BAD_HBITMAP       = $6066;
  ERR_BAD_HRGN          = $6067;
  ERR_BAD_HPALETTE      = $6068;
  ERR_BAD_HMETAFILE     = $6069;


{**** LogError() values *}

{ KERNEL errors }
  ERR_GALLOC            = $0001;
  ERR_GREALLOC          = $0002;
  ERR_GLOCK             = $0003;
  ERR_LALLOC            = $0004;
  ERR_LREALLOC          = $0005;
  ERR_LLOCK             = $0006;
  ERR_ALLOCRES          = $0007;
  ERR_LOCKRES           = $0008;
  ERR_LOADMODULE        = $0009;

{ USER errors }
  ERR_CREATEDLG2        = $0041;
  ERR_CREATEDLG         = $0040;
  ERR_REGISTERCLASS     = $0042;
  ERR_DCBUSY            = $0043;
  ERR_CREATEWND         = $0044;
  ERR_STRUCEXTRA        = $0045;
  ERR_LOADSTR           = $0046;
  ERR_LOADMENU          = $0047;
  ERR_NESTEDBEGINPAINT  = $0048;
  ERR_BADINDEX          = $0049;
  ERR_CREATEMENU        = $004a;

{ GDI errors }
  ERR_CREATEDC          = $0080;
  ERR_CREATEMETA        = $0081;
  ERR_DELOBJSELECTED    = $0082;
  ERR_SELBITMAP         = $0083;

type
{ Debugging support (DEBUG SYSTEM ONLY) }
  LPWINDEBUGINFO = ^WINDEBUGINFO; far;
  WINDEBUGINFO = record
    flags: UINT;
    dwOptions: DWORD;
    dwFilter: DWORD;
    achAllocModule: array [0..7] of char;
    dwAllocBreak: DWORD;
    dwAllocCount: DWORD;
  end;
  PWinDebugInfo = ^TWinDebugInfo;
  TWinDebugInfo = WINDEBUGINFO;

const
{ WINDEBUGINFO flags values }
  WDI_OPTIONS           = $0001;
  WDI_FILTER            = $0002;
  WDI_ALLOCBREAK        = $0004;

{ dwOptions values }
  DBO_CHECKHEAP         = $0001;
  DBO_BUFFERFILL        = $0004;
  DBO_DISABLEGPTRAPPING = $0010;
  DBO_CHECKFREE         = $0020;

  DBO_SILENT            = $8000;

  DBO_TRACEBREAK        = $2000;
  DBO_WARNINGBREAK      = $1000;
  DBO_NOERRORBREAK      = $0800;
  DBO_NOFATALBREAK      = $0400;
  DBO_INT3BREAK         = $0100;

{ DebugOutput flags values }
  DBF_TRACE             = $0000;
  DBF_WARNING           = $4000;
  DBF_ERROR             = $8000;
  DBF_FATAL             = $c000;

{ dwFilter values }
  DBF_KERNEL            = $1000;
  DBF_KRN_MEMMAN        = $0001;
  DBF_KRN_LOADMODULE    = $0002;
  DBF_KRN_SEGMENTLOAD   = $0004;
  DBF_USER              = $0800;
  DBF_GDI               = $0400;
  DBF_MMSYSTEM          = $0040;
  DBF_PENWIN            = $0020;
  DBF_APPLICATION       = $0008;
  DBF_DRIVER            = $0010;

{ ExitWindows values }
  EW_REBOOTSYSTEM = $43;

{ Predefined Resource Types }
  OBM_UPARROWI    = 32737;
  OBM_DNARROWI    = 32736;
  OBM_RGARROWI    = 32735;
  OBM_LFARROWI    = 32734;

type
{ GDI typedefs, structures, and functions }
  PSIZE = ^SIZE;
  NPSIZE = ^SIZE; near;
  LPSIZE = ^SIZE; far;
  SIZE = record
    cx: SmallInt;
    cy: SmallInt;
  end;
  TSize = SIZE;

const
{ Drawing bounds accumulation APIs }
  DCB_RESET      = $0001;
  DCB_ACCUMULATE = $0002;
  DCB_DIRTY      = DCB_ACCUMULATE;
  DCB_SET        = DCB_RESET or DCB_ACCUMULATE;
  DCB_ENABLE     = $0004;
  DCB_DISABLE    = $0008;

{ Color support }
  COLOR_INACTIVECAPTIONTEXT = 19;
  COLOR_BTNHIGHLIGHT        = 20;

{ Font support }
{ OutPrecision values }
  OUT_TT_PRECIS      = 4;
  OUT_DEVICE_PRECIS  = 5;
  OUT_RASTER_PRECIS  = 6;
  OUT_TT_ONLY_PRECIS = 7;

{ ClipPrecision values }
  CLIP_LH_ANGLES = $10;
  CLIP_TT_ALWAYS = $20;
  CLIP_EMBEDDED  = $80;

{ tmPitchAndFamily values }
  TMPF_TRUETYPE = $04;

type
  PPANOSE = ^PANOSE;
  LPPANOSE = ^PANOSE; far;
  PANOSE = record
    bFamilyType: BYTE;
    bSerifStyle: BYTE;
    bWeight: BYTE;
    bProportion: BYTE;
    bContrast: BYTE;
    bStrokeVariation: BYTE;
    bArmStyle: BYTE;
    bLetterform: BYTE;
    bMidline: BYTE;
    bXHeight: BYTE;
  end;
  TPanose = PANOSE;

  POUTLINETEXTMETRIC = ^OUTLINETEXTMETRIC;
  LPOUTLINETEXTMETRIC = ^OUTLINETEXTMETRIC; far;
  OUTLINETEXTMETRIC = record
    otmSize: UINT;
    otmTextMetrics: TEXTMETRIC;
    otmFiller: BYTE;
    otmPanoseNumber: PANOSE;
    otmfsSelection: UINT;
    otmfsType: UINT;
    otmsCharSlopeRise: SmallInt;
    otmsCharSlopeRun: SmallInt;
    otmItalicAngle: SmallInt;
    otmEMSquare: UINT;
    otmAscent: SmallInt;
    otmDescent: SmallInt;
    otmLineGap: UINT;
    otmsCapEmHeight: UINT;
    otmsXHeight: UINT;
    otmrcFontBox: RECT;
    otmMacAscent: SmallInt;
    otmMacDescent: SmallInt;
    otmMacLineGap: UINT;
    otmusMinimumPPEM: UINT;
    otmptSubscriptSize: POINT;
    otmptSubscriptOffset: POINT;
    otmptSuperscriptSize: POINT;
    otmptSuperscriptOffset: POINT;
    otmsStrikeoutSize: UINT;
    otmsStrikeoutPosition: SmallInt;
    otmsUnderscorePosition: SmallInt;
    otmsUnderscoreSize: SmallInt;
    otmpFamilyName: PSTR;
    otmpFaceName: PSTR;
    otmpStyleName: PSTR;
    otmpFullName: PSTR;
  end;
  TOutlineTextMetric = OUTLINETEXTMETRIC;

{ Structure passed to FONTENUMPROC }
{ NOTE: NEWTEXTMETRIC is the same as TEXTMETRIC plus 4 new fields }
  PNEWTEXTMETRIC = ^NEWTEXTMETRIC;
  NPNEWTEXTMETRIC = ^NEWTEXTMETRIC; near;
  LPNEWTEXTMETRIC = ^NEWTEXTMETRIC; far;
  NEWTEXTMETRIC = record
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
    ntmFlags: DWORD;
    ntmSizeEM: UINT;
    ntmCellHeight: UINT;
    ntmAvgWidth: UINT;
  end;
  TNewTextMetric = NEWTEXTMETRIC;

const
{ ntmFlags field flags }
  NTM_REGULAR = $00000040;
  NTM_BOLD    = $00000020;
  NTM_ITALIC  = $00000001;

  LF_FULLFACESIZE = 64;

type
{ Structure passed to FONTENUMPROC }
  PENUMLOGFONT = ^ENUMLOGFONT;
  LPENUMLOGFONT = ^ENUMLOGFONT; far;
  ENUMLOGFONT = record
    elfLogFont: LOGFONT;
    elfFullName: array [0..LF_FULLFACESIZE-1] of char;
    elfStyle: array [0..LF_FACESIZE-1] of char;
  end;
  TEnumLogFont = ENUMLOGFONT;

  FONTENUMPROC = function(lpelf: LPENUMLOGFONT; lpntm: LPNEWTEXTMETRIC; FontType: SmallInt; lpData: LPARAM): SmallInt; far;

const
{ EnumFonts font type values }
  TRUETYPE_FONTTYPE = $0004;

type
  PGLYPHMETRICS = ^GLYPHMETRICS;
  LPGLYPHMETRICS = ^GLYPHMETRICS; far;
  GLYPHMETRICS = record
    gmBlackBoxX: UINT;
    gmBlackBoxY: UINT;
    gmptGlyphOrigin: POINT;
    gmCellIncX: SmallInt;
    gmCellIncY: SmallInt;
  end;
  TGlyphMetrics = GLYPHMETRICS;

  PFIXED = ^FIXED;
  LPFIXED = ^FIXED; far;
  FIXED = record
    fract: UINT;
    value: SmallInt;
  end;
  TFixed = FIXED;

  PMAT2 = ^MAT2;
  LPMAT2 = ^MAT2; far;
  MAT2 = record
    eM11: FIXED;
    eM12: FIXED;
    eM21: FIXED;
    eM22: FIXED;
  end;
  TMat2 = MAT2;

const
{ GetGlyphOutline constants }
  GGO_METRICS     =  0;
  GGO_BITMAP      =  1;
  GGO_NATIVE      =  2;

  TT_POLYGON_TYPE = 24;

  TT_PRIM_LINE    =  1;
  TT_PRIM_QSPLINE =  2;

type
  PPOINTFX = ^POINTFX;
  LPPOINTFX = ^POINTFX; far;
  POINTFX = record
    x: FIXED;
    y: FIXED;
  end;
  TPointFX = POINTFX;

  PTTPOLYCURVE = ^TTPOLYCURVE;
  LPTTPOLYCURVE = ^TTPOLYCURVE; far;
  TTPOLYCURVE = record
    wType: UINT;
    cpfx: UINT;
    apfx: array [0..0] of POINTFX;
  end;
  TTTPOLYCURVE = TTPolyCurve;

  PTTPOLYGONHEADER = ^TTPOLYGONHEADER;
  LPTTPOLYGONHEADER = ^TTPOLYGONHEADER; far;
  TTPOLYGONHEADER = record
    cb: DWORD;
    dwType: DWORD;
    pfxStart: POINTFX;
  end;
  TTTPolygonHeader = TTPOLYGONHEADER;

  PABC = ^ABC;
  LPABC = ^ABC; far;
  ABC = record
    abcA: SmallInt;
    abcB: UINT;
    abcC: SmallInt;
  end;
  TABC = ABC;

  PKERNINGPAIR = ^KERNINGPAIR;
  LPKERNINGPAIR = ^KERNINGPAIR; far;
  KERNINGPAIR = record
    wFirst: WORD;
    wSecond: WORD;
    iKernAmount: SmallInt;
  end;
  TKerningPair = KERNINGPAIR;

  PRASTERIZER_STATUS = ^RASTERIZER_STATUS;
  LPRASTERIZER_STATUS = ^RASTERIZER_STATUS; far;
  RASTERIZER_STATUS = record
    nSize: SmallInt;
    wFlags: SmallInt;
    nLanguageID: SmallInt;
  end;
  TRasterizer_Status = RASTERIZER_STATUS;

const
{ bits defined in wFlags of RASTERIZER_STATUS }
  TT_AVAILABLE = $0001;
  TT_ENABLED   = $0002;

type
{ Printing support }
  PDOCINFO = ^DOCINFO;
  LPDOCINFO = ^DOCINFO; far;
  DOCINFO = record
    cbSize: SmallInt;
    lpszDocName: LPCSTR;
    lpszOutput: LPCSTR;
  end;
  TDocInfo = DOCINFO;

{ System Metrics }
const
{ GetSystemMetrics() codes }
  SM_CXDOUBLECLK       = 36;
  SM_CYDOUBLECLK       = 37;
  SM_CXICONSPACING     = 38;
  SM_CYICONSPACING     = 39;
  SM_MENUDROPALIGNMENT = 40;
  SM_PENWINDOWS        = 41;
  SM_DBCSENABLED       = 42;
  SM_CMETRICS          = 43;

{ System Parameters support }
  SPI_GETBEEP               = 1;
  SPI_SETBEEP               = 2;
  SPI_GETMOUSE              = 3;
  SPI_SETMOUSE              = 4;
  SPI_GETBORDER             = 5;
  SPI_SETBORDER             = 6;
  SPI_GETKEYBOARDSPEED      = 10;
  SPI_SETKEYBOARDSPEED      = 11;
  SPI_LANGDRIVER            = 12;
  SPI_ICONHORIZONTALSPACING = 13;
  SPI_GETSCREENSAVETIMEOUT  = 14;
  SPI_SETSCREENSAVETIMEOUT  = 15;
  SPI_GETSCREENSAVEACTIVE   = 16;
  SPI_SETSCREENSAVEACTIVE   = 17;
  SPI_GETGRIDGRANULARITY    = 18;
  SPI_SETGRIDGRANULARITY    = 19;
  SPI_SETDESKWALLPAPER      = 20;
  SPI_SETDESKPATTERN        = 21;
  SPI_GETKEYBOARDDELAY      = 22;
  SPI_SETKEYBOARDDELAY      = 23;
  SPI_ICONVERTICALSPACING   = 24;
  SPI_GETICONTITLEWRAP      = 25;
  SPI_SETICONTITLEWRAP      = 26;
  SPI_GETMENUDROPALIGNMENT  = 27;
  SPI_SETMENUDROPALIGNMENT  = 28;
  SPI_SETDOUBLECLKWIDTH     = 29;
  SPI_SETDOUBLECLKHEIGHT    = 30;
  SPI_GETICONTITLELOGFONT   = 31;
  SPI_SETDOUBLECLICKTIME    = 32;
  SPI_SETMOUSEBUTTONSWAP    = 33;
  SPI_SETICONTITLELOGFONT   = 34;
  SPI_GETFASTTASKSWITCH     = 35;
  SPI_SETFASTTASKSWITCH     = 36;

{ SystemParametersInfo flags }
  SPIF_UPDATEINIFILE    = $0001;
  SPIF_SENDWININICHANGE = $0002;

{ Window message support }

{ GetQueueStatus flags }
  QS_KEY         = $0001;
  QS_MOUSEMOVE   = $0002;
  QS_MOUSEBUTTON = $0004;
  QS_MOUSE       = QS_MOUSEMOVE or QS_MOUSEBUTTON;
  QS_POSTMESSAGE = $0008;
  QS_TIMER       = $0010;
  QS_PAINT       = $0020;
  QS_SENDMESSAGE = $0040;

  QS_ALLINPUT    = $007f;

{ Power management }
  WM_POWER = $0048;

{ wParam for WM_POWER window message and DRV_POWER driver notification }
  PWR_OK             = 1;
  PWR_FAIL           = (-1);
  PWR_SUSPENDREQUEST = 1;
  PWR_SUSPENDRESUME  = 2;
  PWR_CRITICALRESUME = 3;

{ Window class management }
{ Class field offsets for GetClassLong() and GetClassWord() }
  GCW_ATOM = (-32);

function GetFreeSystemResources(SysResource: UINT): UINT; external 'USER';

procedure LogError(err: UINT; lpInfo: FarPointer); external 'KERNEL';
procedure LogParamError(err: UINT; lpfn: FARPROC; param: FarPointer); external 'KERNEL';

function GetWinDebugInfo(lpwdi: LPWINDEBUGINFO; flags: UINT): BOOL; external 'KERNEL';
function SetWinDebugInfo(lpwdi: LPWINDEBUGINFO): BOOL; external 'KERNEL';

procedure DebugOutput(flags: UINT; lpsz: LPCSTR; etc: array of const); cdecl; external 'KERNEL' name '_DebugOutput';

function ExitWindowsExec(Exe, Params: LPCSTR): BOOL; external 'USER';


{ Pointer validation }

function IsBadReadPtr(lp: FarPointer; cb: UINT): BOOL; external 'KERNEL';
function IsBadWritePtr(lp: FarPointer; cb: UINT): BOOL; external 'KERNEL';
function IsBadHugeReadPtr(lp: HugePointer; cb: DWORD): BOOL; external 'KERNEL';
function IsBadHugeReadPtr(lp: FarPointer; cb: DWORD): BOOL; external 'KERNEL';
function IsBadHugeWritePtr(lp: HugePointer; cb: DWORD): BOOL; external 'KERNEL';
function IsBadHugeWritePtr(lp: FarPointer; cb: DWORD): BOOL; external 'KERNEL';
function IsBadCodePtr(lpfn: FARPROC): BOOL; external 'KERNEL';
function IsBadStringPtr(lpsz: LPSTR; cchMax: UINT): BOOL; external 'KERNEL';

{ Task Management }

function IsTask(Task: HTASK): BOOL; external 'KERNEL';

{ File I/O }

function _hread(FileHandle: HFILE; Buffer: HugePointer; Bytes: LongInt): LongInt; external 'KERNEL';
function _hwrite(FileHandle: HFILE; Buffer: HugePointer; Bytes: LongInt): LongInt; external 'KERNEL';

{ International & Char Translation Support }

function lstrcpyn(lpszString1: LPSTR; lpszString2: LPCSTR; cChars: SmallInt): LPSTR; external 'KERNEL';
procedure hmemcpy(hpvDest, hpvSource: HugePointer; cbCopy: LongInt); external 'KERNEL';
procedure hmemcpy(hpvDest, hpvSource: FarPointer; cbCopy: LongInt); external 'KERNEL';

function IsDBCSLeadByte(bTestChar: BYTE): BOOL; external 'KERNEL';

{ Drawing bounds accumulation APIs }
function SetBoundsRect(hDC: HDC; lprcBounds: LPRECT; flags: UINT): UINT; external 'GDI';
function GetBoundsRect(hDC: HDC; lprcBounds: LPRECT; flags: UINT): UINT; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function SetBoundsRect(hDC: HDC; const lprcBounds: RECT; flags: UINT): UINT; external 'GDI';
function GetBoundsRect(hDC: HDC; var lprcBounds: RECT; flags: UINT): UINT; external 'GDI';
{$endif}

{ Coordinate transformation support }
function SetWindowOrgEx(hdc: HDC; nX, nY: SmallInt; lpPoint: LPPOINT): BOOL; external 'GDI';
function GetWindowOrgEx(hdc: HDC; lpPoint: LPPOINT): BOOL; external 'GDI';

function SetWindowExtEx(hdc: HDC; nX, nY: SmallInt; lpSize: LPSIZE): BOOL; external 'GDI';
function GetWindowExtEx(hdc: HDC; lpSize: LPSIZE): BOOL; external 'GDI';

function OffsetWindowOrgEx(hdc: HDC; nX, nY: SmallInt; lpPoint: LPPOINT): BOOL; external 'GDI';
function ScaleWindowExtEx(hdc: HDC; nXnum, nXdenom, nYnum, nYdenom: SmallInt; lpSize: LPSIZE): BOOL; external 'GDI';

function SetViewportExtEx(hdc: HDC; nX, nY: SmallInt; lpSize: LPSIZE): BOOL; external 'GDI';
function GetViewportExtEx(hdc: HDC; lpSize: LPSIZE): BOOL; external 'GDI';

function SetViewportOrgEx(hdc: HDC; nX, nY: SmallInt; lpPoint: LPPOINT): BOOL; external 'GDI';
function GetViewportOrgEx(hdc: HDC; lpPoint: LPPOINT): BOOL; external 'GDI';

function OffsetViewportOrgEx(hdc: HDC; nX, nY: SmallInt; lpPoint: LPPOINT): BOOL; external 'GDI';
function ScaleViewportExtEx(hdc: HDC; nXnum, nXdenom, nYnum, nYdenom: SmallInt; lpSize: LPSIZE): BOOL; external 'GDI';

{ Brush support }
function GetBrushOrgEx(hDC: HDC; lpPoint: LPPOINT): BOOL; external 'GDI';

{ General drawing support }
function MoveToEx(hdc: HDC; x, y: SmallInt; lpPoint: LPPOINT): BOOL; external 'GDI';
function GetCurrentPositionEx(hdc: HDC; lpPoint: LPPOINT): BOOL; external 'GDI';

{ Text support }
function GetTextExtentPoint(hdc: HDC; lpszString: LPCSTR; cbString: SmallInt; lpSize: LPSIZE): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetTextExtentPoint(hdc: HDC; lpszString: LPCSTR; cbString: SmallInt; var Size: SIZE): BOOL; external 'GDI';
{$endif}

{ Font support }
function GetAspectRatioFilterEx(hdc: HDC; lpAspectRatio: LPSIZE): BOOL; external 'GDI';

function GetOutlineTextMetrics(hdc: HDC; cbData: UINT; lpotm: LPOUTLINETEXTMETRIC): WORD; external 'GDI';

function EnumFontFamilies(hdc: HDC; lpszFamily: LPCSTR; fntenmprc: FONTENUMPROC; lParam: LPARAM): SmallInt; external 'GDI';
function EnumFontFamilies(hdc: HDC; lpszFamily: LPCSTR; fntenmprc: TFarProc; lParam: LPARAM): SmallInt; external 'GDI';

function GetFontData(hdc: HDC; dwTable, dwOffset: DWORD; lpvBuffer: FarPointer; cbData: DWORD): DWORD; external 'GDI';
function CreateScalableFontResource(fHidden: UINT; lpszResourceFile, lpszFontFile, lpszCurrentPath: LPCSTR): BOOL; external 'GDI';

function GetGlyphOutline(hdc: HDC; uChar, fuFormat: UINT; lpgm: LPGLYPHMETRICS; cbBuffer: DWORD; lpBuffer: FarPointer; lpmat2: LPMAT2): DWORD; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetGlyphOutline(hdc: HDC; uChar, fuFormat: UINT; var gm: GLYPHMETRICS; cbBuffer: DWORD; lpBuffer: FarPointer; var mat2: MAT2): DWORD; external 'GDI';
{$endif}

function GetCharABCWidths(hdc: HDC; uFirstChar, uLastChar: UINT; lpabc: LPABC): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetCharABCWidths(hdc: HDC; uFirstChar, uLastChar: UINT; var abc: ABC): BOOL; external 'GDI';
{$endif}

function GetKerningPairs(hdc: HDC; cPairs: SmallInt; lpkrnpair: LPKERNINGPAIR): SmallInt; external 'GDI';

function GetRasterizerCaps(lpraststat: LPRASTERIZER_STATUS; cb: SmallInt): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetRasterizerCaps(var raststat: RASTERIZER_STATUS; cb: SmallInt): BOOL; external 'GDI';
{$endif}

{ Bitmap support }
function SetBitmapDimensionEx(hbm: HBITMAP; nX, nY: SmallInt; lpSize: LPSIZE): BOOL; external 'GDI';
function GetBitmapDimensionEx(hBitmap: HBITMAP; lpDimension: LPSIZE): BOOL; external 'GDI';

{ Metafile support }
function SetMetaFileBitsBetter(hmf: HGLOBAL): HMETAFILE; external 'GDI';

{ Printing support }
function StartDoc(hdc: HDC; lpdi: LPDOCINFO): SmallInt; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function StartDoc(hdc: HDC; var di: DOCINFO): SmallInt; external 'GDI';
{$endif}
function StartPage(hdc: HDC): SmallInt; external 'GDI';
function EndPage(hdc: HDC): SmallInt; external 'GDI';
function EndDoc(hdc: HDC): SmallInt; external 'GDI';
function AbortDoc(hdc: HDC): SmallInt; external 'GDI';

function SetAbortProc(hdc: HDC; abrtprc: ABORTPROC): SmallInt; external 'GDI';
function SpoolFile(lpszPrinter, lpszPort, lpszJob, lpszFile: LPSTR): HANDLE; external 'GDI';

{ System Parameters support }
function SystemParametersInfo(uAction, uParam: UINT; lpvParam: FarPointer; fuWinIni: UINT): BOOL; external 'USER';

{ Rectangle support }
function SubtractRect(lprcDest: LPRECT; lprcSource1, lprcSource2: LPRECT): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function SubtractRect(var rcDest: RECT; var rcSource1, rcSource2: RECT): BOOL; external 'USER';
{$endif}

{ Window message support }
function GetMessageExtraInfo: LPARAM; external 'USER';
function GetQueueStatus(flags: UINT): DWORD; external 'USER';

{ Window class management }
{ in Windows 3.1+, RegisterClass returns an ATOM that unquely identifies the 
  class. In Windows 3.0 and earlier, the return value is BOOL. That's why we
  redefine this function in the win31 unit. }
function RegisterClass(lpwc: LPWNDCLASS): ATOM; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function RegisterClass(var wc: WNDCLASS): ATOM; external 'USER';
{$endif}

implementation

end.
