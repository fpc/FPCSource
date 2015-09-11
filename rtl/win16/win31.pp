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

implementation

end.
