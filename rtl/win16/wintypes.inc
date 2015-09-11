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
