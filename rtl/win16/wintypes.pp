unit wintypes;

interface

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
  LPSTR = ^Char; far;
  LPCSTR = ^Char; far;
  
  { PBYTE is already defined in system }
  LPBYTE = ^Byte; far;
  
  PINT = ^SmallInt;
  LPINT = ^SmallInt; far;
  
  { PWORD is already defined in system }
  LPWORD = ^Word; far;
  
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
  
  HGLOBAL = THandle;
  HLOCAL = THandle;
  
  TGlobalHandle = THandle;
  TLocalHandle = THandle;
  
  ATOM = UINT;
  TAtom = ATOM;
  
  HINST = THandle; { instead of HINSTANCE, to avoid conflict with var hInstance }
  HMODULE = HINST;
  
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

implementation

end.
