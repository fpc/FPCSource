unit winprocs;

{$if defined(FPC_MM_COMPACT) or defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}
  {$define VAR_PARAMS_ARE_FAR}
{$endif}

interface

uses
  wintypes;

function LOBYTE(w: Word): Byte; inline;
function HIBYTE(w: Word): Byte; inline;

function LOWORD(l: LongInt): Word; inline;
function HIWORD(l: LongInt): Word; inline;

function MAKELONG(low, high: Word): LONG; inline;

function MAKELPARAM(low, high: Word): LPARAM; inline;
function MAKELRESULT(low, high: Word): LRESULT; inline;

function MAKELP(sel, off: Word): FarPointer; inline;
function SELECTOROF(lp: FarPointer): Word; inline;
function OFFSETOF(lp: FarPointer): Word; inline;

// FIELDOFFSET

{ System Information }
function GetVersion: DWORD; external 'KERNEL';

function GetFreeSpace(Flag: UINT): DWORD; external 'KERNEL';
function GetCurrentPDB: UINT; external 'KERNEL';

function GetWindowsDirectory(Buffer: LPSTR; Size: UINT): UINT; external 'KERNEL';
function GetSystemDirectory(Buffer: LPSTR; Size: UINT): UINT; external 'KERNEL';

function GetWinFlags: DWORD; external 'KERNEL';

function GetDOSEnvironment: LPSTR; external 'KERNEL';

function GetCurrentTime: DWORD; external 'USER';
function GetTickCount: DWORD; external 'USER';
function GetTimerResolution: DWORD; external 'USER';

{ Error handling }
procedure FatalExit(Code: SmallInt); external 'KERNEL';
procedure FatalAppExit(Action: UINT; MessageText: LPCSTR); external 'KERNEL';

function ExitWindows(dwReturnCode: DWORD; wReserved: UINT): BOOL; external 'USER';

procedure DebugBreak; external 'KERNEL';
procedure OutputDebugString(OutputString: LPCSTR); external 'KERNEL';

function SetErrorMode(Mode: UINT): UINT; external 'KERNEL';

{ Catch/Throw and stack management }

function Catch(CatchBuf: LPCATCHBUF): SmallInt; external 'KERNEL';
procedure Throw(CatchBuf: LPCATCHBUF; ThrowBack: SmallInt); external 'KERNEL';
{$ifdef VAR_PARAMS_ARE_FAR}
function Catch(var CatchBuf: TCatchBuf): SmallInt; external 'KERNEL';
procedure Throw(var CatchBuf: TCatchBuf; ThrowBack: SmallInt); external 'KERNEL';
{$endif}

procedure SwitchStackBack; external 'KERNEL';
procedure SwitchStackTo(StackSegment, StackPointer, StackTop: UINT); external 'KERNEL';

{ Module Management }

function LoadModule(ModuleName: LPCSTR; ParameterName: LPVOID): HINST; external 'KERNEL';
function FreeModule(Module: HINST): BOOL; external 'KERNEL';

function LoadLibrary(LibFileName: LPCSTR): HINST; external 'KERNEL';
procedure FreeLibrary(LibModule: HINST); external 'KERNEL';

function WinExec(CmdLine: LPCSTR; CmdShow: UINT): UINT; external 'KERNEL';

function GetModuleHandle(ModuleName: LPCSTR): HMODULE; external 'KERNEL';

function GetModuleUsage(Module: HINST): SmallInt; external 'KERNEL';
function GetModuleFileName(Module: HINST; FileName: LPSTR; Size: SmallInt): SmallInt; external 'KERNEL';

function GetProcAddress(Module: HINST; ProcName: LPCSTR): FARPROC; external 'KERNEL';

function GetInstanceData(Instance: HINST; Data: PBYTE; Count: SmallInt): SmallInt; external 'KERNEL';

function GetCodeHandle(Proc: FARPROC): HGLOBAL; external 'KERNEL';

procedure GetCodeInfo(lpProc: FARPROC; lpSegInfo: LPSEGINFO); external 'KERNEL';

function MakeProcInstance(Proc: FARPROC; Instance: HINST): FARPROC; external 'KERNEL';
procedure FreeProcInstance(Proc: FARPROC); external 'KERNEL';

{#ifdef _LAX
#define MakeProcInstance(__F, __H) MakeProcInstance((FARPROC)__F, __H)
#define FreeProcInstance(__F)      FreeProcInstance((FARPROC)__F)
#endif /* _LAX */}

function SetSwapAreaSize(Size: UINT): LONG; external 'KERNEL';
procedure SwapRecording(Flag: UINT); external 'KERNEL';
procedure ValidateCodeSegments; external 'KERNEL';

implementation

function LOBYTE(w: Word): Byte;
begin
  LOBYTE := Byte(w);
end;

function HIBYTE(w: Word): Byte;
begin
  HIBYTE := Byte(w shr 8);
end;

function LOWORD(l: LongInt): Word;
begin
  LOWORD := Word(l);
end;

function HIWORD(l: LongInt): Word;
begin
  HIWORD := Word(l shr 16);
end;

function MAKELONG(low, high: Word): LONG;
begin
  MAKELONG := low or (LongInt(high) shl 16);
end;

function MAKELPARAM(low, high: Word): LPARAM;
begin
  MAKELPARAM := MAKELONG(low, high);
end;

function MAKELRESULT(low, high: Word): LRESULT;
begin
  MAKELRESULT := MAKELONG(low, high);
end;

function MAKELP(sel, off: Word): FarPointer;
begin
  MAKELP := Ptr(sel, off);
end;

function SELECTOROF(lp: FarPointer): Word;
begin
  SELECTOROF:=HIWORD(LongInt(lp));
end;

function OFFSETOF(lp: FarPointer): Word;
begin
  OFFSETOF:=LOWORD(LongInt(lp));
end;

end.
