unit winprocs;

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
