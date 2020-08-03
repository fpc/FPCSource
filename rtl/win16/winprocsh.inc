
function LOBYTE(w: Word): Byte; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
function HIBYTE(w: Word): Byte; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function LOWORD(l: LongInt): Word; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
function HIWORD(l: LongInt): Word; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function MAKELONG(low, high: Word): LONG; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function MAKELPARAM(low, high: Word): LPARAM; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
function MAKELRESULT(low, high: Word): LRESULT; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function MAKELP(sel, off: Word): FarPointer; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
function SELECTOROF(lp: FarPointer): Word; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
function OFFSETOF(lp: FarPointer): Word; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

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

function WinLoadLibrary(LibFileName: LPCSTR): HINST; external 'KERNEL' name 'LoadLibrary';
procedure WinFreeLibrary(LibModule: HINST); external 'KERNEL' name 'FreeLibrary';

function WinExec(CmdLine: LPCSTR; CmdShow: UINT): UINT; external 'KERNEL';

function GetModuleHandle(ModuleName: LPCSTR): HMODULE; external 'KERNEL';

function GetModuleUsage(Module: HINST): SmallInt; external 'KERNEL';
function GetModuleFileName(Module: HINST; FileName: LPSTR; Size: SmallInt): SmallInt; external 'KERNEL';

function WinGetProcAddress(Module: HINST; ProcName: LPCSTR): FARPROC; external 'KERNEL' name 'GetProcAddress';

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

{ Task Management }

function GetNumTasks: UINT; external 'KERNEL';
function GetCurrentTask: HTASK; external 'KERNEL';

procedure Yield; external 'KERNEL';
procedure DirectedYield(Task: HTASK); external 'KERNEL';

{ Global memory management }

function GlobalDiscard(h: HGLOBAL): HGLOBAL; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function GlobalAlloc(Flags: UINT; Bytes: DWORD): HGLOBAL; external 'KERNEL';
function GlobalReAlloc(Mem: HGLOBAL; Bytes: DWORD; Flags: UINT): HGLOBAL; external 'KERNEL';
function GlobalFree(Mem: HGLOBAL): HGLOBAL; external 'KERNEL';

function GlobalDosAlloc(Bytes: DWORD): DWORD; external 'KERNEL';
function GlobalDosFree(Selector: UINT): UINT; external 'KERNEL';

function GlobalLock(Mem: HGLOBAL): FarPointer; external 'KERNEL';
function GlobalUnlock(Mem: HGLOBAL): BOOL; external 'KERNEL';

function GlobalSize(Mem: HGLOBAL): DWORD; external 'KERNEL';
function GlobalHandle(Mem: UINT): DWORD; external 'KERNEL';

function GlobalFlags(Mem: HGLOBAL): UINT; external 'KERNEL';

function GlobalWire(Mem: HGLOBAL): FarPointer; external 'KERNEL';
function GlobalUnWire(Mem: HGLOBAL): BOOL; external 'KERNEL';

function GlobalPageLock(Selector: HGLOBAL): UINT; external 'KERNEL';
function GlobalPageUnlock(Selector: HGLOBAL): UINT; external 'KERNEL';

procedure GlobalFix(Mem: HGLOBAL); external 'KERNEL';
procedure GlobalUnfix(Mem: HGLOBAL); external 'KERNEL';

function GlobalLRUNewest(Mem: HGLOBAL): HGLOBAL; external 'KERNEL';
function GlobalLRUOldest(Mem: HGLOBAL): HGLOBAL; external 'KERNEL';

function GlobalCompact(MinFree: DWORD): DWORD; external 'KERNEL';

procedure GlobalNotify(NotifyProc: GNOTIFYPROC); external 'KERNEL';

function LockSegment(Segment: UINT): HGLOBAL; external 'KERNEL';
procedure UnlockSegment(Segment: UINT); external 'KERNEL';

function LockData(dummy: SmallInt): HGLOBAL; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
procedure UnlockData(dummy: SmallInt); {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function AllocSelector(Selector: UINT): UINT; external 'KERNEL';
function FreeSelector(Selector: UINT): UINT; external 'KERNEL';
function AllocDStoCSAlias(Selector: UINT): UINT; external 'KERNEL';
function PrestoChangoSelector(sourceSel, destSel: UINT): UINT; external 'KERNEL';
function GetSelectorBase(Selector: UINT): DWORD; external 'KERNEL';
function SetSelectorBase(Selector: UINT; Base: DWORD): UINT; external 'KERNEL';
function GetSelectorLimit(Selector: UINT): DWORD; external 'KERNEL';
function SetSelectorLimit(Selector: UINT; Base: DWORD): UINT; external 'KERNEL';

procedure LimitEmsPages(Kbytes: DWORD); external 'KERNEL';

procedure ValidateFreeSpaces; external 'KERNEL';

{ Local Memory Management }

function LocalDiscard(h: HLOCAL): HLOCAL; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function LocalAlloc(Flags, Bytes: UINT): HLOCAL; external 'KERNEL';
function LocalReAlloc(Mem: HLOCAL; Bytes, Flags: UINT): HLOCAL; external 'KERNEL';
function LocalFree(Mem: HLOCAL): HLOCAL; external 'KERNEL';

function LocalLock(Mem: HLOCAL): NearPointer; external 'KERNEL';
function LocalUnlock(Mem: HLOCAL): BOOL; external 'KERNEL';

function LocalSize(Mem: HLOCAL): UINT; external 'KERNEL';
function LocalHandle(Mem: NearPointer): HLOCAL; external 'KERNEL';

function LocalFlags(Mem: HLOCAL): UINT; external 'KERNEL';

function LocalInit(Segment, Start, EndPos: UINT): BOOL; external 'KERNEL';
function LocalCompact(MinFree: UINT): UINT; external 'KERNEL';
function LocalShrink(Seg: HLOCAL; Size: UINT): UINT; external 'KERNEL';

{ File I/O }

function OpenFile(FileName: LPCSTR; ReOpenBuff: LPOFSTRUCT; Style: UINT): HFILE; external 'KERNEL';
{$ifdef VAR_PARAMS_ARE_FAR}
function OpenFile(FileName: LPCSTR; var ReOpenBuff: TOFStruct; Style: UINT): HFILE; external 'KERNEL';
{$endif}

function _lopen(PathName: LPCSTR; ReadWrite: SmallInt): HFILE; external 'KERNEL';
function _lcreat(PathName: LPCSTR; Attribute: SmallInt): HFILE; external 'KERNEL';

function _lclose(FileHandle: HFILE): HFILE; external 'KERNEL';

function _llseek(FileHandle: HFILE; Offset: LONG; Origin: SmallInt): LONG; external 'KERNEL';

function _lread(FileHandle: HFILE; Buffer: HugePointer; Bytes: UINT): UINT; external 'KERNEL';
function _lread(FileHandle: HFILE; Buffer: FarPointer; Bytes: UINT): UINT; external 'KERNEL';
function _lwrite(FileHandle: HFILE; Buffer: HugePointer; Bytes: UINT): UINT; external 'KERNEL';
function _lwrite(FileHandle: HFILE; Buffer: FarPointer; Bytes: UINT): UINT; external 'KERNEL';

function GetTempFileName(DriveLetter: BYTE; PrefixString: LPCSTR; Unique: UINT; TempFileName: LPSTR): SmallInt; external 'KERNEL';
function GetTempFileName(DriveLetter: Char; PrefixString: LPCSTR; Unique: UINT; TempFileName: LPSTR): SmallInt; external 'KERNEL';
function GetTempDrive(DriveLetter: Char): BYTE; external 'KERNEL';

function GetDriveType(Drive: SmallInt): UINT; external 'KERNEL';

function SetHandleCount(Number: UINT): UINT; external 'KERNEL';

{ Network support }
function WNetAddConnection(lpszNetPath, lpszPassword, lpszLocalName: LPSTR): UINT; external 'USER';
function WNetGetConnection(lpszLocalName, lpszRemoteName: LPSTR; cbBufferSize: LPUINT): UINT; external 'USER';
function WNetCancelConnection(lpszName: LPSTR; tForce: BOOL): UINT; external 'USER';

{ Resource Management }

function FindResource(hInstance: HINST; lpName, lpType: LPCSTR): HRSRC; external 'KERNEL';
function LoadResource(hInstance: HINST; hResInfo: HRSRC): HGLOBAL; external 'KERNEL';
function FreeResource(hResData: HGLOBAL): BOOL; external 'KERNEL';

function LockResource(hResData: HGLOBAL): FarPointer; external 'KERNEL';
function UnlockResource(hResData: HGLOBAL): BOOL; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function SizeofResource(hInstance: HINST; hResInfo: HRSRC): DWORD; external 'KERNEL';

function AccessResource(hInstance: HINST; hResInfo: HRSRC): SmallInt; external 'KERNEL';

function AllocResource(hInstance: HINST; hResInfo: HRSRC; dwSize: DWORD): HGLOBAL; external 'KERNEL';

function SetResourceHandler(hInstance: HINST; lpType: LPCSTR; lpLoadFunc: RSRCHDLRPROC): RSRCHDLRPROC; external 'KERNEL';

{ Atom Management }

function InitAtomTable(cTableEntries: SmallInt): BOOL; external 'KERNEL';
function AddAtom(lpszName: LPCSTR): ATOM; external 'KERNEL';
function DeleteAtom(atm: ATOM): ATOM; external 'KERNEL';
function FindAtom(lpszString: LPCSTR): ATOM; external 'KERNEL';
function GetAtomName(atm: ATOM; lpszBuffer: LPSTR; cbBuffer: SmallInt): UINT; external 'KERNEL';
function GlobalAddAtom(lpszString: LPCSTR): ATOM; external 'USER';
function GlobalDeleteAtom(atm: ATOM): ATOM; external 'USER';
function GlobalFindAtom(lpszString: LPCSTR): ATOM; external 'USER';
function GlobalGetAtomName(atom: ATOM; lpszBuffer: LPSTR; cbBuffer: SmallInt): UINT; external 'USER';
function GetAtomHandle(atm: ATOM): HLOCAL; external 'KERNEL';

{ WIN.INI Support }

{ User Profile Routines }
  function GetProfileInt(lpszSection: LPCSTR; lpszEntry: LPCSTR; default: SmallInt): UINT; external 'KERNEL';
  function GetProfileString(lpszSection, lpszEntry, lpszDefault: LPCSTR; lpszReturnBuffer: LPSTR; cbReturnBuffer: SmallInt): SmallInt; external 'KERNEL';
  function WriteProfileString(lpszSection, lpszEntry, lpszString: LPCSTR): BOOL; external 'KERNEL';

  function GetPrivateProfileInt(lpszSection, lpszEntry: LPCSTR; default: SmallInt; lpszFilename: LPCSTR): UINT; external 'KERNEL';
  function GetPrivateProfileString(lpszSection, lpszEntry, lpszDefault: LPCSTR; lpszReturnBuffer: LPSTR; cbReturnBuffer: SmallInt; lpszFilename: LPCSTR): SmallInt; external 'KERNEL';
  function WritePrivateProfileString(lpszSection, lpszEntry, lpszString, lpszFilename: LPCSTR): BOOL; external 'KERNEL';

{ International & Char Translation Support }

procedure AnsiToOem(hpszWindows, hpszOem: PHugeChar); external 'KEYBOARD';
procedure AnsiToOem(hpszWindows, hpszOem: PFarChar); external 'KEYBOARD';
procedure OemToAnsi(hpszOemStr, hpszWindowsStr: PHugeChar); external 'KEYBOARD';
procedure OemToAnsi(hpszOemStr, hpszWindowsStr: PFarChar); external 'KEYBOARD';

procedure AnsiToOemBuff(lpszWindowsStr: LPCSTR; lpszOemStr: LPSTR; cbWindowsStr: UINT); external 'KEYBOARD';
procedure OemToAnsiBuff(lpszOemStr: LPCSTR; lpszWindowsStr: LPSTR; cbOemStr: UINT); external 'KEYBOARD';

function AnsiNext(lpchCurrentChar: LPCSTR): LPSTR; external 'USER';
function AnsiPrev(lpchStart, lpchCurrentChar: LPCSTR): LPSTR; external 'USER';

function AnsiUpper(lpszString: LPSTR): LPSTR; external 'USER';
function AnsiLower(lpsz: LPSTR): LPSTR; external 'USER';

function AnsiUpperBuff(lpszString: LPSTR; cbString: UINT): UINT; external 'USER';
function AnsiLowerBuff(lpszString: LPSTR; cbString: UINT): UINT; external 'USER';


function IsCharAlpha(chTest: char): BOOL; external 'USER';
function IsCharAlphaNumeric(chTest: char): BOOL; external 'USER';
function IsCharUpper(chTest: char): BOOL; external 'USER';
function IsCharLower(chTest: char): BOOL; external 'USER';

function lstrcmp(lpszString1, lpszString2: LPCSTR): SmallInt; external 'USER';
function lstrcmpi(lpszString1, lpszString2: LPCSTR): SmallInt; external 'USER';
function lstrcpy(lpszString1: LPSTR; lpszString2: LPCSTR): LPSTR; external 'KERNEL';
function lstrcat(lpszString1: LPSTR; lpszString2: LPCSTR): LPSTR; external 'KERNEL';
function lstrlen(lpszString: LPCSTR): SmallInt; external 'KERNEL';

function LoadString(hints: HINST; idResrouce: UINT; lpszBuffer: LPSTR; cbBuffer: SmallInt): SmallInt; external 'USER';

{ Keyboard Driver Functions }

function OemKeyScan(uOemChar: UINT): DWORD; external 'KEYBOARD';
function VkKeyScan(uChar: UINT): UINT; external 'KEYBOARD';
function GetKeyboardType(fnKeybInfo: SmallInt): SmallInt; external 'KEYBOARD';
function MapVirtualKey(uKeyCode, fuMapType: UINT): UINT; external 'KEYBOARD';
function GetKBCodePage: SmallInt; external 'KEYBOARD';
function GetKeyNameText(lParam: LONG; lpszBuffer: LPSTR; cbMaxKey: SmallInt): SmallInt; external 'KEYBOARD';
function ToAscii(wVirtKey, wScanCode: UINT; lpKeyState: LPBYTE; lpChar: LPDWORD; wFlags: UINT): SmallInt; external 'KEYBOARD';

{ DC Management }

function CreateDC(lpszDriver, lpszDevice, lpszOutput: LPCSTR; lpvInitData: FarPointer): HDC; external 'GDI';
function CreateIC(lpszDriver, lpszDevice, lpszOutput: LPCSTR; lpvInitData: FarPointer): HDC; external 'GDI';
function CreateCompatibleDC(hdc: HDC): HDC; external 'GDI';

function DeleteDC(hdc: HDC): BOOL; external 'GDI';

function GetDCOrg(hdc: HDC): DWORD; external 'GDI';

function SaveDC(hdc: HDC): SmallInt; external 'GDI';
function RestoreDC(hdc: HDC; nSavedDC: SmallInt): BOOL; external 'GDI';

function SetEnvironment(lpPortName: LPCSTR; lpdev: FarPointer; nCount: UINT): SmallInt; external 'GDI';
function GetEnvironment(lpPortName: LPCSTR; lpdev: FarPointer; nMaxSize: UINT): SmallInt; external 'GDI';

function MulDiv(nMultiplicand, nMultiplier, nDivisor: SmallInt): SmallInt; external 'GDI';

{ Device Capabilities }

function GetDeviceCaps(hdc: HDC; iCapability: SmallInt): SmallInt; external 'GDI';

{ Coordinate transformation support }

function SetMapMode(hdc: HDC; fnMapMode: SmallInt): SmallInt; external 'GDI';
function GetMapMode(hdc: HDC): SmallInt; external 'GDI';

function SetWindowOrg(hdc: HDC; nXOrigin, nYOrigin: SmallInt): DWORD; external 'GDI';
function GetWindowOrg(hdc: HDC): DWORD; external 'GDI';

function SetWindowExt(hdc: HDC; nXExtent, nYExtent: SmallInt): DWORD; external 'GDI';
function GetWindowExt(hdc: HDC): DWORD; external 'GDI';

function OffsetWindowOrg(hdc: HDC; nXOffset, nYOffset: SmallInt): DWORD; external 'GDI';
function ScaleWindowExt(hdc: HDC; nXNum, nXDenom, nYNum, nYDenom: SmallInt): DWORD; external 'GDI';

function SetViewportOrg(hdc: HDC; nXOrigin, nYOrigin: SmallInt): DWORD; external 'GDI';
function GetViewportOrg(hdc: HDC): DWORD; external 'GDI';

function SetViewportExt(hdc: HDC; nXExtent, nYExtent: SmallInt): DWORD; external 'GDI';
function GetViewportExt(hdc: HDC): DWORD; external 'GDI';

function OffsetViewportOrg(hdc: HDC; nXOffset, nYOffset: SmallInt): DWORD; external 'GDI';
function ScaleViewportExt(hdc: HDC; nXNum, nXDenom, nYNum, nYDenom: SmallInt): DWORD; external 'GDI';

function DPtoLP(hdc: HDC; lppt: LPPOINT; cPoints: SmallInt): BOOL; external 'GDI';
function LPtoDP(hdc: HDC; lppt: LPPOINT; cPoints: SmallInt): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function DPtoLP(hdc: HDC; var pt; cPoints: SmallInt): BOOL; external 'GDI';
function LPtoDP(hdc: HDC; var pt; cPoints: SmallInt): BOOL; external 'GDI';
{$endif}

{ Color support }

function RGB(cRed, cGreen, cBlue: BYTE): COLORREF; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function GetRValue(rgb: DWORD): BYTE; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
function GetGValue(rgb: DWORD): BYTE; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
function GetBValue(rgb: DWORD): BYTE; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function GetNearestColor(hdc: HDC; clrref: COLORREF): COLORREF; external 'GDI';

function GetSysColor(nDspElement: SmallInt): COLORREF; external 'USER';
procedure SetSysColors(cDspElements: SmallInt; lpnDspElements: LPINT; lpdwRgbValues: LPCOLORREF); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure SetSysColors(cDspElements: SmallInt; var DspElements; var RgbValues); external 'USER';
{$endif}

{ GDI Object Support }

function GetStockObject(fnObject: SmallInt): HGDIOBJ; external 'GDI';

function IsGDIObject(hgdiobj: HGDIOBJ): BOOL; external 'GDI';

function DeleteObject(hgdiobj: HGDIOBJ): BOOL; external 'GDI';
function SelectObject(hdc: HDC; hgdiobj: HGDIOBJ): HGDIOBJ; external 'GDI';
function GetObject(hgdiobj: HGDIOBJ; cbBuffer: SmallInt; lpvObject: FarPointer): SmallInt; external 'GDI';
function UnrealizeObject(hgdiobj: HGDIOBJ): BOOL; external 'GDI';

function EnumObjects(hdc: HDC; fnObjectType: SmallInt; goenmprc: GOBJENUMPROC; lParam: LPARAM): SmallInt; external 'GDI';
function EnumObjects(hdc: HDC; fnObjectType: SmallInt; goenmprc: FARPROC; lParam: LPARAM): SmallInt; external 'GDI';

{ Pen support }

function CreatePen(fnPenStyle, nWidth: SmallInt; clrref: COLORREF): HPEN; external 'GDI';
function CreatePenIndirect(lplgpn: LPLOGPEN): HPEN; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreatePenIndirect(var lgpn: LOGPEN): HPEN; external 'GDI';
{$endif}

{ Brush support }

function CreateSolidBrush(clrref: COLORREF): HBRUSH; external 'GDI';
function CreateHatchBrush(fnStyle: SmallInt; clrref: COLORREF): HBRUSH; external 'GDI';
function CreatePatternBrush(hbmp: HBITMAP): HBRUSH; external 'GDI';
function CreateDIBPatternBrush(hglbDIBPacked: HGLOBAL; fnColorSpec: UINT): HBRUSH; external 'GDI';
function CreateBrushIndirect(lplb: LPLOGBRUSH): HBRUSH; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreateBrushIndirect(var lb: LOGBRUSH): HBRUSH; external 'GDI';
{$endif}

function SetBrushOrg(hdc: HDC; nXOrg, nYOrg: SmallInt): DWORD; external 'GDI';
function GetBrushOrg(hdc: HDC): DWORD; external 'GDI';

{ Region support }

function CreateRectRgn(nLeftRect, nTopRect, nRightRect, nBottomRect: SmallInt): HRGN; external 'GDI';
function CreateRectRgnIndirect(lprc: LPRECT): HRGN; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreateRectRgnIndirect(var rc: RECT): HRGN; external 'GDI';
{$endif}
function CreateEllipticRgnIndirect(lprc: LPRECT): HRGN; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreateEllipticRgnIndirect(var rc: RECT): HRGN; external 'GDI';
{$endif}
function CreateEllipticRgn(nLeftRect, nTopRect, nRightRect, nBottomRect: SmallInt): HRGN; external 'GDI';
function CreatePolygonRgn(lppt: LPPOINT; cPoints, fnPolyFillMode: SmallInt): HRGN; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreatePolygonRgn(var pt; cPoints, fnPolyFillMode: SmallInt): HRGN; external 'GDI';
{$endif}
function CreatePolyPolygonRgn(lppt: LPPOINT; lpnPolyCount: LPINT; cIntegers, fnPolyFillMode: SmallInt): HRGN; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreatePolyPolygonRgn(var pt; var nPolyCount; cIntegers, fnPolyFillMode: SmallInt): HRGN; external 'GDI';
{$endif}
function CreateRoundRectRgn(nLeftRect, nTopRect, nRightRect, nBottomRect, nWidthEllipse, nHeightEllipse: SmallInt): HRGN; external 'GDI';

procedure SetRectRgn(hrgn: HRGN; nLeftRect, nTopRect, nRightRect, nBottomRect: SmallInt); external 'GDI';

function CombineRgn(hrgnDest, hrgnSrc1, hrgnSrc2: HRGN; fCombineMode: SmallInt): SmallInt; external 'GDI';

function EqualRgn(hrgnSrc1, hrgnSrc2: HRGN): BOOL; external 'GDI';
function OffsetRgn(hrgn: HRGN; nXOffset, nYOffset: SmallInt): SmallInt; external 'GDI';

function GetRgnBox(hrgn: HRGN; lprc: LPRECT): SmallInt; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetRgnBox(hrgn: HRGN; var rc: RECT): SmallInt; external 'GDI';
{$endif}

function RectInRegion(hrgn: HRGN; lprc: LPRECT): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function RectInRegion(hrgn: HRGN; var rc: RECT): BOOL; external 'GDI';
{$endif}
function PtInRegion(hrgn: HRGN; nXPos, nYPos: SmallInt): BOOL; external 'GDI';

{ Color palette Support }

function PALETTERGB(cRed, cGreen, cBlue: BYTE): COLORREF; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}
function PALETTEINDEX(wPaletteIndex: WORD): COLORREF; {$ifdef SYSTEMUNIT}forward;{$else}inline;{$endif}

function CreatePalette(lplgpl: LPLOGPALETTE): HPALETTE; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreatePalette(var lgpl: LOGPALETTE): HPALETTE; external 'GDI';
{$endif}

function SelectPalette(hdc: HDC; hpal: HPALETTE; fPalBack: BOOL): HPALETTE; external 'USER';

function RealizePalette(hdc: HDC): UINT; external 'USER';

function UpdateColors(hdc: HDC): SmallInt; external 'GDI';
procedure AnimatePalette(hpal: HPALETTE; iStart, cEntries: UINT; lppe: LPPALETTEENTRY); external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure AnimatePalette(hpal: HPALETTE; iStart, cEntries: UINT; var pe); external 'GDI';
{$endif}

function SetPaletteEntries(hpal: HPALETTE; iStart, cEntries: UINT; lppe: LPPALETTEENTRY): UINT; external 'GDI';
function GetPaletteEntries(hpal: HPALETTE; iStart, cEntries: UINT; lppe: LPPALETTEENTRY): UINT; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function SetPaletteEntries(hpal: HPALETTE; iStart, cEntries: UINT; var pe): UINT; external 'GDI';
function GetPaletteEntries(hpal: HPALETTE; iStart, cEntries: UINT; var pe): UINT; external 'GDI';
{$endif}

function GetNearestPaletteIndex(hpal: HPALETTE; clrref: COLORREF): UINT; external 'GDI';

function ResizePalette(hpal: HPALETTE; cEntries: UINT): BOOL; external 'GDI';

function GetSystemPaletteEntries(hdc: HDC; iStart, cEntries: UINT; lppe: LPPALETTEENTRY): UINT; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetSystemPaletteEntries(hdc: HDC; iStart, cEntries: UINT; var pe): UINT; external 'GDI';
{$endif}

function GetSystemPaletteUse(hdc: HDC): UINT; external 'GDI';
function SetSystemPaletteUse(hdc: HDC; fuStatic: UINT): UINT; external 'GDI';

{ Clipping support }

function SelectClipRgn(hdc: HDC; hrgn: HRGN): SmallInt; external 'GDI';
function GetClipBox(hdc: HDC; lprc: LPRECT): SmallInt; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetClipBox(hdc: HDC; var rc: RECT): SmallInt; external 'GDI';
{$endif}

function IntersectClipRect(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect: SmallInt): SmallInt; external 'GDI';
function OffsetClipRgn(hdc: HDC; nXOffset, nYOffset: SmallInt): SmallInt; external 'GDI';
function ExcludeClipRect(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect: SmallInt): SmallInt; external 'GDI';

function PtVisible(hdc: HDC; nXPos, nYPos: SmallInt): BOOL; external 'GDI';
function RectVisible(hdc: HDC; lprc: LPRECT): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function RectVisible(hdc: HDC; var rc: RECT): BOOL; external 'GDI';
{$endif}

{ General drawing support }

function MoveTo(hdc: HDC; x, y: SmallInt): DWORD; external 'GDI';
function GetCurrentPosition(hdc: HDC): DWORD; external 'GDI';

function LineTo(hdc: HDC; xEnd, yEnd: SmallInt): BOOL; external 'GDI';
function Polyline(hdc: HDC; lppt: LPPOINT; cPoints: SmallInt): BOOL; external 'GDI';

procedure LineDDA(nXStart, nYStart, nXEnd, nYEnd: SmallInt; lnddaprc: LINEDDAPROC; lParam: LPARAM); external 'GDI';
procedure LineDDA(nXStart, nYStart, nXEnd, nYEnd: SmallInt; lnddaprc: TFarProc; lParam: FarPointer); external 'GDI';

function Rectangle(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect: SmallInt): BOOL; external 'GDI';
function RoundRect(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nEllipseWidth, nEllipseHeight: SmallInt): BOOL; external 'GDI';

function Ellipse(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect: SmallInt): BOOL; external 'GDI';
function Arc(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nXStartArc, nYStartArc, nXEndArc, nYEndArc: SmallInt): BOOL; external 'GDI';
function Chord(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nXStartLine, nYStartLine, nXEndLine, nYEndLine: SmallInt): BOOL; external 'GDI';
function Pie(hdc: HDC; nLeftRect, nTopRect, nRightRect, nBottomRect, nxStartArc, nyStartArc, nxEndArc, nyEndArc: SmallInt): BOOL; external 'GDI';

function Polygon(hdc: HDC; lppt: LPPOINT; cPoints: SmallInt): BOOL; external 'GDI';
function PolyPolygon(hdc: HDC; lppt: LPPOINT; lpnPolyCounts: LPINT; cPolygons: SmallInt): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function Polygon(hdc: HDC; var pt; cPoints: SmallInt): BOOL; external 'GDI';
function PolyPolygon(hdc: HDC; var pt; var nPolyCounts; cPolygons: SmallInt): BOOL; external 'GDI';
{$endif}

function SetPolyFillMode(hdc: HDC; fnMode: SmallInt): SmallInt; external 'GDI';
function GetPolyFillMode(hdc: HDC): SmallInt; external 'GDI';

function FloodFill(hdc: HDC; nXStart, nYStart: SmallInt; clrref: COLORREF): BOOL; external 'GDI';
function ExtFloodFill(hdc: HDC; nXStart, nYStart: SmallInt; clrref: COLORREF; fuFillType: UINT): BOOL; external 'GDI';

function FillRgn(hdc: HDC; hrgn: HRGN; hbr: HBRUSH): BOOL; external 'GDI';
function FrameRgn(hdc: HDC; hrgn: HRGN; hbr: HBRUSH; nWidth, nHeight: SmallInt): BOOL; external 'GDI';
function InvertRgn(hdc: HDC; hrgn: HRGN): BOOL; external 'GDI';
function PaintRgn(hdc: HDC; hrgn: HRGN): BOOL; external 'GDI';

{ Rectangle output routines }
function FillRect(hdc: HDC; lprc: LPRECT; hbr: HBRUSH): SmallInt; external 'USER';
function FrameRect(hdc: HDC; lprc: LPRECT; hbr: HBRUSH): SmallInt; external 'USER';
procedure InvertRect(hdc: HDC; lprc: LPRECT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function FillRect(hdc: HDC; var rc: RECT; hbr: HBRUSH): SmallInt; external 'USER';
function FrameRect(hdc: HDC; var rc: RECT; hbr: HBRUSH): SmallInt; external 'USER';
procedure InvertRect(hdc: HDC; var rc: RECT); external 'USER';
{$endif}

procedure DrawFocusRect(hdc: HDC; lprc: LPRECT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure DrawFocusRect(hdc: HDC; var rc: RECT); external 'USER';
{$endif}

{ Text support }

function TextOut(hdc: HDC; nXStart, nYStart: SmallInt; lpszString: LPCSTR; cbString: SmallInt): BOOL; external 'GDI';
function TabbedTextOut(hdc: HDC; xPosStart, yPosStart: SmallInt; lpszString: LPCSTR; cbString, cTabStops: SmallInt; lpnTabPositions: LPINT; nTabOrigin: SmallInt): LONG; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function TabbedTextOut(hdc: HDC; xPosStart, yPosStart: SmallInt; lpszString: LPCSTR; cbString, cTabStops: SmallInt; var nTabPositions; nTabOrigin: SmallInt): LONG; external 'USER';
{$endif}
function ExtTextOut(hdc: HDC; nXStart, nYStart: SmallInt; fuOptions: UINT; lprc: LPRECT; lpszString: LPCSTR; cbString: UINT; lpDx: LPINT): BOOL; external 'GDI';

function GetTextExtent(hdc: HDC; lpszString: LPCSTR; cbString: SmallInt): DWORD; external 'GDI';
function GetTabbedTextExtent(hdc: HDC; lpszString: LPCSTR; cChars, cTabs: SmallInt; lpnTabs: LPINT): DWORD; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetTabbedTextExtent(hdc: HDC; lpszString: LPCSTR; cChars, cTabs: SmallInt; var nTabs): DWORD; external 'USER';
{$endif}

function DrawText(hdc: HDC; lpsz: LPCSTR; cb: SmallInt; lprc: LPRECT; fuFormat: UINT): SmallInt; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function DrawText(hdc: HDC; lpsz: LPCSTR; cb: SmallInt; var rc: RECT; fuFormat: UINT): SmallInt; external 'USER';
{$endif}

function GrayString(hdc: HDC; hbr: HBRUSH; gsprc: GRAYSTRINGPROC; lParam: LPARAM; cch, x, y, cx, cy: SmallInt): BOOL; external 'USER';
function GrayString(hdc: HDC; hbr: HBRUSH; gsprc: TFarProc; lParam: LPARAM; cch, x, y, cx, cy: SmallInt): BOOL; external 'USER';

function GetCharWidth(hdc: HDC; uFirstChar, uLastChar: UINT; lpnWidths: LPINT): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetCharWidth(hdc: HDC; uFirstChar, uLastChar: UINT; var nWidths): BOOL; external 'GDI';
{$endif}

function SetTextColor(hdc: HDC; clrref: COLORREF): COLORREF; external 'GDI';
function GetTextColor(hdc: HDC): COLORREF; external 'GDI';

function SetBkColor(hdc: HDC; clrref: COLORREF): COLORREF; external 'GDI';
function GetBkColor(hdc: HDC): COLORREF; external 'GDI';

function SetBkMode(hdc: HDC; fnBkMode: SmallInt): SmallInt; external 'GDI';
function GetBkMode(hdc: HDC): SmallInt; external 'GDI';

function SetTextAlign(hdc: HDC; fuAlign: UINT): UINT; external 'GDI';
function GetTextAlign(hdc: HDC): UINT; external 'GDI';

function SetTextCharacterExtra(hdc: HDC; nExtraSpace: SmallInt): SmallInt; external 'GDI';
function GetTextCharacterExtra(hdc: HDC): SmallInt; external 'GDI';

function SetTextJustification(hdc: HDC; nExtraSpace, cBreakChars: SmallInt): SmallInt; external 'GDI';

{ Font support }

function CreateFont(nHeight, nWidth, nEscapement, nOrientation, fnWeight: SmallInt;
                    fbItalic, fbUnderline, fbStrikeOut, fbCharSet,
                    fbOutputPrecision, fbClipPrecision, fbQuality, fbPitchAndFamily: BYTE; lpszFace: LPCSTR): HFONT; external 'GDI';
function CreateFontIndirect(lplf: LPLOGFONT): HFONT; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreateFontIndirect(var lf: LOGFONT): HFONT; external 'GDI';
{$endif}

function SetMapperFlags(hdc: HDC; fdwMatch: DWORD): DWORD; external 'GDI';

function AddFontResource(lpszFilename: LPCSTR): SmallInt; external 'GDI';
function RemoveFontResource(lpszFile: LPCSTR): BOOL; external 'GDI';

function GetTextFace(hdc: HDC; cbBuffer: SmallInt; lpszFace: LPSTR): SmallInt; external 'GDI';

function GetAspectRatioFilter(hdc: HDC): DWORD; external 'GDI';

function GetTextMetrics(hdc: HDC; lptm: LPTEXTMETRIC): BOOL; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetTextMetrics(hdc: HDC; var tm: TEXTMETRIC): BOOL; external 'GDI';
{$endif}

function EnumFonts(hdc: HDC; lpszFace: LPCSTR; fntenmprc: OLDFONTENUMPROC; lParam: LPARAM): SmallInt; external 'GDI';
function EnumFonts(hdc: HDC; lpszFace: LPCSTR; fntenmprc: TFarProc; lParam: LPARAM): SmallInt; external 'GDI';

{ Bitmap support }

function CreateBitmap(nWidth, nHeight: SmallInt; cbPlanes, cbBits: UINT; lpvBits: FarPointer): HBITMAP; external 'GDI';
function CreateBitmapIndirect(lpbm: LPBITMAP): HBITMAP; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreateBitmapIndirect(var bm: BITMAP): HBITMAP; external 'GDI';
{$endif}
function CreateCompatibleBitmap(hdc: HDC; nWidth, nHeight: SmallInt): HBITMAP; external 'GDI';
function CreateDiscardableBitmap(hdc: HDC; nWidth, nHeight: SmallInt): HBITMAP; external 'GDI';
function CreateDIBitmap(hdc: HDC; lpbmih: LPBITMAPINFOHEADER; dwInit: DWORD; lpvBits: FarPointer; lpbmi: LPBITMAPINFO; fnColorUse: UINT): HBITMAP; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function CreateDIBitmap(hdc: HDC; var bmih: BITMAPINFOHEADER; dwInit: DWORD; lpvBits: FarPointer; var bmi: BITMAPINFO; fnColorUse: UINT): HBITMAP; external 'GDI';
{$endif}

function LoadBitmap(hinst: HINST; lpszBitmap: LPCSTR): HBITMAP; external 'USER';

function BitBlt(hdcDest: HDC; nXDest, nYDest, nWidth, nHeight: SmallInt; hdcSrc: HDC; nXSrc, nYSrc: SmallInt; dwRop: DWORD): BOOL; external 'GDI';

function PatBlt(hdc: HDC; nLeftRect, nTopRect, nwidth, nheight: SmallInt; fdwRop: DWORD): BOOL; external 'GDI';

function StretchBlt(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: SmallInt; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: SmallInt; fdwRop: DWORD): BOOL; external 'GDI';
function StretchDIBits(hdc: HDC; XDest, YDest, cxDest, cyDest, XSrc, YSrc, cxSrc, cySrc: SmallInt;
                       lpvBits: FarPointer; lpbmi: LPBITMAPINFO; fuColorUse: UINT; fdwRop: DWORD): SmallInt; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function StretchDIBits(hdc: HDC; XDest, YDest, cxDest, cyDest, XSrc, YSrc, cxSrc, cySrc: SmallInt;
                       lpvBits: FarPointer; var bmi: BITMAPINFO; fuColorUse: UINT; fdwRop: DWORD): SmallInt; external 'GDI';
{$endif}

function SetPixel(hdc: HDC; nXPos, nYPos: SmallInt; clrref: COLORREF): COLORREF; external 'GDI';
function GetPixel(hdc: HDC; nXPos, nYPos: SmallInt): COLORREF; external 'GDI';

function SetStretchBltMode(hdc: HDC; fnStretchMode: SmallInt): SmallInt; external 'GDI';
function GetStretchBltMode(hdc: HDC): SmallInt; external 'GDI';

function SetBitmapDimension(hbmp: HBITMAP; nWidth, nHeight: SmallInt): DWORD; external 'GDI';
function GetBitmapDimension(hbm: HBITMAP): DWORD; external 'GDI';

function SetROP2(hdc: HDC; fnDrawMode: SmallInt): SmallInt; external 'GDI';
function GetROP2(hdc: HDC): SmallInt; external 'GDI';

function SetBitmapBits(hbmp: HBITMAP; cBits: DWORD; lpvBits: FarPointer): LONG; external 'GDI';
function GetBitmapBits(hbm: HBITMAP; cbBuffer: LONG; lpvBits: FarPointer): LONG; external 'GDI';

function SetDIBits(hdc: HDC; hbmp: HBITMAP; uStartScan, cScanLines: UINT; lpvBits: FarPointer; lpbmi: LPBITMAPINFO; fuColorUse: UINT): SmallInt; external 'GDI';
function GetDIBits(hdc: HDC; hbmp: HBITMAP; nStartScan, cScanLines: UINT; lpvBits: FarPointer; lpbmi: LPBITMAPINFO; fuColorUse: UINT): SmallInt; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function SetDIBits(hdc: HDC; hbmp: HBITMAP; uStartScan, cScanLines: UINT; lpvBits: FarPointer; var bmi: BITMAPINFO; fuColorUse: UINT): SmallInt; external 'GDI';
function GetDIBits(hdc: HDC; hbmp: HBITMAP; nStartScan, cScanLines: UINT; lpvBits: FarPointer; var bmi: BITMAPINFO; fuColorUse: UINT): SmallInt; external 'GDI';
{$endif}

function SetDIBitsToDevice(hdc: HDC; XDest, YDest, cx, cy, XSrc, YSrc: SmallInt; uStartScan, cScanLines: UINT;
                    lpvBits: FarPointer; lpbmi: LPBITMAPINFO; fuColorUse: UINT): SmallInt; external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
function SetDIBitsToDevice(hdc: HDC; XDest, YDest, cx, cy, XSrc, YSrc: SmallInt; uStartScan, cScanLines: UINT;
                    lpvBits: FarPointer; var bmi: BITMAPINFO; fuColorUse: UINT): SmallInt; external 'GDI';
{$endif}

{ Metafile support }

function CreateMetaFile(lpszFile: LPCSTR): HDC; external 'GDI';
function CloseMetaFile(hdc: HDC): HMETAFILE; external 'GDI';

function GetMetaFile(lpszFile: LPCSTR): HMETAFILE; external 'GDI';
function DeleteMetaFile(hmf: HMETAFILE): BOOL; external 'GDI';
function CopyMetaFile(hmfSrc: HMETAFILE; lpszFile: LPCSTR): HMETAFILE; external 'GDI';

function PlayMetaFile(hdc: HDC; hmf: HMETAFILE): BOOL; external 'GDI';

function GetMetaFileBits(hmf: HMETAFILE): HGLOBAL; external 'GDI';
function SetMetaFileBits(hmf: HGLOBAL): HMETAFILE; external 'GDI';

procedure PlayMetaFileRecord(hdc: HDC; lpht: LPHANDLETABLE; lpmr: LPMETARECORD; cHandles: UINT); external 'GDI';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure PlayMetaFileRecord(hdc: HDC; var ht: HANDLETABLE; var mr: METARECORD; cHandles: UINT); external 'GDI';
{$endif}

function EnumMetaFile(hdc: HDC; hmf: HMETAFILE; mfenmprc: MFENUMPROC; lParam: LPARAM): BOOL; external 'GDI';
function EnumMetaFile(hdc: HDC; hmf: HMETAFILE; mfenmprc: TFarProc; lParam: LPARAM): BOOL; external 'GDI';

{ Printing support }

function QueryAbort(hdc: HDC; reserved: SmallInt): BOOL; external 'GDI';

{ GDI Escape support }

function Escape(hdc: HDC; nEscape, cbInput: SmallInt; lpszInData: LPCSTR; lpvOutData: FarPointer): SmallInt; external 'GDI';

{ System Metrics }

function GetSystemMetrics(nIndex: SmallInt): SmallInt; external 'USER';

function GetDoubleClickTime: UINT; external 'USER';
procedure SetDoubleClickTime(uInterval: UINT); external 'USER';

{ Rectangle support }

procedure SetRect(lprc: LPRECT; nLeft, nTop, nRight, nBottom: SmallInt); external 'USER';
procedure SetRectEmpty(lprc: LPRECT); external 'USER';

procedure CopyRect(lprcDst: LPRECT; lprcSrc: LPRECT); external 'USER';

function IsRectEmpty(lprc: LPRECT): BOOL; external 'USER';

function EqualRect(lprc1, lprc2: LPRECT): BOOL; external 'USER';

function IntersectRect(lprcDest: LPRECT; lprcSrc1, lprcSrc2: LPRECT): BOOL; external 'USER';
function UnionRect(lprcDest: LPRECT; lprcSrc1, lprcSrc2: LPRECT): BOOL; external 'USER';

procedure OffsetRect(lprc: LPRECT; x, y: SmallInt); external 'USER';
procedure InflateRect(lprc: LPRECT; xAmt, yAmt: SmallInt); external 'USER';

function PtInRect(lprc: LPRECT; pt: POINT): BOOL; external 'USER';

{$ifdef VAR_PARAMS_ARE_FAR}
procedure SetRect(var rc: RECT; nLeft, nTop, nRight, nBottom: SmallInt); external 'USER';
procedure SetRectEmpty(var rc: RECT); external 'USER';

procedure CopyRect(var rcDst: RECT; var rcSrc: RECT); external 'USER';

function IsRectEmpty(var rc: RECT): BOOL; external 'USER';

function EqualRect(var rc1, rc2: RECT): BOOL; external 'USER';

function IntersectRect(var rcDest: RECT; var rcSrc1, rcSrc2: RECT): BOOL; external 'USER';
function UnionRect(var rcDest: RECT; var rcSrc1, rcSrc2: RECT): BOOL; external 'USER';

procedure OffsetRect(var rc: RECT; x, y: SmallInt); external 'USER';
procedure InflateRect(var rc: RECT; xAmt, yAmt: SmallInt); external 'USER';

function PtInRect(var rc: RECT; pt: POINT): BOOL; external 'USER';
{$endif}

{ Window message support }

function RegisterWindowMessage(lpsz: LPCSTR): UINT; external 'USER';

function GetMessage(lpmsg: LPMSG; hwnd: HWND; uMsgFilterMin, uMsgFilterMax: UINT): BOOL; external 'USER';
function PeekMessage(lpmsg: LPMSG; hwnd: HWND; uFilterFirst, uFilterLast, fuRemove: UINT): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetMessage(var msg: MSG; hwnd: HWND; uMsgFilterMin, uMsgFilterMax: UINT): BOOL; external 'USER';
function PeekMessage(var msg: MSG; hwnd: HWND; uFilterFirst, uFilterLast, fuRemove: UINT): BOOL; external 'USER';
{$endif}

procedure WaitMessage; external 'USER';

function GetMessagePos: DWORD; external 'USER';
function GetMessageTime: LONG; external 'USER';

function TranslateMessage(lpmsg: LPMSG): BOOL; external 'USER';
function DispatchMessage(lpmsg: LPMSG): LONG; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function TranslateMessage(var msg: MSG): BOOL; external 'USER';
function DispatchMessage(var msg: MSG): LONG; external 'USER';
{$endif}

function SetMessageQueue(cMsg: SmallInt): BOOL; external 'USER';

function GetInputState: BOOL; external 'USER';

function PostMessage(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; external 'USER';
function SendMessage(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external 'USER';

function PostAppMessage(htask: HTASK; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; external 'USER';

procedure ReplyMessage(lResult: LRESULT); external 'USER';
function InSendMessage: BOOL; external 'USER';

function CallMsgFilter(lpmsg: LPMSG; nCode: SmallInt): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function CallMsgFilter(var msg: MSG; nCode: SmallInt): BOOL; external 'USER';
{$endif}

{ Application termination }

procedure PostQuitMessage(nExitCode: SmallInt); external 'USER';

{ Window class management }

{ in Windows 3.1+, RegisterClass returns an ATOM that unquely identifies the 
  class. In Windows 3.0 and earlier, the return value is BOOL. That's why we
  redefine this function in the win31 unit. }
function RegisterClass(lpwc: LPWNDCLASS): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function RegisterClass(var wc: WNDCLASS): BOOL; external 'USER';
{$endif}
function UnregisterClass(lpszClassName: LPCSTR; hinst: HINST): BOOL; external 'USER';

function GetClassInfo(hinst: HINST; lpszClassName: LPCSTR; lpwc: LPWNDCLASS): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetClassInfo(hinst: HINST; lpszClassName: LPCSTR; var wc: WNDCLASS): BOOL; external 'USER';
{$endif}
function GetClassName(hwnd: HWND; lpszClassName: LPSTR; cchClassName: SmallInt): SmallInt; external 'USER';

function GetClassWord(hwnd: HWND; offset: SmallInt): WORD; external 'USER';
function SetClassWord(hwnd: HWND; nIndex: SmallInt; wNewWord: WORD): WORD; external 'USER';
function GetClassLong(hwnd: HWND; offset: SmallInt): LONG; external 'USER';
function SetClassLong(hwnd: HWND; nIndex: SmallInt; nVal: LONG): LONG; external 'USER';

{ Window creation/destroy }

function IsWindow(hwnd: HWND): BOOL; external 'USER';

function CreateWindowEx(dwExStyle: DWORD; lpszClassName, lpszWindowName: LPCSTR; dwStyle: DWORD; x, y, nWidth, nHeight: SmallInt; hwndParent: HWND; hmenu: HMENU; hinst: HINST; lpvCreateParams: FarPointer): HWND; external 'USER';
function CreateWindow(lpszClassName, lpszWindowName: LPCSTR; dwStyle: DWORD; x, y, nWidth, nHeight: SmallInt; hwndParent: HWND; hmenu: HMENU; hinst: HINST; lpvParam: FarPointer): HWND; external 'USER';

function DestroyWindow(hwnd: HWND): BOOL; external 'USER';

{ Basic window attributes }

function GetWindowTask(hwnd: HWND): HTASK; external 'USER';

function IsChild(hwndParent, hwndChild: HWND): BOOL; external 'USER';

function GetParent(hwnd: HWND): HWND; external 'USER';
function SetParent(hwndChild, hwndNewParent: HWND): HWND; external 'USER';

function IsWindowVisible(hwnd: HWND): BOOL; external 'USER';

function ShowWindow(hwnd: HWND; nCmdShow: SmallInt): BOOL; external 'USER';

{ Enabled state }
function EnableWindow(hwnd: HWND; fEnable: BOOL): BOOL; external 'USER';
function IsWindowEnabled(hwnd: HWND): BOOL; external 'USER';

{ Window text }
procedure SetWindowText(hwnd: HWND; lpsz: LPCSTR); external 'USER';
function GetWindowText(hwnd: HWND; lpsz: LPSTR; cbMax: SmallInt): SmallInt; external 'USER';
function GetWindowTextLength(hwnd: HWND): SmallInt; external 'USER';

{ Window words }
function GetWindowWord(hwnd: HWND; nOffset: SmallInt): WORD; external 'USER';
function SetWindowWord(hwnd: HWND; nOffset: SmallInt; nVal: WORD): WORD; external 'USER';
function GetWindowLong(hwnd: HWND; nOffset: SmallInt): LONG; external 'USER';
function SetWindowLong(hwnd: HWND; nOffset: SmallInt; nVal: LONG): LONG; external 'USER';

{ Window size, position, Z-order, and visibility }

procedure GetClientRect(hwnd: HWND; lprc: LPRECT); external 'USER';
procedure GetWindowRect(hwnd: HWND; lprc: LPRECT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure GetClientRect(hwnd: HWND; var rc: RECT); external 'USER';
procedure GetWindowRect(hwnd: HWND; var rc: RECT); external 'USER';
{$endif}

function SetWindowPos(hwnd, hwndInsertAfter: HWND; x, y, cx, cy: SmallInt; fuFlags: UINT): BOOL; external 'USER';

function BeginDeferWindowPos(cWindows: SmallInt): HDWP; external 'USER';
function DeferWindowPos(hdwp: HDWP; hwnd, hwndInsertAfter: HWND; x, y, cx, cy: SmallInt; flags: UINT): HDWP; external 'USER';
function EndDeferWindowPos(hdwp: HDWP): BOOL; external 'USER';

function MoveWindow(hwnd: HWND; nLeft, nTop, nWidth, nHeight: SmallInt; fRepaint: BOOL): BOOL; external 'USER';
function BringWindowToTop(hwnd: HWND): BOOL; external 'USER';

{ Window proc implementation & subclassing support }

function DefWindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external 'USER';

function CallWindowProc(wndprcPrev: WNDPROC; hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external 'USER';
function CallWindowProc(wndprcPrev: TFarProc; hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external 'USER';

{ Main window support }

procedure AdjustWindowRect(lprc: LPRECT; dwStyle: DWORD; fMenu: BOOL); external 'USER';
procedure AdjustWindowRectEx(lprc: LPRECT; dwStyle: DWORD; fMenu: BOOL; dwExStyle: DWORD); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure AdjustWindowRect(var rc: RECT; dwStyle: DWORD; fMenu: BOOL); external 'USER';
procedure AdjustWindowRectEx(var rc: RECT; dwStyle: DWORD; fMenu: BOOL; dwExStyle: DWORD); external 'USER';
{$endif}

function FlashWindow(hwnd: HWND; fInvert: BOOL): BOOL; external 'USER';

procedure ShowOwnedPopups(hwnd: HWND; fShow: BOOL); external 'USER';

{ Obsolete functions }
function OpenIcon(hwnd: HWND): BOOL; external 'USER';
procedure CloseWindow(hwnd: HWND); external 'USER';
function AnyPopup: BOOL; external 'USER';
function IsIconic(hwnd: HWND): BOOL; external 'USER';
function IsZoomed(hwnd: HWND): BOOL; external 'USER';

{ Window coordinate mapping and hit-testing }

procedure ClientToScreen(hwnd: HWND; lppt: LPPOINT); external 'USER';
procedure ScreenToClient(hwnd: HWND; lppt: LPPOINT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure ClientToScreen(hwnd: HWND; var pt: POINT); external 'USER';
procedure ScreenToClient(hwnd: HWND; var pt: POINT); external 'USER';
{$endif}

function WindowFromPoint(pt: POINT): HWND; external 'USER';
function ChildWindowFromPoint(hwndParent: HWND; pt: POINT): HWND; external 'USER';

{ Window query and enumeration }

function GetDesktopWindow: HWND; external 'USER';

function FindWindow(lpszClassName, lpszWindow: LPCSTR): HWND; external 'USER';

function EnumWindows(wndenmprc: WNDENUMPROC; lParam: LPARAM): BOOL; external 'USER';
function EnumWindows(wndenmprc: TFarProc; lParam: LPARAM): BOOL; external 'USER';
function EnumChildWindows(hwndParent: HWND; wndenmprc: WNDENUMPROC; lParam: LPARAM): BOOL; external 'USER';
function EnumChildWindows(hwndParent: HWND; wndenmprc: TFarProc; lParam: LPARAM): BOOL; external 'USER';
function EnumTaskWindows(htask: HTASK; wndenmprc: WNDENUMPROC; lParam: LPARAM): BOOL; external 'USER';
function EnumTaskWindows(htask: HTASK; wndenmprc: TFarProc; lParam: LPARAM): BOOL; external 'USER';

function GetTopWindow(hwnd: HWND): HWND; external 'USER';

function GetWindow(hwnd: HWND; fuRel: UINT): HWND; external 'USER';
function GetNextWindow(hwnd: HWND; uFlag: UINT): HWND; external 'USER';

{ Window property support }

function SetProp(hwnd: HWND; lpsz: LPCSTR; hData: HANDLE): BOOL; external 'USER';
function GetProp(hwnd: HWND; lpsz: LPCSTR): HANDLE; external 'USER';
function RemoveProp(hwnd: HWND; lpsz: LPCSTR): HANDLE; external 'USER';

function EnumProps(hwnd: HWND; prpenmprc: PROPENUMPROC): SmallInt; external 'USER';
function EnumProps(hwnd: HWND; prpenmprc: TFarProc): SmallInt; external 'USER';

{ Window drawing support }

function GetDC(hwnd: HWND): HDC; external 'USER';
function ReleaseDC(hwnd: HWND; hdc: HDC): SmallInt; external 'USER';

function GetWindowDC(hwnd: HWND): HDC; external 'USER';

{ Window repainting }

function BeginPaint(hwnd: HWND; lpps: LPPAINTSTRUCT): HDC; external 'USER';
procedure EndPaint(hwnd: HWND; lpps: LPPAINTSTRUCT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function BeginPaint(hwnd: HWND; var ps: PAINTSTRUCT): HDC; external 'USER';
procedure EndPaint(hwnd: HWND; var ps: PAINTSTRUCT); external 'USER';
{$endif}

procedure UpdateWindow(hwnd: HWND); external 'USER';

function ExcludeUpdateRgn(hdc: HDC; hwnd: HWND): SmallInt; external 'USER';

function GetUpdateRect(hwnd: HWND; lprc: LPRECT; fErase: BOOL): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetUpdateRect(hwnd: HWND; var rc: RECT; fErase: BOOL): BOOL; external 'USER';
{$endif}
function GetUpdateRgn(hwnd: HWND; hrgn: HRGN; fErase: BOOL): SmallInt; external 'USER';

procedure InvalidateRect(hwnd: HWND; lprc: LPRECT; fErase: BOOL); external 'USER';
procedure ValidateRect(hwnd: HWND; lprc: LPRECT); external 'USER';

procedure InvalidateRgn(hwnd: HWND; hrgn: HRGN; fErase: BOOL); external 'USER';
procedure ValidateRgn(hwnd: HWND; hrgn: HRGN); external 'USER';

{ Window scrolling }

procedure ScrollWindow(hwnd: HWND; dx, dy: SmallInt; lprcScroll, lprcClip: LPRECT); external 'USER';
function ScrollDC(hdc: HDC; dx, dy: SmallInt; lprcScroll, lprcClip: LPRECT; hrgnUpdate: HRGN; lprcUpdate: LPRECT): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function ScrollDC(hdc: HDC; dx, dy: SmallInt; var rcScroll, rcClip: RECT; hrgnUpdate: HRGN; lprcUpdate: LPRECT): BOOL; external 'USER';
{$endif}

{ Window activation }

function SetActiveWindow(hwnd: HWND): HWND; external 'USER';
function GetActiveWindow: HWND; external 'USER';

function GetLastActivePopup(hwndOwner: HWND): HWND; external 'USER';

{ Keyboard input support }

function SetFocus(hwnd: HWND): HWND; external 'USER';
function GetFocus: HWND; external 'USER';

function GetKeyState(vkey: SmallInt): SmallInt; external 'USER';
function GetAsyncKeyState(vkey: SmallInt): SmallInt; external 'USER';

procedure GetKeyboardState(lpbKeyState: LPBYTE); external 'USER';
procedure SetKeyboardState(lpbKeyState: LPBYTE); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure GetKeyboardState(var bKeyState: TKeyboardState); external 'USER';
procedure SetKeyboardState(var bKeyState: TKeyboardState); external 'USER';
{$endif}

{ Mouse input support }

function SetCapture(hwnd: HWND): HWND; external 'USER';
procedure ReleaseCapture; external 'USER';
function GetCapture: HWND; external 'USER';

function SwapMouseButton(fSwap: BOOL): BOOL; external 'USER';

{ System modal window support }

function GetSysModalWindow: HWND; external 'USER';
function SetSysModalWindow(hwnd: HWND): HWND; external 'USER';

{ Timer support }
function SetTimer(hwnd: HWND; idTimer, uTimeout: UINT; tmprc: TIMERPROC): UINT; external 'USER';
function SetTimer(hwnd: HWND; idTimer, uTimeout: UINT; tmprc: TFarProc): UINT; external 'USER';
function KillTimer(hwnd: HWND; idTimer: UINT): BOOL; external 'USER';

{ Accelerator support }
function LoadAccelerators(hinst: HINST; lpszTableName: LPCSTR): HACCEL; external 'USER';
function TranslateAccelerator(hwnd: HWND; haccl: HACCEL; lpmsg: LPMSG): SmallInt; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function TranslateAccelerator(hwnd: HWND; haccl: HACCEL; var msg: MSG): SmallInt; external 'USER';
{$endif}

{ Menu support }

function CreateMenu: HMENU; external 'USER';
function CreatePopupMenu: HMENU; external 'USER';
function LoadMenu(hinst: HINST; lpszMenuName: LPCSTR): HMENU; external 'USER';
function LoadMenuIndirect(lpmith: LPVOID): HMENU; external 'USER';

function DestroyMenu(hmenu: HMENU): BOOL; external 'USER';

function GetMenu(hwnd: HWND): HMENU; external 'USER';
function SetMenu(hwnd: HWND; hmenu: HMENU): BOOL; external 'USER';

function GetSystemMenu(hwnd: HWND; fRevert: BOOL): HMENU; external 'USER';

procedure DrawMenuBar(hwnd: HWND); external 'USER';

function HiliteMenuItem(hwnd: HWND; hmenu: HMENU; idHiliteItem, fuHilite: UINT): BOOL; external 'USER';

function InsertMenu(hmenu: HMENU; idItem, fuFlags, idNewItem: UINT; lpNewItem: LPCSTR): BOOL; external 'USER';
function AppendMenu(hmenu: HMENU; fuFlags, idNewItem: UINT; lpNewItem: LPCSTR): BOOL; external 'USER';
function ModifyMenu(hmenu: HMENU; idItem, fuFlags, idNewItem: UINT; lpNewItem: LPCSTR): BOOL; external 'USER';
function RemoveMenu(hmenu: HMENU; idItem, fuFlags: UINT): BOOL; external 'USER';
function DeleteMenu(hmenu: HMENU; idItem, fuFlags: UINT): BOOL; external 'USER';

function ChangeMenu(hmenu: HMENU; uPosition: UINT; lpNewItem: LPCSTR; idNewItem, fuFlags: UINT): BOOL; external 'USER';

function EnableMenuItem(hmenu: HMENU; idEnableItem, uEnable: UINT): BOOL; external 'USER';
function CheckMenuItem(hmenu: HMENU; idCheckItem, uCheck: UINT): BOOL; external 'USER';

function GetSubMenu(hmenu: HMENU; nPos: SmallInt): HMENU; external 'USER';

function GetMenuItemCount(hmenu: HMENU): SmallInt; external 'USER';
function GetMenuItemID(hmenu: HMENU; pos: SmallInt): UINT; external 'USER';

function GetMenuString(hmenu: HMENU; idItem: UINT; lpsz: LPSTR; cbMax: SmallInt; fwFlags: UINT): SmallInt; external 'USER';
function GetMenuState(hmenu: HMENU; idItem, fuFlags: UINT): UINT; external 'USER';

function SetMenuItemBitmaps(hmenu: HMENU; idItem, fuFlags: UINT; hbmUnchecked, hbmChecked: HBITMAP): BOOL; external 'USER';
function GetMenuCheckMarkDimensions: DWORD; external 'USER';

function TrackPopupMenu(hmenu: HMENU; fuFlags: UINT; x, y, nReserved: SmallInt; hwnd: HWND; lprc: LPRECT): BOOL; external 'USER';

{ Scroll bar support }

function SetScrollPos(hwnd: HWND; fnBar, nPos: SmallInt; fRepaint: BOOL): SmallInt; external 'USER';
function GetScrollPos(hwnd: HWND; fnBar: SmallInt): SmallInt; external 'USER';
procedure SetScrollRange(hwnd: HWND; fnBar, nMin, nMax: SmallInt; fRedraw: BOOL); external 'USER';
procedure GetScrollRange(hwnd: HWND; fnBar: SmallInt; lpnMinPos, lpnMaxPos: LPINT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure GetScrollRange(hwnd: HWND; fnBar: SmallInt; var nMinPos, nMaxPos: SmallInt); external 'USER';
{$endif}
procedure ShowScrollBar(hwnd: HWND; fnBar: SmallInt; fShow: BOOL); external 'USER';

{ Clipboard manager }

{ Clipboard Manager Functions }
function OpenClipboard(hwnd: HWND): BOOL; external 'USER';
function CloseClipboard: BOOL; external 'USER';
function EmptyClipboard: BOOL; external 'USER';

function GetClipboardOwner: HWND; external 'USER';

function SetClipboardViewer(hwnd: HWND): HWND; external 'USER';
function GetClipboardViewer: HWND; external 'USER';

function SetClipboardData(uFormat: UINT; hData: HANDLE): HANDLE; external 'USER';
function GetClipboardData(uFormat: UINT): HANDLE; external 'USER';

function IsClipboardFormatAvailable(uFormat: UINT): BOOL; external 'USER';
function GetPriorityClipboardFormat(lpuPriorityList: LPUINT; cEntries: SmallInt): SmallInt; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function GetPriorityClipboardFormat(var uPriorityList; cEntries: SmallInt): SmallInt; external 'USER';
{$endif}

function RegisterClipboardFormat(lpszFormatName: LPCSTR): UINT; external 'USER';
function CountClipboardFormats: SmallInt; external 'USER';
function EnumClipboardFormats(uFormat: UINT): UINT; external 'USER';
function GetClipboardFormatName(uFormat: UINT; lpszFormatName: LPSTR; cbMax: SmallInt): SmallInt; external 'USER';

function ChangeClipboardChain(hwnd, hwndNext: HWND): BOOL; external 'USER';

{ Mouse cursor support }

function LoadCursor(hinst: HINST; pszCursor: LPCSTR): HCURSOR; external 'USER';
function CreateCursor(hinst: HINST; xHotSpot, yHotSpot, nWidth, nHeight: SmallInt; lpvANDplane, lpvXORplane: LPVOID): HCURSOR; external 'USER';
function DestroyCursor(hcur: HCURSOR): BOOL; external 'USER';

function ShowCursor(fShow: BOOL): SmallInt; external 'USER';

procedure SetCursorPos(x, y: SmallInt); external 'USER';
procedure GetCursorPos(lppt: LPPOINT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure GetCursorPos(var pt: POINT); external 'USER';
{$endif}

function SetCursor(hcur: HCURSOR): HCURSOR; external 'USER';

procedure ClipCursor(lprc: LPRECT); external 'USER';

{ Icon support }

function LoadIcon(hinst: HINST; pszIcon: LPCSTR): HICON; external 'USER';
function CreateIcon(hinst: HINST; nWidth, nHeight: SmallInt; bPlanes, bBitsPerPixel: BYTE; lpvANDbits, lpvXORbits: LPVOID): HICON; external 'USER';
function DestroyIcon(hicon: HICON): BOOL; external 'USER';

function DrawIcon(hdc: HDC; x, y: SmallInt; hicon: HICON): BOOL; external 'USER';

{ Message Box support }

function MessageBox(hwndParent: HWND; lpszText, lpszTitle: LPCSTR; fuStyle: UINT): SmallInt; external 'USER';
procedure MessageBeep(uAlert: UINT); external 'USER';

{ Caret support }

procedure CreateCaret(hwnd: HWND; hbmp: HBITMAP; nWidth, nHeight: SmallInt); external 'USER';
procedure DestroyCaret; external 'USER';

procedure SetCaretPos(x, y: SmallInt); external 'USER';
procedure GetCaretPos(lppt: LPPOINT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure GetCaretPos(var pt: POINT); external 'USER';
{$endif}

procedure HideCaret(hwnd: HWND); external 'USER';
procedure ShowCaret(hwnd: HWND); external 'USER';

function GetCaretBlinkTime: UINT; external 'USER';
procedure SetCaretBlinkTime(uMSeconds: UINT); external 'USER';

{ MDI Support }

function DefFrameProc(hwnd, hwndMDIClient: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external 'USER';
function DefMDIChildProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external 'USER';

function TranslateMDISysAccel(hwndClient: HWND; lpmsg: LPMSG): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function TranslateMDISysAccel(hwndClient: HWND; var msg: MSG): BOOL; external 'USER';
{$endif}

function ArrangeIconicWindows(hwnd: HWND): UINT; external 'USER';

{ Dialog and Control Management }

function IsDialogMessage(hwndDlg: HWND; lpmsg: LPMSG): BOOL; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function IsDialogMessage(hwndDlg: HWND; var msg: MSG): BOOL; external 'USER';
{$endif}

function DefDlgProc(hwndDlg: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external 'USER';

function CreateDialog(hinst: HINST; lpszDlgTemp: LPCSTR; hwndOwner: HWND; dlgprc: DLGPROC): HWND; external 'USER';
function CreateDialog(hinst: HINST; lpszDlgTemp: LPCSTR; hwndOwner: HWND; dlgprc: TFarProc): HWND; external 'USER';
function CreateDialogIndirect(hinst: HINST; lpvDlgTmp: FarPointer; hwndOwner: HWND; dlgprc: DLGPROC): HWND; external 'USER';
function CreateDialogIndirect(hinst: HINST; lpvDlgTmp: FarPointer; hwndOwner: HWND; dlgprc: TFarProc): HWND; external 'USER';
function CreateDialogParam(hinst: HINST; lpszDlgTemp: LPCSTR; hwndOwner: HWND; dlgprc: DLGPROC; lParamInit: LPARAM): HWND; external 'USER';
function CreateDialogParam(hinst: HINST; lpszDlgTemp: LPCSTR; hwndOwner: HWND; dlgprc: TFarProc; lParamInit: LPARAM): HWND; external 'USER';
function CreateDialogIndirectParam(hinst: HINST; lpvDlgTmp: FarPointer; hwndOwner: HWND; dlgprc: DLGPROC; lParamInit: LPARAM): HWND; external 'USER';
function CreateDialogIndirectParam(hinst: HINST; lpvDlgTmp: FarPointer; hwndOwner: HWND; dlgprc: TFarProc; lParamInit: LPARAM): HWND; external 'USER';

function DialogBox(hinst: HINST; lpszDlgTemp: LPCSTR; hwndOwner: HWND; dlgprc: DLGPROC): SmallInt; external 'USER';
function DialogBox(hinst: HINST; lpszDlgTemp: LPCSTR; hwndOwner: HWND; dlgprc: TFarProc): SmallInt; external 'USER';
function DialogBoxIndirect(hinst: HINST; hglbDlgTemp: HGLOBAL; hwndOwner: HWND; dlgprc: DLGPROC): SmallInt; external 'USER';
function DialogBoxIndirect(hinst: HINST; hglbDlgTemp: HGLOBAL; hwndOwner: HWND; dlgprc: TFarProc): SmallInt; external 'USER';
function DialogBoxParam(hinst: HINST; lpszDlgTemp: LPCSTR; hwndOwner: HWND; dlgprc: DLGPROC; lParamInit: LPARAM): SmallInt; external 'USER';
function DialogBoxParam(hinst: HINST; lpszDlgTemp: LPCSTR; hwndOwner: HWND; dlgprc: TFarProc; lParamInit: LPARAM): SmallInt; external 'USER';
function DialogBoxIndirectParam(hinst: HINST; hglbDlgTemp: HGLOBAL; hwndOwner: HWND; dlgprc: DLGPROC; lParamInit: LPARAM): SmallInt; external 'USER';
function DialogBoxIndirectParam(hinst: HINST; hglbDlgTemp: HGLOBAL; hwndOwner: HWND; dlgprc: TFarProc; lParamInit: LPARAM): SmallInt; external 'USER';

procedure EndDialog(hwndDlg: HWND; nResult: SmallInt); external 'USER';

function GetDlgCtrlID(hwnd: HWND): SmallInt; external 'USER';
function GetDlgItem(hwndDlg: HWND; idControl: SmallInt): HWND; external 'USER';
function SendDlgItemMessage(hwndDlg: HWND; idDlgItem: SmallInt; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; external 'USER';

procedure SetDlgItemInt(hwndDlg: HWND; idControl: SmallInt; uValue: UINT; fSigned: BOOL); external 'USER';
function GetDlgItemInt(hwndDlg: HWND; idControl: SmallInt; lpfTranslated: LPBOOL; fSigned: BOOL): UINT; external 'USER';

procedure SetDlgItemText(hwndDlg: HWND; idControl: SmallInt; lpsz: LPCSTR); external 'USER';
function GetDlgItemText(hwndDlg: HWND; idControl: SmallInt; lpsz: LPSTR; cbMax: SmallInt): SmallInt; external 'USER';

procedure CheckDlgButton(hwndDlg: HWND; idButton: SmallInt; uCheck: UINT); external 'USER';
procedure CheckRadioButton(hwndDlg: HWND; idFirstButton, idLastButton, idCheckButton: SmallInt); external 'USER';
function IsDlgButtonChecked(hwndDlg: HWND; idButton: SmallInt): UINT; external 'USER';

function GetNextDlgGroupItem(hwndDlg, hwndCtrl: HWND; fPrevious: BOOL): HWND; external 'USER';
function GetNextDlgTabItem(hwndDlg, hwndCtrl: HWND; fPrevious: BOOL): HWND; external 'USER';

procedure MapDialogRect(hwndDlg: HWND; lprc: LPRECT); external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
procedure MapDialogRect(hwndDlg: HWND; var rc: RECT); external 'USER';
{$endif}
function GetDialogBaseUnits: DWORD; external 'USER';

{ Dialog directory support }

function DlgDirList(hwndDlg: HWND; lpszPath: LPSTR; idListBox, idStaticPath: SmallInt; uFileType: UINT): SmallInt; external 'USER';
function DlgDirSelect(hwndDlg: HWND; lpszPath: LPSTR; idListBox: SmallInt): BOOL; external 'USER';

function DlgDirListComboBox(hwndDlg: HWND; lpszPath: LPSTR; idComboBox, idStaticPath: SmallInt; uFileType: UINT): SmallInt; external 'USER';
function DlgDirSelectComboBox(hwndDlg: HWND; lpszPath: LPSTR; idComboBox: SmallInt): BOOL; external 'USER';

{ Windows hook support }

function SetWindowsHook(idHook: SmallInt; hkprc: HOOKPROC): HHOOK; external 'USER';
function SetWindowsHook(idHook: SmallInt; hkprc: TFarProc): TFarProc; external 'USER';
function DefHookProc(nCode: SmallInt; uParam: WPARAM; dwParam: LPARAM; lphhook: LPHHOOK): LRESULT; external 'USER';
function DefHookProc(nCode: SmallInt; uParam: WPARAM; dwParam: LPARAM; lphhook: TFarProc): LRESULT; external 'USER';
function UnhookWindowsHook(idHook: SmallInt; hkprc: HOOKPROC): BOOL; external 'USER';
function UnhookWindowsHook(idHook: SmallInt; hkprc: TFarProc): BOOL; external 'USER';

{ Journalling support }

function EnableHardwareInput(fEnableInput: BOOL): BOOL; external 'USER';

{ Help support }

function WinHelp(hwndMain: HWND; lpszHelp: LPCSTR; usCommand: UINT; ulData: DWORD): BOOL; external 'USER';

{ Sound support }

function OpenSound: SmallInt; external 'SOUND';
procedure CloseSound; external 'SOUND';

function StartSound: SmallInt; external 'SOUND';
function StopSound: SmallInt; external 'SOUND';

function SetVoiceQueueSize(nVoice, cbQueue: SmallInt): SmallInt; external 'SOUND';
function SetVoiceNote(voice, value, length, cdots: SmallInt): SmallInt; external 'SOUND';
function SetVoiceAccent(nVoice, nTempo, nVolume, fnMode, nPitch: SmallInt): SmallInt; external 'SOUND';
function SetVoiceEnvelope(nVoice, nShape, nRepeat: SmallInt): SmallInt; external 'SOUND';
function SetVoiceSound(nVoice: SmallInt; dwFrequency: DWORD; nDuration: SmallInt): SmallInt; external 'SOUND';

function SetVoiceThreshold(voice, cNotesThreshold: SmallInt): SmallInt; external 'SOUND';
function GetThresholdEvent: LPINT; external 'SOUND';
function GetThresholdStatus: SmallInt; external 'SOUND';

function SetSoundNoise(fnSource, nDuration: SmallInt): SmallInt; external 'SOUND';

function WaitSoundState(fnState: SmallInt): SmallInt; external 'SOUND';

function SyncAllVoices: SmallInt; external 'SOUND';
function CountVoiceNotes(nvoice: SmallInt): SmallInt; external 'SOUND';

{ Comm support }

function BuildCommDCB(lpszDef: LPCSTR; lpdcb: LPDCB): SmallInt; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function BuildCommDCB(lpszDef: LPCSTR; var dcb: DCB): SmallInt; external 'USER';
{$endif}

function OpenComm(lpszDevControl: LPCSTR; cbInQueue, cbOutQueue: UINT): SmallInt; external 'USER';
function CloseComm(idComDev: SmallInt): SmallInt; external 'USER';

function ReadComm(idComDev: SmallInt; lpvBuf: FarPointer; cbRead: SmallInt): SmallInt; external 'USER';
function ReadComm(idComDev: SmallInt; lpvBuf: PFarChar; cbRead: SmallInt): SmallInt; external 'USER';
function WriteComm(idComDev: SmallInt; lpvBuf: FarPointer; cbWrite: SmallInt): SmallInt; external 'USER';
function WriteComm(idComDev: SmallInt; lpvBuf: PFarChar; cbWrite: SmallInt): SmallInt; external 'USER';
function UngetCommChar(idComDev: SmallInt; chUnget: char): SmallInt; external 'USER';
function FlushComm(idComDev, fnQueue: SmallInt): SmallInt; external 'USER';
function TransmitCommChar(idComDev: SmallInt; chTransmit: char): SmallInt; external 'USER';

function SetCommState(lpdcb: LPDCB): SmallInt; external 'USER';
function GetCommState(idComDev: SmallInt; lpdcb: LPDCB): SmallInt; external 'USER';
function GetCommError(idComDev: SmallInt; lpStat: LPCOMSTAT): SmallInt; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function SetCommState(var dcb: DCB): SmallInt; external 'USER';
function GetCommState(idComDev: SmallInt; var dcb: DCB): SmallInt; external 'USER';
function GetCommError(idComDev: SmallInt; var Stat: COMSTAT): SmallInt; external 'USER';
{$endif}

function SetCommBreak(idComDev: SmallInt): SmallInt; external 'USER';
function ClearCommBreak(idComDev: SmallInt): SmallInt; external 'USER';

function SetCommEventMask(idComDev: SmallInt; fuEvtMask: UINT): LPUINT; external 'USER';
function GetCommEventMask(idComDev, fnEvtClear: SmallInt): UINT; external 'USER';

function EscapeCommFunction(idComDev, nFunction: SmallInt): LONG; external 'USER';

{ String formatting support }

function wvsprintf(lpszOut: LPSTR; lpszFmt: LPCSTR; lpParams: FarPointer): SmallInt; external 'USER';
{$ifdef VAR_PARAMS_ARE_FAR}
function wvsprintf(lpszOut: LPSTR; lpszFmt: LPCSTR; var Params): SmallInt; external 'USER';
{$endif}

{$ifndef SYSTEMUNIT}
function wsprintf(lpszOut: LPSTR; lpszFmt: LPCSTR; etc: array of const): SmallInt; cdecl; external 'USER' name '_wsprintf';
{$endif not SYSTEMUNIT}

