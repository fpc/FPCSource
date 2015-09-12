
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

function GlobalDiscard(h: HGLOBAL): HGLOBAL;
begin
  GlobalDiscard := GlobalReAlloc(h, 0, GMEM_MOVEABLE);
end;

function LockData(dummy: SmallInt): HGLOBAL;
begin
  LockData := LockSegment(UINT(-1));
end;

procedure UnlockData(dummy: SmallInt);
begin
  UnlockSegment(UINT(-1));
end;

function LocalDiscard(h: HLOCAL): HLOCAL;
begin
  LocalDiscard := LocalReAlloc(h, 0, LMEM_MOVEABLE);
end;

function UnlockResource(hResData: HGLOBAL): BOOL;
begin
  UnlockResource := GlobalUnlock(hResData);
end;

function RGB(cRed, cGreen, cBlue: BYTE): COLORREF;
begin
  RGB := COLORREF(cRed or (WORD(cGreen) shl 8) or (DWORD(cBlue) shl 16));
end;

function GetRValue(rgb: DWORD): BYTE;
begin
  GetRValue := Byte(rgb);
end;

function GetGValue(rgb: DWORD): BYTE;
begin
  GetGValue := Byte((Word(rgb) shr 8));
end;

function GetBValue(rgb: DWORD): BYTE;
begin
  GetBValue := Byte(rgb shr 16);
end;

function PALETTERGB(cRed, cGreen, cBlue: BYTE): COLORREF;
begin
  PALETTERGB := $02000000 or RGB(cRed, cGreen, cBlue);
end;

function PALETTEINDEX(wPaletteIndex: WORD): COLORREF;
begin
  PALETTEINDEX := COLORREF(DWORD($01000000) or DWORD(wPaletteIndex));
end;
