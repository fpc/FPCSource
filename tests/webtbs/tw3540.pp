{ %target=win32 }
{ %norun }
{ %cpu=i386 }
{ %opt=-s -Amasm -TWin32 -Rintel }
{ Source provided for Free Pascal Bug Report 3540 }
{ Submitted by "Vladimir Panteleev" on  2005-01-11 }
{ e-mail: thecybershadow@gmail.com }
library Test;

type
  Integer=LongInt;

  ULONG=Cardinal;
  PUCHAR=PChar;

type
  TMyVars=record
    RandSeed: Integer;
    SX: Integer;
    end;

  TMyData=record
    SomeConst: Integer;
    end;

var
  RMyData:TMyData=(
    SomeConst: 31337;
    );

{$R-,Q-}

function CalcAddr(const V):Pointer;stdcall;assembler;
var
  P: Integer absolute V;
asm
  call @@next
@@next:
  pop eax
  sub eax, offset @@next
  add eax, P
end;

type
  PMyVars=^TMyVars;

var
  RMyVars: TMyVars;

function MyVars:PMyVars;
begin MyVars:=CalcAddr(RMyVars) end;

type
  PMyData=^TMyData;

function MyData:PMyData;
begin MyData:=CalcAddr(RMyData) end;

procedure VidBufferToScreenBlt(Buffer:PUCHAR; x:ULONG; y:ULONG; width:ULONG; height:ULONG; lDelta:ULONG); stdcall; external;
type TPalette=array[0..15,0..3]of byte;PPalette=^TPalette;
function Palette:PPalette; external;

type
  TBitmapInfo=record
    Width, Height: Word;
    Bits: Pointer;
    end;

procedure DrawBitmap(X, Y: Integer; const Bitmap: TBitmapInfo);
begin
  VidBufferToScreenBlt(CalcAddr(Bitmap.Bits^), X, Y, Bitmap.Width, Bitmap.Height, 4);
end;

const
  test_palette: TPalette = (
    ($00, $00, $00, 0),
    ($80, $00, $00, 0),
    ($00, $80, $00, 0),
    ($80, $80, $00, 0),
    ($00, $00, $80, 0),
    ($80, $00, $80, 0),
    ($00, $80, $80, 0),
    ($80, $80, $80, 0),
    ($C0, $C0, $C0, 0),
    ($FF, $00, $00, 0),
    ($00, $FF, $00, 0),
    ($FF, $FF, $00, 0),
    ($00, $00, $FF, 0),
    ($FF, $00, $FF, 0),
    ($00, $FF, $FF, 0),
    ($FF, $FF, $FF, 0));

procedure Draw; stdcall;
var
  i:integer;
  x1,y1,x2,y2:integer;
begin
  Palette^:=TPalette(CalcAddr(Test_palette)^);
end;

end.
