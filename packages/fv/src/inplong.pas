Unit InpLong;

(*--
TInputLong is a derivitave of TInputline designed to accept LongInt
numeric input.  Since both the upper and lower limit of acceptable numeric
input can be set, TInputLong may be used for Integer, Word, or Byte input
as well.  Option flag bits allow optional hex input and display.  A blank
field may optionally be rejected or interpreted as zero.

Methods

constructor Init(var R : TRect; AMaxLen : Integer;
                LowerLim, UpperLim : LongInt; Flgs : Word);

Calls TInputline.Init and saves the desired limits and Flags.  Flags may
be a combination of:

ilHex          will accept hex input (preceded by '$')  as well as decimal.
ilBlankEqZero  if set, will interpret a blank field as '0'.
ilDisplayHex   if set, will display numeric as hex when possible.


constructor Load(var S : TStream);
procedure Store(var S : TStream);

The usual Load and Store routines.  Be sure to call RegisterType(RInputLong)
to register the type.


FUNCTION DataSize : Word; virtual;
PROCEDURE GetData(var Rec); virtual;
PROCEDURE SetData(var Rec); virtual;

The transfer methods.  DataSize is Sizeof(LongInt) and Rec should be
the address of a LongInt.


FUNCTION RangeCheck : Boolean; virtual;

Returns True if the entered string evaluates to a number >= LowerLim and
<= UpperLim.


PROCEDURE Error; virtual;

Error is called when RangeCheck fails.  It displays a messagebox indicating
the label (if any) of the faulting view, as well as the allowable range.


PROCEDURE HandleEvent(var Event : TEvent); virtual;

HandleEvent filters out characters which are not appropriate to numeric
input.  Tab and Shift Tab cause a call to RangeCheck and a call to Error
if RangeCheck returns false.  The input must be valid to Tab from the view.
There's no attempt made to stop moving to another view with the mouse.


FUNCTION Valid(Cmd : Word) : Boolean; virtual;

if TInputline.Valid is true and Cmd is neither cmValid or cmCancel, Valid
then calls RangeCheck.  If RangeCheck is false, then Error is called and
Valid returns False.

----*)

{$i platform.inc}

{$ifdef PPC_FPC}
  {$H-}
{$else}
  {$F+,O+,E+,N+}
{$endif}
{$X+,R-,I-,Q-,V-}
{$ifndef OS_UNIX}
  {$S-}
{$endif}

Interface
uses objects, drivers, views, dialogs, msgbox, fvconsts;

{flags for TInputLong constructor}
const
  ilHex = 1;          {will enable hex input with leading '$'}
  ilBlankEqZero = 2;  {No input (blank) will be interpreted as '0'}
  ilDisplayHex = 4;   {Number displayed as hex when possible}
Type
  TInputLong = Object(TInputLine)
    ILOptions : Word;
    LLim, ULim : LongInt;
    constructor Init(var R : TRect; AMaxLen : Sw_Integer;
        LowerLim, UpperLim : LongInt; Flgs : Word);
    constructor Load(var S : TStream);
    procedure Store(var S : TStream);
    FUNCTION DataSize : Sw_Word; virtual;
    PROCEDURE GetData(var Rec); virtual;
    PROCEDURE SetData(var Rec); virtual;
    FUNCTION RangeCheck : Boolean; virtual;
    PROCEDURE Error; virtual;
    PROCEDURE HandleEvent(var Event : TEvent); virtual;
    FUNCTION Valid(Cmd : Word) : Boolean; virtual;
    end;
  PInputLong = ^TInputLong;

const
  RInputLong : TStreamRec = (
    ObjType: idInputLong;
    VmtLink: Ofs(Typeof(TInputLong)^);
    Load : @TInputLong.Load;
    Store : @TInputLong.Store);

Implementation

{-----------------TInputLong.Init}
constructor TInputLong.Init(var R : TRect; AMaxLen : Sw_Integer;
        LowerLim, UpperLim : LongInt; Flgs : Word);
begin
if not TInputLine.Init(R, AMaxLen) then fail;
ULim := UpperLim;
LLim := LowerLim;
if Flgs and ilDisplayHex <> 0 then Flgs := Flgs or ilHex;
ILOptions := Flgs;
if ILOptions and ilBlankEqZero <> 0 then Data^ := '0';
end;

{-------------------TInputLong.Load}
constructor TInputLong.Load(var S : TStream);
begin
TInputLine.Load(S);
S.Read(ILOptions, Sizeof(ILOptions));
S.Read(LLim, Sizeof(LLim));
S.Read(ULim, Sizeof(ULim));
end;

{-------------------TInputLong.Store}
procedure TInputLong.Store(var S : TStream);
begin
TInputLine.Store(S);
S.Write(ILOptions, Sizeof(ILOptions));
S.Write(LLim, Sizeof(LLim));
S.Write(ULim, Sizeof(ULim));
end;

{-------------------TInputLong.DataSize}
FUNCTION TInputLong.DataSize:Sw_Word;
begin
DataSize := Sizeof(LongInt);
end;

{-------------------TInputLong.GetData}
PROCEDURE TInputLong.GetData(var Rec);
var code : Integer;
begin
Val(Data^, LongInt(Rec), code);
end;

FUNCTION Hex2(B : Byte) : String;
Const
  HexArray : array[0..15] of char = '0123456789ABCDEF';
begin
Hex2[0] := #2;
Hex2[1] := HexArray[B shr 4];
Hex2[2] := HexArray[B and $F];
end;

FUNCTION Hex4(W : Word) : String;
begin Hex4 := Hex2(Hi(W))+Hex2(Lo(W)); end;

FUNCTION Hex8(L : LongInt) : String;
begin Hex8 := Hex4(LongRec(L).Hi)+Hex4(LongRec(L).Lo); end;

function FormHexStr(L : LongInt) : String;
var
  Minus : boolean;
  S : string[20];
begin
Minus := L < 0;
if Minus then L := -L;
S := Hex8(L);
while (Length(S) > 1) and (S[1] = '0') do Delete(S, 1, 1);
S := '$' + S;
if Minus then System.Insert('-', S, 2);
FormHexStr := S;
end;

{-------------------TInputLong.SetData}
PROCEDURE TInputLong.SetData(var Rec);
var
  L : LongInt;
  S : string;
begin
L := LongInt(Rec);
if L > ULim then L := ULim
else if L < LLim then L := LLim;
if ILOptions and ilDisplayHex <> 0 then
  S := FormHexStr(L)
else
  Str(L : -1, S);
if Length(S) > MaxLen then S[0] := chr(MaxLen);
Data^ := S;
end;

{-------------------TInputLong.RangeCheck}
FUNCTION TInputLong.RangeCheck : Boolean;
var
  L : LongInt;
  code : Integer;
begin
if (Data^ = '') and (ILOptions and ilBlankEqZero <> 0) then
  Data^ := '0';
Val(Data^, L, code);
RangeCheck := (Code = 0) and (L >= LLim) and (L <= ULim);
end;

{-------------------TInputLong.Error}
PROCEDURE TInputLong.Error;
var
  SU, SL : string[40];
  PMyLabel : PLabel;
  Labl : string;
  I : Integer;

  function FindIt(P : PView) : boolean;{$ifdef PPC_BP}far;{$endif}
  begin
  FindIt := (Typeof(P^) = Typeof(TLabel)) and (PLabel(P)^.Link = PView(@Self));
  end;

begin
Str(LLim : -1, SL);
Str(ULim : -1, SU);
if ILOptions and ilHex <> 0 then
  begin
  SL := SL+'('+FormHexStr(LLim)+')';
  SU := SU+'('+FormHexStr(ULim)+')';
  end;
if Owner <> Nil then
  PMyLabel := PLabel(Owner^.FirstThat(@FindIt))
else PMyLabel := Nil;
if PMyLabel <> Nil then PMyLabel^.GetText(Labl)
else Labl := '';
if Labl <> '' then
  begin
  I := Pos('~', Labl);
  while I > 0 do
    begin
    System.Delete(Labl, I, 1);
    I := Pos('~', Labl);
    end;
  Labl := '"'+Labl+'"';
  end;
MessageBox(Labl + ^M^J'Value not within range '+SL+' to '+SU, Nil,
                            mfError+mfOKButton);
end;

{-------------------TInputLong.HandleEvent}
PROCEDURE TInputLong.HandleEvent(var Event : TEvent);
begin
if (Event.What = evKeyDown) then
  begin
    case Event.KeyCode of
       kbTab, kbShiftTab
          : if not RangeCheck then
              begin
              Error;
              SelectAll(True);
              ClearEvent(Event);
              end;
      end;
  if Event.CharCode <> #0 then  {a character key}
    begin
    Event.Charcode := Upcase(Event.Charcode);
    case Event.Charcode of
      '0'..'9', #1..#$1B : ;       {acceptable}

      '-'       : if (LLim >= 0) or (CurPos <> 0) then
                        ClearEvent(Event);
      '$'       : if ILOptions and ilHex = 0 then ClearEvent(Event);
      'A'..'F'  : if Pos('$', Data^) = 0 then ClearEvent(Event);

      else ClearEvent(Event);
      end;
    end;
  end;
TInputLine.HandleEvent(Event);
end;

{-------------------TInputLong.Valid}
FUNCTION TInputLong.Valid(Cmd : Word) : Boolean;
var
  Rslt : boolean;
begin
Rslt := TInputLine.Valid(Cmd);
if Rslt and (Cmd <> 0) and (Cmd <> cmCancel) then
  begin
  Rslt := RangeCheck;
  if not Rslt then
    begin
    Error;
    Select;
    SelectAll(True);
    end;
  end;
Valid := Rslt;
end;

end.
