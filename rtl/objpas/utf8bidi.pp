{
Author Mazen NEIFER
Licence LGPL
}
unit UTF8BIDI;

{$mode objfpc}{$H+}

interface

type
  TUCS32Char = Cardinal;
  TUCS16Char = Word;
  TUTF8Char = String[4];
  TUTF8String = UTF8String;
  TDirection=(
    drNONE,
    drRTL,
    drLTR
  );
  TVisualToLogical = Array[Byte]Of Byte;

{****************************Conversion routines*******************************}
{Converts an UCS 16/32 bits charcater to UTF8 character}
function UnicodeToUTF8(aChar:TUCS32Char):TUTF8Char;
{Converts a wide char UCS 16 bits chcarcter to UTF8 character}
function UnicodeToUTF8(aChar:WideChar):TUTF8Char;
{Converts an UTF8 character to UCS 32 bits character}
function UTF8ToUnicode(const UTF8Char:TUTF8Char):TUCS32Char;
{Converts an UTF8 string to a double byte string}
function UTF8ToDoubleByteString(const UTF8Str:TUTF8String):String;
function UTF8ToDoubleByte(UTF8Str:PChar; Len:Cardinal; DBStr:PByte):Cardinal;
{****************************Logical aspects***********************************}
{Returns the number of logical characters}
function LLength(const UTF8Str:TUTF8String):Cardinal;
{Converts visual position to logical position}
function LPos(const UTF8Str:TUTF8String; vp:Integer; pDir:TDirection):Cardinal;
{Returns character at a given logical position according to paragraph direction}
function LCharOf(UTF8String:TUTF8String; lp:Integer):TUTF8Char;
{****************************Visual aspects************************************}
{Returns the number of visual characters}
function VLength(const UTF8Str:TUTF8String):Cardinal;
function VLength(p: PChar; Count:Cardinal):Cardinal;
{Converts a logical position to a visual position}
function VPos(const UTF8Str:TUTF8String; lp:Integer; pDir, cDir:TDirection):Cardinal;
function VPos(UTF8Char:PChar; Len:integer; BytePos:integer):Cardinal;
{Returns character at a given visual position according to paragraph direction}
function VCharOf(UTF8Str:TUTF8String; vp:Integer; dir:TDirection):TUTF8Char;
{Inserts a string into an other paying attention of RTL/LTR direction}
procedure VInsert(const Src:TUTF8String; var Dest:TUTF8String; vp:Integer; pDir:TDirection);
{****************************Helper routines***********************************}
{Returns direction of a character}
function DirectionOf(UTF8Char:TUTF8Char):TDirection;
{Inserts a char as if it was typed using keyboard in the most user friendly way.
Returns the new cursor position after insersion depending on the new visual text}
function InsertChar(Src:TUTF8Char; var Dest:TUTF8String; vp:Integer; pDir:TDirection):Integer;
{Returns a table mapping each visual position to its logical position in an UTF8*
string}
function VisualToLogical(const UTF8String:TUTF8String; pDir:TDirection):TVisualToLogical;

implementation

function UTF8Str(const s:TUTF8String):String;
var
  i:Integer;
begin
  Result := '';
  for i:= 1 to Length(s) do
    case s[i] of
      #0..#127:
         Result := Result + s[i];
    else
      Result := Result + '$' + HexStr(Ord(s[i]),2);
    end;
end;
function ComputeCharLength(p:PChar):Cardinal;
begin
  if ord(p^)<%11000000
  then
{regular single byte character (#0 is a normal char, this is UTF8Charascal ;)}
    Result:=1
  else if ((ord(p^) and %11100000) = %11000000)
  then
    if (ord(p[1]) and %11000000) = %10000000 then
      Result:=2
    else
      Result:=1
  else if ((ord(p^) and %11110000) = %11100000)
  then
    if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
    then
      Result:=3
    else
        Result:=1
  else if ((ord(p^) and %11111000) = %11110000)
  then
    if ((ord(p[1]) and %11000000) = %10000000)
    and ((ord(p[2]) and %11000000) = %10000000)
    and ((ord(p[3]) and %11000000) = %10000000)
    then
      Result:=4
    else
      Result:=1
  else
    Result:=1
end;

{****************************Conversion routines*******************************}
function UnicodeToUTF8(aChar:TUCS32Char):TUTF8Char;
begin
  case aChar of
    0..$7f:
      begin
        Result[1]:=char(aChar);
        SetLength(UnicodeToUTF8,1);
      end;
    $80..$7ff:
      begin
        Result[1]:=char($c0 or (aChar shr 6));
        Result[2]:=char($80 or (aChar and $3f));
        SetLength(UnicodeToUTF8,2);
      end;
    $800..$ffff:
      begin
        SetLength(Result,3);
        Result[1]:=char($e0 or (aChar shr 12));
        Result[2]:=char($80 or ((aChar shr 6) and $3f));
        Result[3]:=char($80 or (aChar and $3f));
      end;
    $10000..$1fffff:
      begin
        SetLength(UnicodeToUTF8,4);
        Result[1]:=char($f0 or (aChar shr 18));
        Result[2]:=char($80 or ((aChar shr 12) and $3f));
        Result[3]:=char($80 or ((aChar shr 6) and $3f));
        Result[4]:=char($80 or (aChar and $3f));
      end;
  else
    SetLength(UnicodeToUTF8, 0);
  end;
end;

function UnicodeToUTF8(aChar:WideChar):TUTF8Char;
begin
  Result := UnicodeToUTF8(Word(aChar));
end;

function UTF8ToUnicode(const UTF8Char:TUTF8Char):TUCS32Char;
begin
  case ComputeCharLength(@UTF8Char[1]) of
    1:{regular single byte character (#0 is a normal char, this is UTF8Charascal ;)}
      Result := ord(UTF8Char[1]);
    2:
      Result := ((ord(UTF8Char[1]) and %00011111) shl 6)
                or (ord(UTF8Char[2]) and %00111111);
    3:
      Result := ((ord(UTF8Char[1]) and %00011111) shl 12)
                or ((ord(UTF8Char[1]) and %00111111) shl 6)
                or (ord(UTF8Char[2]) and %00111111);
    4:
      Result := ((ord(UTF8Char[1]) and %00011111) shl 18)
                or ((ord(UTF8Char[2]) and %00111111) shl 12)
                or ((ord(UTF8Char[3]) and %00111111) shl 6)
                or (ord(UTF8Char[4]) and %00111111);
  else
    Result := $FFFFFFFF;
  end
end;

function UTF8ToDoubleByteString(const UTF8Str: TUTF8String): string;
var
  Len: Integer;
begin
  Len:=VLength(UTF8Str);
  SetLength(Result,Len*2);
  if Len=0 then exit;
  UTF8ToDoubleByte(PChar(UTF8Str),length(UTF8Str),PByte(Result));
end;

function UTF8ToDoubleByte(UTF8Str: PChar; Len:Cardinal; DBStr: PByte):Cardinal;
var
  SrcPos: PChar;
  CharLen: LongInt;
  DestPos: PByte;
  u: Cardinal;
begin
  SrcPos:=UTF8Str;
  DestPos:=DBStr;
  Result:=0;
  while Len>0 do begin
    u:=UTF8ToUnicode(SrcPos);
    DestPos^:=byte((u shr 8) and $ff);
    inc(DestPos);
    DestPos^:=byte(u and $ff);
    inc(DestPos);
    inc(SrcPos,CharLen);
    dec(Len,CharLen);
    inc(Result);
  end;
end;
{****************************Logical aspects***********************************}
function LLength(const UTF8Str:TUTF8String):Cardinal;
begin
  Result := Length(UTF8Str);
end;

function LPos(const UTF8Str:TUTF8String; vp:Integer; pDir:TDirection):Cardinal;
var
  v2l:TVisualToLogical;
  i:integer;
begin
  v2l := VisualToLogical(UTF8Str, pDir);
  for i:= 0 to v2l[0] do Write(v2l[i],' ');writeln('vp=',vp,' v2l[vp]=',v2l[vp]);
  if vp <= v2l[0]
  then
    Result := v2l[vp]
  else
    Result := Length(UTF8Str) + 1;
end;

function LCharOf(UTF8String:TUTF8String; lp:Integer):TUTF8Char;
begin
  if lp > Length(UTF8String)
  then
    Exit('');
  while(lp > 0) and ((Ord(UTF8String[lp]) and $F0) = $80) do
    Dec(lp);
  if lp = 0
  then
    Exit('');
  Move(UTF8String[lp], Result[1], SizeOf(TUTF8Char) - 1);
  SetLength(Result, ComputeCharLength(@Result[1]));
end;
{****************************Visual aspects************************************}
function VLength(const UTF8Str:TUTF8String):Cardinal;
begin
  Result := VLength(PChar(UTF8Str),LLength(UTF8Str));
end;

function VLength(p:PChar; Count:Cardinal):Cardinal;
var
  CharLen: LongInt;
begin
  Result := 0;
  while (Count>0) do begin
    inc(Result);
    CharLen:=ComputeCharLength(p);
    inc(p,CharLen);
    dec(Count,CharLen);
  end;
end;

function VPos(const UTF8Str:TUTF8String; lp:Integer; pDir, cDir:TDirection):Cardinal;
var
  v2l:TVisualToLogical;
  vp:Integer;
begin
  v2l := VisualToLogical(UTF8Str, pDir);
  for vp := 1 to v2l[0] do
  if lp = v2l[vp]
  then
    begin
      Exit(vp);
    end;
  Result := v2l[0];
end;

function VPos(UTF8Char:PChar; Len:integer; BytePos:integer):Cardinal;
begin
end;


function VCharOf(UTF8Str:TUTF8String; vp:Integer; dir:TDirection):TUTF8Char;
var
  CharLen: LongInt;
begin
  Result:=LCharOf(UTF8Str,LPos(UTF8Str, vp, dir));
end;

{****************************Helper routines***********************************}
function DirectionOf(UTF8Char:TUTF8Char):TDirection;
begin
  case UTF8Char[1] of
    #9,#32,'/','{','}','[',']','(',')':
      Result := drNONE;
    #$D8,#$D9:
      Result := drRTL;
  else
    Result := drLTR;
  end;
end;

function VisualToLogical(const UTF8String:TUTF8String; pDir:TDirection):TVisualToLogical;
  procedure Insert(value:Byte; var v2l:TVisualToLogical; InsPos:Byte);
    var
      l:Byte;
    begin
      if v2l[0] < 255
      then
        Inc(InsPos);
      if InsPos > v2l[0]
      then
        InsPos := v2l[0];
      for l := v2l[0] downto InsPos do
        v2l[l] := v2l[l-1];
      v2l[InsPos] := Value;
    end;
var
  lp, vp : Integer;
  cDir:TDirection;
  Character:TUTF8Char;
i:Integer;
begin
  Result[0] := 0;
  lp := 1;
  vp := 1;
  while lp <= Length(UTF8String) do
  begin
    Character := LCharOf(UTF8String, lp);
    cDir := DirectionOf(Character);
    Inc(Result[0]);
          WriteLn('lpos=',lp,' vpos=',vp,' cDir=',Byte(cDir));
    case cDir of
      drRTL:
        begin
          pDir := drRTL;
        end;
      drLTR:
        begin
          pDir := drLTR;
          vp := Result[0];
        end;
    else
      case pDir of
        drRTL:;
        drLTR:
          vp := Result[0];
      else
        vp := vp;
      end;
    end;
    Insert(lp, Result, vp);
    for i := 1 to Result[0] do Write('v2l[',i,']=',Result[vp],'/',lp);
    Inc(lp, Length(Character));
  end;
end;

function InsertChar(Src:TUTF8Char; var Dest:TUTF8String; vp:Integer; pDir:TDirection):Integer;
var
  v2l:TVisualToLogical;
  lp:Integer;
begin
  v2l := VisualToLogical(Dest, pDir);
  if vp > v2l[0]
  then
    begin
      lp := Length(Dest) + 1
    end
  else
    lp := v2l[vp];
Write('vp=',vp,' lp=',lp,' len=', Length(Dest));
  case DirectionOf(Src) of
    drRTL:
      begin
        if lp > Length(Dest)
        then
          Insert(Src, Dest, v2l[v2l[0]])
        else
          if(vp > v2l[0]) or (DirectionOf(LCharOf(Dest,v2l[vp])) = drRTL)
          then
            Insert(Src, Dest, lp + Length(LCharOf(Dest, lp)))
          else
            Insert(Src, Dest, lp);
        Result := vp;
      end;
    drLTR:
      begin
        Insert(Src, Dest, lp);
        Result := vp + 1;
      end;
  else
    begin
      Insert(Src, Dest, lp);
      if lp > Length(Dest)
      then
        Result := lp
      else
        Result := lp + 1;
    end;
  end;
WriteLn(' Result=', Result);
end;

procedure VInsert(const Src:TUTF8String;var Dest:TUTF8String; vp:Integer; pDir:TDirection);
begin
  Insert(Src, Dest, LPos(Dest, vp, pDir));
end;

end.

