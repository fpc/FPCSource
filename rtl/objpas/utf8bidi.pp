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
function LPos(const UTF8Str:TUTF8String; vp:Cardinal; pDir:TDirection):Cardinal;
{Returns character at a given logical position according to paragraph direction}
function LCharOf(UTF8Str:TUTF8String; lp:Cardinal):TUTF8Char;
{****************************Visual aspects************************************}
{Returns the number of visual characters}
function VLength(const UTF8Str:TUTF8String):Cardinal;
function VLength(p: PChar; Count:Cardinal):Cardinal;
{Converts a logical position to a visual position}
function VPos(const UTF8Str:TUTF8String; lp:Cardinal; pDir, cDir:TDirection):Cardinal;
function VPos(UTF8Char:PChar; Len:integer; BytePos:integer):Cardinal;
{Returns character at a given visual position according to paragraph direction}
function VCharOf(UTF8Str:TUTF8String; vp:Cardinal; dir:TDirection):TUTF8Char;

implementation

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

function LPos(const UTF8Str:TUTF8String; vp:Cardinal; pDir:TDirection):Cardinal;
var
{At beginning of the line we don't know which direction, thus the first
 character usually decides of paragrph direction}
  LeftCursorPos, RightCursorPos:Integer;
  uLen:Integer;
begin
  uLen := Length(UTF8Str);
  LeftCursorPos := 1;
  RightCursorPos := 1;
  Result := 1;
  if(uLen > 0) then
    repeat
      case UTF8Str[Result] of
        #32,'{','}','/'://Does not change direction, this is a neutral character;
          begin
            if(pDir = drLTR) then
              Inc(RightCursorPos);
          end;
        #$d8,#$d9://Arabic
          begin
            pDir := drRTL;
            Inc(Result);//Consume control character
          end;
      else //Latin
        begin
          pDir := drLTR;
          RightCursorPos := LeftCursorPos + 1;
        end;
      end;
      Inc(LeftCursorPos);
      Inc(Result);
    until(Result > uLen) or
         ((pDir = drLTR) and (LeftCursorPos > vp)) or
         ((pDir = drRTL) and (RightCursorPos > vp));
//WriteLn('uLen=',uLen,' Result=',Result,' CursorPos=',CursorPos,' LeftCursorPos=',LeftCursorPos,' RightCursorPos=',RightCursorPos);
  if(Result > uLen)
  then begin
    if(vp > LeftCursorPos) then begin
      Inc(Result, vp - LeftCursorPos);
      LeftCursorPos := vp;
    end;
    Inc(LeftCursorPos);
    if(vp > RightCursorPos) then
      if(pDir = drLTR) then
        RightCursorPos := vp;
  end;
//WriteLn('CursorPos=',CursorPos,' LeftCursorPos=',LeftCursorPos,' RightCursorPos=',RightCursorPos);
  Result := Result;
end;

function LCharOf(UTF8Str:TUTF8String; lp:Cardinal):TUTF8Char;
begin
  while(lp > 0) and (UTF8Str[lp] > #128) do
    Dec(lp);
  if lp = 0
  then
    Exit('');
  Move(Result, UTF8Str[lp], SizeOf(Result));
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
  VLength:=0;
  while (Count>0) do begin
    inc(Result);
    CharLen:=ComputeCharLength(p);
    inc(p,CharLen);
    dec(Count,CharLen);
  end;
end;

function VPos(const UTF8Str:TUTF8String; lp:Cardinal; pDir, cDir:TDirection):Cardinal;
var
{At beginning of the line we don't know which direction, thus the first
 character usually decides of paragrph direction}
  LeftCursorPos, RightCursorPos:Integer;
  uLen:Integer;
begin
  uLen := Length(UTF8Str);
  LeftCursorPos := 1;
  RightCursorPos := 1;
  Result := 1;
  if(uLen > 0) then
    repeat
      case UTF8Str[Result] of
        #32,'{','}','/'://Does not change direction, this is a neutral character;
          begin
            if(pDir = drLTR) then
              Inc(RightCursorPos);
          end;
        #$d8,#$d9://Arabic
          begin
            pDir := drRTL;
            Inc(Result);//Consume control character
          end;
      else //Latin
        begin
          pDir := drLTR;
          RightCursorPos := LeftCursorPos + 1;
        end;
      end;
      Inc(LeftCursorPos);
      Inc(Result);
    until(Result > uLen) or
         ((pDir = drLTR) and (LeftCursorPos > lp)) or
         ((pDir = drRTL) and (RightCursorPos > lp));
//WriteLn('uLen=',uLen,' Result=',Result,' CursorPos=',CursorPos,' LeftCursorPos=',LeftCursorPos,' RightCursorPos=',RightCursorPos);
  if(Result > uLen)
  then begin
    if(lp > LeftCursorPos) then begin
      Inc(Result, lp - LeftCursorPos);
      LeftCursorPos := lp;
    end;
    Inc(LeftCursorPos);
    if(lp > RightCursorPos) then
      if(pDir = drLTR) then
        RightCursorPos := lp;
  end;
//WriteLn('CursorPos=',CursorPos,' LeftCursorPos=',LeftCursorPos,' RightCursorPos=',RightCursorPos);
  Result := Result;
{  case dir of
    #32:
      CursorPos := LeftCursorPos;
    #$d8,#$d9:
      CursorPos := RightCursorPos;
    else
      CursorPos := LeftCursorPos;
  end;}
//WriteLn('Result=',Result,' New CursorPos=',CursorPos);
end;

function VPos(UTF8Char:PChar; Len:integer; BytePos:integer):Cardinal;
begin
end;


function VCharOf(UTF8Str:TUTF8String; vp:Cardinal; dir:TDirection):TUTF8Char;
var
  CharLen: LongInt;
begin
  Result:=LCharOf(UTF8Str,LPos(UTF8Str, vp, dir));
end;

end.

