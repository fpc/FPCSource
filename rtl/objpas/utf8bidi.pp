{
Author Mazen NEIFER
Licence LGPL
}
unit UTF8BIDI;

{$mode objfpc}{$H+}

interface

type
  TUCS32Char = Cardinal;
  TUTF8Char = String[3];
  TUTF8Str = UTF8String;
  TDirection=(
    drNONE,
    drRTL,
    drLTR
  );

function UnicodeToUtf8(aChar:TUCS32Char):TUTF8Char;
function UnicodeToUtf8(aChar:WideChar):TUTF8Char;
procedure insert(var uString:TUTF8Str; CharToInsert:TUTF8Char; var CursorPos:Integer);

implementation

function UnicodeToUtf8(aChar:TUCS32Char):TUTF8Char;
begin
  case aChar of
    0..$7f:
      begin
        Result[1]:=char(aChar);
        SetLength(Result,1);
      end;
    $80..$7ff:
      begin
        Result[1]:=char($c0 or (aChar shr 6));
        Result[2]:=char($80 or (aChar and $3f));
        SetLength(Result,2);
      end;
    else
      begin
        Result[1]:=char($e0 or (aChar shr 12));
        Result[2]:=char($80 or ((aChar shr 6)and $3f));
        Result[3]:=char($80 or (aChar and $3f));
        SetLength(Result,3);
      end;
  end;
end;

function UnicodeToUtf8(aChar:WideChar):TUTF8Char;
begin
  UnicodeToUtf8(Word(aChar));
end;

procedure insert(var uString:TUTF8Str; CharToInsert:TUTF8Char; var CursorPos:Integer);
var
{At beginning of the line we don't know which direction, thus the first
 character usually decides of paragrph direction}
  dir:TDirection;
  LeftCursorPos, RightCursorPos, InsertPos:Integer;
  uLen:Integer;
begin
  dir := drNONE;
  uLen := Length(uString);
  LeftCursorPos := 1;
  RightCursorPos := 1;
  InsertPos := 1;
  if(uLen > 0) then
    repeat
      case uString[InsertPos] of
        #32,'{','}','/'://Does not change direction, this is a neutral character;
          begin
            if(dir = drLTR) then
              Inc(RightCursorPos);
          end;
        #$d8,#$d9://Arabic
          begin
            dir := drRTL;
            Inc(InsertPos);//Consume control character
          end;
      else //Latin
        begin
          dir := drLTR;
          RightCursorPos := LeftCursorPos + 1;
        end;
      end;
      Inc(LeftCursorPos);
      Inc(InsertPos);
    until(InsertPos > uLen) or
         ((dir = drLTR) and (LeftCursorPos > CursorPos)) or
         ((dir = drRTL) and (RightCursorPos > CursorPos));
//WriteLn('uLen=',uLen,' InsertPos=',InsertPos,' CursorPos=',CursorPos,' LeftCursorPos=',LeftCursorPos,' RightCursorPos=',RightCursorPos);
  if(InsertPos > uLen)
  then begin
    if(CursorPos > LeftCursorPos) then begin
      Inc(InsertPos, CursorPos - LeftCursorPos);
      LeftCursorPos := CursorPos;
    end;
    Inc(LeftCursorPos);
    if(CursorPos > RightCursorPos) then
      if(dir = drLTR) then
        RightCursorPos := CursorPos;
    uString := uString  + StringOfChar(' ', InsertPos - uLen);
  end;
//WriteLn('CursorPos=',CursorPos,' LeftCursorPos=',LeftCursorPos,' RightCursorPos=',RightCursorPos);
  System.insert(CharToInsert, uString,InsertPos);
  case CharToInsert[1] of
    #32:
      CursorPos := LeftCursorPos;
    #$d8,#$d9:
      CursorPos := RightCursorPos;
    else
      CursorPos := LeftCursorPos;
  end;
//WriteLn('InsertPos=',InsertPos,' New CursorPos=',CursorPos);
end;
end.

