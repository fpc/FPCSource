program tissurrogatepair2;

{$ifdef FPC}
  {$mode objfpc}
  {$H+}
  {$PACKENUM 1}
{$endif fpc} 

{$ifndef FPC}
  {$APPTYPE CONSOLE}    
{$endif}
  
uses     
  SysUtils,
  unicodedata,character;
    
{$ifndef FPC}
  type UnicodeChar = WideChar;   
{$endif}
    
procedure DoError(ACode : Integer; ACodePoint1, ACodePoint2 : Integer); overload;
begin
  WriteLn(
    'Error #',ACode,
    ' ; CodePoint1 = ',IntToHex(ACodePoint1,4),
    ' ; CodePoint2 = ',IntToHex(ACodePoint2,4)
  );
  Halt(Acode);
end;

var
  e, i , j: Integer;
  s : UnicodeString;
begin
  s := 'azerty12345';
  e := 1;
  for i := HIGH_SURROGATE_BEGIN to HIGH_SURROGATE_END do begin
    for j := LOW_SURROGATE_BEGIN to LOW_SURROGATE_END do begin
      s[3] := UnicodeChar(i);
      s[4] := UnicodeChar(j);
      if not TCharacter.IsSurrogatePair(s,3) then
        DoError(e,i,j);
    end;
  end;

  Inc(e);
  for i := Low(Word) to High(Word) do begin
    if (i < HIGH_SURROGATE_BEGIN) or (i > HIGH_SURROGATE_END) then begin
      for j := Low(Word) to High(Word) do begin
        if (j < LOW_SURROGATE_BEGIN) or (j > LOW_SURROGATE_END) then begin
          s[5] := UnicodeChar(i);
          s[6] := UnicodeChar(j);
          if TCharacter.IsSurrogatePair(s,5) then
            DoError(e,i,j);
        end;
      end;
    end;
  end;

  WriteLn('ok');
end.
