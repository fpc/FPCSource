program tisnumber2;

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
  character;
    
{$ifndef FPC}
  type UnicodeChar = WideChar;   
{$endif} 

procedure DoError(ACode : Integer); overload;
begin
  WriteLn('Error #',ACode);
  Halt(Acode);
end;         
    
procedure DoError(ACode : Integer; ACodePoint : Integer); overload;
begin
  WriteLn('Error #',ACode,' ; CodePoint = ',IntToHex(ACodePoint,4));
  Halt(Acode);
end;          
    
procedure DoError(ACode : Integer; ACodePoint : UnicodeChar); overload;
begin
  WriteLn('Error #',ACode,' ; CodePoint = ',IntToHex(Ord(ACodePoint),4));
  Halt(Acode);
end;         

var
  e, i , k: Integer;
  strPrefix, uc : UnicodeString;
  locCharPos : Integer;
  d : Double;
begin
  strPrefix := '012345AZERT ';
  locCharPos := Length(strPrefix) + 1;
  e := 1;
  k := 0;
  for i := Low(Word) to High(Word) do begin
    uc := strPrefix + UnicodeChar(i) + strPrefix;
    if TCharacter.IsNumber(uc,locCharPos) then begin
      WriteLn('CodePoint = ',IntToHex(Ord(uc[locCharPos]),4), ' ; IsNumber = ',TCharacter.IsNumber(uc,locCharPos));
      Inc(k);
    end;
  end;
  WriteLn(k, ' numbers',sLineBreak);

  Inc(e);
  for i := 0 to 9 do begin
    uc := strPrefix + IntToStr(i) + strPrefix;
    if not TCharacter.IsNumber(uc,locCharPos) then
      DoError(e,uc[locCharPos]);
  end;

  WriteLn('ok');
end.
