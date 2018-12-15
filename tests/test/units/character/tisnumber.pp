program tisnumber;

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
  unicodedata, character;
    
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
  uc : UnicodeChar;
  d : Double;
begin  
  e := 1;
  k := 0;
  for i := Low(Word) to High(Word) do begin
    { Skip all surrogate values }
    if (i>=HIGH_SURROGATE_BEGIN) and (i<=LOW_SURROGATE_END) then continue;
    uc := UnicodeChar(i);
    if TCharacter.IsNumber(uc) then begin
      WriteLn('CodePoint = ',IntToHex(Ord(uc),4), ' ; IsNumber = ',TCharacter.IsNumber(uc));
      Inc(k);
    end;
  end;
  WriteLn(k, ' numbers',sLineBreak);

  Inc(e);
  for i := 0 to 9 do begin
    uc := IntToStr(i)[1];
    if not TCharacter.IsNumber(uc) then
      DoError(e,uc);
  end;

  WriteLn('ok');
end.
