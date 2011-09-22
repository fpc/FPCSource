program tfpwidestring;

{$ifdef FPC}
  {$mode objfpc}
  {$H+}
{$endif fpc} 

{$ifndef FPC}
  {$APPTYPE CONSOLE}    
{$endif}
  
uses     
  SysUtils,
  character, fpwidestring;
    
{$ifndef FPC}
  type UnicodeChar = WideChar;   
{$endif} 

var
  e, i, j : Integer;
  uc : UnicodeChar;
begin  
  e := 1;

  WriteLn('ok');
end.
