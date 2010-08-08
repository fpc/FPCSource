{$ifdef fpc}{$mode delphi}{$endif}
{$ifdef MSWindows}{$apptype console}{$endif}
uses
  SysUtils;

type
  PByteArray=^TByteArray; 
var
  g : array [byte] of byte;

function GetArray: PByteArray;
begin
  Result:=@g[0];
end;

var
  p : PByteArray;
begin
  g[0]:=111;
  g[1]:=221;
  g[2]:=252;
  
  p:=PByteArray(@GetArray[0]);
  if p[0]<>111 then
    halt(1);

  p:=PByteArray(@((GetArray))[1]);
  if p[0]<>221 then
    halt(2);

  p:=PByteArray(@(GetArray[2])); 
  if p[0]<>252 then
    halt(3);
end.
