unit wasmbindebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wasmbin, lebutils;

procedure DumpTypes(sr: TStream);

implementation

procedure DumpTypes(sr: TStream);
var
  ar : TFuncTypeArray;
  i  : integer;
  j  : integer;
begin
  ReadFuncTypesArray(sr, ar);
  for i:=0 to length(ar.funTypes)-1 do begin
    write('#',i);
    writeln;
    write('  params:');
    for j:=0 to length(ar.funTypes[i].param)-1 do
      write(' ', ValTypeToStr(ar.funTypes[i].param[j]));
    writeln;
    write('  result:');
    for j:=0 to length(ar.funTypes[i].result)-1 do
      write(' ', ValTypeToStr(ar.funTypes[i].result[j]));
    writeln;
  end;
end;

end.

