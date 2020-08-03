unit wasmbindebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, wasmbin, lebutils;

procedure DumpTypes(sr: TStream);

procedure ReadFuncTypesArray(src: TStream; var arr: TFuncTypeArray);
procedure ReadFuncType(src: TStream; var ft: TFuncType);

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

procedure ReadFuncType(src: TStream; var ft: TFuncType);
var
  c: integer;
begin
  // vector of t1
  c:=ReadU(src);
  SetLength(ft.param, c);
  src.Read(ft.param[0], c);

  // vector of t2
  c:=ReadU(src);
  SetLength(ft.result, c);
  src.Read(ft.result[0], c);
end;

procedure ReadFuncTypesArray(src: TStream; var arr: TFuncTypeArray);
var
  cnt : integer;
  i   : Integer;
begin
  cnt := ReadU(src);
  SetLength(arr.funTypes, cnt);
  for i:=0 to cnt-1 do begin
    if src.ReadByte = func_type then
      ReadFuncType(src, arr.funTypes[i]);
  end;
end;


end.

