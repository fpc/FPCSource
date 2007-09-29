
{$mode objfpc}

program TestGetSetProp;
{$APPTYPE CONSOLE}{$PACKSET 1}

uses TypInfo;

{$M+}
type
  TEnum = (ckNormal, ckBusiness, ckVip, ckCorporate);
  TSet = set of TEnum;
  TClient = class
  private
    _Num: byte; // Works if Integer
    _St: TSet;
  published
    property Num: byte read _Num write _Num; // Works if Integer
    property St: TSet read _St write _St;
  end;

var
  C : TClient;
  V : TSet;
begin
  C := TClient.Create;
  C.Num := 2;
  C.St := [ckVip, ckNormal]; // the numeric representation is 5 (on little endian systems)
  V := C.St;
  writeln(sizeof(V), ' ', byte(V)); // It's OK
  writeln(sizeof(C.St), ' ', byte(C.St)); // It's OK
{$ifdef FPC_LITTLE_ENDIAN}
  if GetOrdProp(C, 'St')<>5 then
{$else}
  if GetOrdProp(C, 'St')<>160 then
{$endif}
    halt(1);
  if GetSetProp(C, 'St')<>'ckNormal,ckVip' then
    halt(1);
  writeln('ok');
end.
