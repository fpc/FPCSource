{$ifdef fpc}
{$mode delphi}
{$endif}

{$r-}
uses
  SysUtils, Classes, TypInfo, Variants;

type
  TBla = class(TPersistent)
  private
    fustr: unicodestring;
  published
    property ustr: unicodestring read fustr write fustr;
  end;

var
  b: tbla;
  u: unicodestring;
begin
  b:=tbla.create;
  SetPropValue(b, 'ustr', 'abc');
  if (b.ustr<>'abc') then
    halt(1);
  u:=getpropvalue(b,'ustr');
  if (u<>'abc') then
    halt(2);
end.

