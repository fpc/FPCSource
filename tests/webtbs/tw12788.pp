{$packset 1}
program t;
{$mode objfpc}{$h+}

uses typinfo;

type
  tsmall = 0..11;
  tsmallset = set of tsmall;

  tbig = 25..200;
  tbigset_ = set of tbig;

var
  ti: PTypeInfo;
  tdata: PTypeData;
begin
  ti := typeinfo(tsmallset);
  tdata := GetTypeData(ti);
  writeln(ord(tdata^.OrdType));
  if tdata^.CompType = typeinfo(tsmall) then
    writeln('small ok')
  else
    halt(1);

  ti := typeinfo(tbigset_);
  tdata := GetTypeData(ti);
  writeln(ord(tdata^.OrdType));
  if tdata^.CompType = typeinfo(tbig) then
    writeln('big ok')
  else
    halt(2);
end.
