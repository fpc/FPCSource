{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  classes, typinfo;

type
  tstrtype = shortstring;
  TSomeType = class (TPersistent)
  private
    FName: tstrtype;
    procedure SetName(const AValue: tstrtype);
  published
    property Name: tstrtype read FName write SetName;
  end;


procedure tsometype.setname(const avalue: tstrtype);
begin
  fname:=avalue;
end;

var
  c: tsometype;
begin
  c:=tsometype.create;
  SetStrProp(c,'Name','This is a test of the emergency broadcast system');
  if (c.name<>'This is a test of the emergency broadcast system') then
    begin
      writeln('"',c.name,'"');
      halt(1);
    end;
  if getstrprop(c,'Name')<>'This is a test of the emergency broadcast system' then
    halt(2);
end.
