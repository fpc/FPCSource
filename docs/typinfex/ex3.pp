program example3;

{ This program demonstrates the GetStrProp function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;

begin
  O:=TMyTestObject.Create;
  PI:=GetPropInfo(O,'AnsiStringField');
  Writeln('String property : ');
  Writeln('Value                   : ',O.AnsiStringField);
  Writeln('Get (name)              : ',GetStrProp(O,'AnsiStringField'));
  Writeln('Get (propinfo)          : ',GetStrProp(O,PI));
  SetStrProp(O,'AnsiStringField','First');
  Writeln('Set (name,''First'')      : ',O.AnsiStringField);
  SetStrProp(O,PI,'Second');
  Writeln('Set (propinfo,''Second'') : ',O.AnsiStringField);
  O.Free;
end.