program example1;

{ This program demonstrates the GetOrdProp function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;

begin
  O:=TMyTestObject.Create;
  Writeln('Boolean property    : ');
  Writeln('Value               : ',O.BooleanField);
  Writeln('Ord(Value)          : ',Ord(O.BooleanField));
  Writeln('Get (name)          : ',GetOrdProp(O,'BooleanField'));
  PI:=GetPropInfo(O,'BooleanField');
  Writeln('Get (propinfo)      : ',GetOrdProp(O,PI));
  SetOrdProp(O,'BooleanField',Ord(False));
  Writeln('Set (name,false)    : ',O.BooleanField);
  SetOrdProp(O,PI,Ord(True));
  Writeln('Set (propinfo,true) : ',O.BooleanField);
  O.Free;
end.