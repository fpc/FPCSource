program example4;

{ This program demonstrates the GetFloatProp function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;
  PI : PPropInfo;

begin
  O:=TMyTestObject.Create;
  Writeln('Real property : ');
  PI:=GetPropInfo(O,'RealField');
  Writeln('Value            : ',O.RealField);
  Writeln('Get (name)       : ',GetFloatProp(O,'RealField'));
  Writeln('Get (propinfo)   : ',GetFloatProp(O,PI));
  SetFloatProp(O,'RealField',system.Pi);
  Writeln('Set (name,pi)    : ',O.RealField);
  SetFloatProp(O,PI,exp(1));
  Writeln('Set (propinfo,e) : ',O.RealField);
  Writeln('Extended property : ');
  PI:=GetPropInfo(O,'ExtendedField');
  Writeln('Value            : ',O.ExtendedField);
  Writeln('Get (name)       : ',GetFloatProp(O,'ExtendedField'));
  Writeln('Get (propinfo)   : ',GetFloatProp(O,PI));
  SetFloatProp(O,'ExtendedField',system.Pi);
  Writeln('Set (name,pi)    : ',O.ExtendedField);
  SetFloatProp(O,PI,exp(1));
  Writeln('Set (propinfo,e) : ',O.ExtendedField);
  O.Free;
end.