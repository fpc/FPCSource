program example16;

{ This program demonstrates the PropIsType function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;

begin
  O:=TMyTestObject.Create;
  Writeln('Property tests    : ');
  Write('PropIsType(O,BooleanField,tkBool)     : ');
  Writeln(PropIsType(O,'BooleanField',tkBool));
  Write('PropIsType(Class,BooleanField,tkBool) : ');
  Writeln(PropIsType(O.ClassType,'BooleanField',tkBool));
  Write('PropIsType(O,ByteField,tkString)      : ');
  Writeln(PropisType(O,'ByteField',tkString));
  Write('PropIsType(Class,ByteField,tkString)  : ');
  Writeln(PropIsType(O.ClassType,'ByteField',tkString));
  O.Free;
end.