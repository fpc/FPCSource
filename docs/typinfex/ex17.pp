program example17;

{ This program demonstrates the PropType function }

{$mode objfpc}

uses rttiobj,typinfo;

Var
  O : TMyTestObject;

begin
  O:=TMyTestObject.Create;
  Writeln('Property tests    : ');
  Write('PropType(O,BooleanField)     : ');
  Writeln(TypeNames[PropType(O,'BooleanField')]);
  Write('PropType(Class,BooleanField) : ');
  Writeln(TypeNames[PropType(O.ClassType,'BooleanField')]);
  Write('PropType(O,ByteField)        : ');
  Writeln(TypeNames[PropType(O,'ByteField')]);
  Write('PropType(Class,ByteField)    : ');
  Writeln(TypeNames[PropType(O.ClassType,'ByteField')]);
  O.Free;
end.