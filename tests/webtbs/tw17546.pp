{ %opt=-gh }

{$MODE OBJFPC}

program test02;

{$STATIC ON}

type

  TDummyClass = class
    IdName: AnsiString; static;
  end;

var

  o: TDummyClass;

begin
  HaltOnNotReleased := true;
  TDummyClass.IdName := 'Test';
  TDummyClass.IdName := TDummyClass.IdName + 'a';
  o := TDummyClass.Create;
  WriteLn('Here we go');
  o.Free;
  WriteLn('We did it!');
end.

