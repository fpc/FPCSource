{$MODE OBJFPC} { -*- text -*- }
program tests;

type
   generic TTest <PTest> = class
     FPointer: PTest;
     procedure Foo();
   end;

procedure TTest.Foo();
var
   Result: Boolean;
begin
   Result := FPointer = nil;
end;

type
  TPointerTest = specialize TTest <Pointer>;

begin
end.
