program testnullptr;

{$mode objfpc}
{$h+}

uses types;

Function TestAssign : String;

Var
  A: TObject;
  B: Pointer;

begin
  Result:='';
  A:=TObject(1);
  A:=nullptr;
  If Assigned(A) then
    Exit('Assignment should set to nil');
  B:=Pointer(1);
  B:=nullptr;
  If Assigned(B) then
    Exit('Assignment should set to nil');
end;

Function TestCompare : String;

begin
  Result:='';
  If not (nullptr = nil) then
    Exit('nullptr should compare to nil');
  If not (nullptr = TObject(nil)) then
    Exit('nullptr should compare to nil');
  If not (nil = nullptr) then
    Exit('nullptr should compare to nil');
  If not (TObject(nil) = nullptr) then
    Exit('nullptr should compare to nil');

  If nullptr <> nil then
    Exit('nullptr should compare to nil');
  If nullptr <> TObject(nil) then
    Exit('nullptr should compare to nil');
  If nil <> nullptr then
    Exit('nullptr should compare to nil');
  If TObject(nil) <> nullptr then
    Exit('nullptr should compare to nil');
end;

Procedure DoTest(aTest,aResult : String);

begin
  if aResult<>'' then
    begin
    writeln(aTest,' failed : ',aResult);
    Halt(1);
    end
  else
    Writeln(aTest,' OK.');
end;


begin
  DoTest('TestAssign',TestAssign);
  DoTest('TestCompare',TestCompare);
end.

