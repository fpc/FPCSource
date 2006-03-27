{ Source provided for Free Pascal Bug Report 4635 }
{ Submitted by "Ales Katona" on  2005-12-23 }
{ e-mail: almindor@gmail.com }
program p1;

{$mode objfpc}{$H+}

type
  TTestEnum = (Enum1, Enum2);

  TTest = class
   protected
    FArray: array[TTestEnum] of Boolean;
    procedure SetTestB(const Value: Boolean);
   public
    property TestB: Boolean read FArray[Enum1] write SetTestB;
  end;

procedure TTest.SetTestB(const Value: Boolean);
begin
  FArray[Enum1]:=Value;
end;

var
  t1: TTest;
begin
  t1:=TTest.Create;
  t1.TestB:=true;
  Writeln(t1.TestB); // it doesn't compile here, but if you comment this line it works
  if not t1.TestB then
    halt;
  t1.Free;
end.
