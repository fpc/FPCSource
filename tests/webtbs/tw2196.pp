{ Source provided for Free Pascal Bug Report 2196 }
{ Submitted by "Andrew Johnson" on  2002-10-22 }
{ e-mail: acjgenius@earthlink.net }
Program bugtest;

{$Mode ObjFPC}
{ $Define bug_workaround}
uses classes;

type
  TSomeClass = Class
  private
    FTestString : AnsiString;
  public
    Property  TestString : AnsiString read FTestString write FTestString;
  end;

var
  TestClass : TSomeClass;
begin
  TestClass := TSomeClass.Create;
  {$IfDef bug_workaround}
  TestClass.TestString := '' + TestClass.TestString + 'Whatever';
  {$Else}
  TestClass.TestString :=  TestClass.TestString + 'Whatever';
  {$EndIF}
  writeln(TestClass.TestString);
  TestClass.Free;
end.
