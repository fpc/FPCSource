{ Source provided for Free Pascal Bug Report 2196 }
{ Submitted by "Andrew Johnson" on  2002-10-22 }
{ e-mail: acjgenius@earthlink.net }
Program bugtest;

{$Mode ObjFPC}
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
  TestClass.TestString :=  TestClass.TestString + 'Whatever';
  writeln(TestClass.TestString);
  if TestClass.TestString<>'Whatever' then
   begin
     writeln('Error!');
     halt(1);
   end;
  TestClass.Free;
end.
