{
  Program to demonstrate UseUnicodeWordDetection property. 
  Run this program as
  testwd
  testwd 1
  to see the difference  
}
{$mode objfpc}
{$h+}
uses cwstring,uregexpr;

Function ReplaceRegExpr(ARegExpr, AInputStr, AReplaceStr : Unicodestring) : string;

begin
  with TRegExpr.Create do
    try
      UseUnicodeWordDetection:=ParamStr(1)='1'; 
      Expression := ARegExpr;
      Result:=Replace (AInputStr, AReplaceStr, True);
    finally
      Free;
    end;
end;

begin    
  Writeln(ReplaceRegExpr('\w+', UTF8Decode('test слово ŕáćéí ϸϬϛ ュユョ'), '<$0>'));
end.