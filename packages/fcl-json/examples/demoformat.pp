{$mode objfpc}
{$h+}
program demoformat;

uses fpjson;

var
  O : TJSONObject;
  A : TJSONArray;  
begin  
  O:=TJSONObject.Create(['a',1,'b','two','three',TJSONObject.Create(['x',10,'y',20])]);
  Writeln (O.FormatJSon);
  Writeln (O.FormatJSon([foDonotQuoteMembers,foUseTabChar],1));
  Writeln (O.FormatJSon([foSingleLineObject,foUseTabChar],1));
  Writeln (O.asJSON);
  A:=TJSONArray.Create([1,2,'a',TJSONObject.Create(['x',10,'y',20])]);
  Writeln (A.FormatJSon());
  Writeln (A.FormatJSON([foSinglelineArray],2));
  Writeln (A.FormatJSON([foSinglelineArray,foSingleLineObject],2));
  Writeln (A.asJSON);
end.  