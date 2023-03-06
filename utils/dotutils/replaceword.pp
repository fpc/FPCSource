program replaceword;

{$mode objfpc}
{$h+}
uses regexpr,sysutils, classes;

function ReplaceWord(aLine, aName, aFull: String): String;

var
  RE : TRegExpr;

begin
  RE:=TRegExpr.Create('\b'+aName+'\b');
  try
//    RE.ModifierI:=True;
    Result:=RE.Replace(aLine,aFull);
//    Writeln(aLine,': ',aName,' -> ',aFull,' = ',Result);
  finally
    RE.Free;
  end;
end;


var
  I,J : Integer;
  aMakeFile: TStrings;
  W1,W2,aFN : String;

begin
  W1:=ParamStr(1);
  W2:=ParamStr(2);
  aMakeFile:=TStringList.Create;
  try
    for I:=3 to ParamCount do
      begin
      aFN:=Paramstr(I);
      aMakeFile.LoadFromFile(aFN);
      aMakeFile.SaveToFile(aFn+'.bak');
      For J:=0 to aMakefile.Count-1 do
        aMakefile[J]:=ReplaceWord(aMakefile[J],W1,W2);
      aMakeFile.SaveToFile(AFN);
      end;
  finally
    aMakeFile.Free;
  end;
end.

