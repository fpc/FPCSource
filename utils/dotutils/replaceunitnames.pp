{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Replace hardcoded unit names xyz in a makefile by a variable XYZUNIT.
    (see genunitnames for how to create the variables)
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program replaceunitnames;

uses regexpr,sysutils, classes, types, namespacetool, prefixer;

function ReplaceWord(aLine, aName, aFull: String): String;

var
  RE : TRegExpr;

begin
  RE:=TRegExpr.Create('\b'+aName+'\b');
  try
    RE.ModifierI:=True;
    Result:=RE.Replace(aLine,aFull);
//    Writeln(aLine,': ',aName,' -> ',aFull,' = ',Result);
  finally
    RE.Free;
  end;
end;


function ReplaceUnits(const aLine: string; aUnitNames: TStrings): String;

Var
  res,aName,aFull : String;

begin
  Res:=aLine;
  for aName in aUnitNames do
    begin
    aFull:='$('+UpperCase(aName)+'UNIT)';
    Res:=ReplaceWord(Res,aName,aFull);
    end;
  Result:=Res;
end;


var
  i : Integer;
  L,aNames,aMakeFile: TStrings;
  aFN,aRule : String;

begin
  aNames:=Nil;
  aMakeFile:=nil;
  L:=TStringList.Create;
  try
    aMakeFile:=TStringList.Create;
    aNames:=TStringList.Create;
    L.LoadFromFile(paramstr(1));
    for I:=0 to L.Count-1 do
      begin
      L.GetNameValue(I,aFN,aRule);
      aNames.Add(aFN);
      end;
    aMakeFile.LoadFromFile(Paramstr(2));
    aMakeFile.SaveToFile(Paramstr(2)+'.bak');
    For I:=0 to aMakefile.Count-1 do
      aMakefile[I]:=ReplaceUnits(aMakefile[I],aNames);
    aMakeFile.SaveToFile(ParamStr(2)+'.new');
  finally
    aMakeFile.Free;
    aNames.Free;
    L.Free;
  end;
end.

