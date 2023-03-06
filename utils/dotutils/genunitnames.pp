{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Read a list of unit file transformations and generate a list of unit names for use in a Makefile.
    The output consists of a XYZUNIT variable for each unit in the file list, twice: once dotted, once not dotted.
    The variables can be used in target definitions and dependency lists.
        
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program genunitnames;

uses sysutils, classes, types, namespacetool, prefixer;

var
  L,Dotted,NDotted : TStrings;
  aLine, aNewUnit, aFN,aRule,aLastDir,aLastRule : String;
  aOpts : TStringDynArray;

begin
  Dotted:=Nil;
  NDotted:=nil;
  L:=TStringList.Create;
  try
    Dotted:=TStringList.Create;
    NDotted:=TStringList.Create;
    L.LoadFromFile(paramstr(1));
    for aLine in L do
      begin
      TNamespaceTool.SplitRuleLine(aLine,aFN,aRule,aLastDir,aLastRule,aOPts);
      aNewUnit:=TPrefixer.ApplyRule(aFN,aRule,aRule<>'');
      NDotted.Add(UpperCase(aFN)+'UNIT='+aFn);
      Dotted.Add(UpperCase(aFN)+'UNIT='+aNewUnit);
      end;

    Writeln('ifdef FPC_DOTTEDUNITS');
    For aLine in Dotted do
      Writeln(aLine);
    Writeln('else');
    For aLine in NDotted do
      Writeln(aLine);
    Writeln('endif');
  finally
    Dotted.Free;
    NDotted.Free;
    L.Free;
  end;

end.

