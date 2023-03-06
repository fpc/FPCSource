{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Utility to add a statement to add a namespace to a fpmake program file for FPC packages.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program addnamespacetofpmake;

uses classes, sysutils;

const
  namespacelist = 'namespaces.lst';

Function HandleFile(const aFileName : string) : boolean;

Var
  aFile : TStringList;
  I : Integer;
  aLine : string;

begin
  Result:=False;
  aFile:=TStringList.Create;
  try
    aFile.LoadFromFile(aFileName);
    i:=aFile.Count-1;
    while (I>=0) and not Result do
      begin
      aLine:=aFile[i];
      if pos('{$ifndef ALLPACKAGES}',aLine)>0 then
        if Pos('run',Lowercase(aFile[i+1]))>0 then
          begin
          aFile.Insert(I,'');
          aFile.Insert(I,Format('    P.NamespaceMap:=''%s'';',[namespacelist]));
          aFile.Insert(I,'');
          Result:=True;
          end;
      Dec(I);
      end;
    if Result then
      aFile.SaveToFile(aFileName);
  finally
    aFile.Free;
  end;
end;

var
  I : Integer;

begin
  for I:=1 to ParamCount do
    if not handleFile(Paramstr(i)) then
      Writeln('Could not modify file: ',Paramstr(i));
end.

