{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Generate a skeleton unit with namespaced name which defines FPC_DOTTEDUNITS and 
    includes the original non-dotted unit.
        
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program proxyunit;

uses StrUtils, SysUtils, Classes;

  Function MaybeExt(S : String) : String;
  begin
    Result:=S;
    if IndexText(ExtractFileExt(S),['.pas','.pp'])=-1 then
      Result:=Result+'.pp'
  end;

Function MaybeStripExt(S : String) : String;
  begin
    Result:=S;
    if IndexText(ExtractFileExt(S),['.pas','.pp'])<>-1 then
      Result:=ChangeFileExt(Result,'')
  end;

var
  aFile : TStrings;

begin
  if (ParamCount<>2) then
    begin
    Writeln('Usage: dottedunit nondottedunit');
    Writeln('Default extension is .pp and is appended if needed');
    Halt(1);
    end;
  aFile:=TStringList.Create;
  try
    aFile.Add('unit '+MaybeStripExt(ExtractFileName(ParamStr(1)))+';');
    aFile.Add('{$DEFINE FPC_DOTTEDUNITS}');
    aFile.Add('{$i '+MaybeExt(ParamStr(2))+'}');
    aFile.SaveToFile(MaybeExt(ParamStr(1)));
  finally
    aFile.Free;
  end;

end.

