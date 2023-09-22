{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Take a uses clause from standard input, 
    and replace it with a conditional uses clause that allows dotted and non-dotted units.
    To be used in shell scripts or to be invoked from an editor 
    that allows you to filter a selection through a command.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program fixuses;

uses classes, prefixer;

Var
  aIn,aOut : TStrings;
  P : TPrefixer;
  S : String;

begin
  if ParamStr(1)='' then
    begin
    Writeln('Error : need name of known namespaces');
    Halt(1);
    end;
  P:=nil;
  aOut:=nil;
  aIn:=TStringList.Create;
  try
    aOut:=TStringList.Create;
    While not EOF do
      begin
      ReadLn(S);
      aIn.Add(S);
      end;
    P:=TPrefixer.Create(Nil);
    P.KnownNameSpaces.LoadFromFile(ParamStr(1));
    P.ReworkUses(aIn,aOut);
    For S in aout do
      Writeln(S);
  finally
    aIn.Free;
    aOut.Free;
    P.Free;
  end;
end.

