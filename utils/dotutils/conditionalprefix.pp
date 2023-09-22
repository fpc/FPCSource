{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Take a unit name and add a conditional define for a namespace.
    To be used in shell scripts or to be invoked from an editor 
    that allows you to filter a selection through a command
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program conditionalprefix;

Var
  S : AnsiString;

begin
  Readln(S);
  if S<>'' then
  S[1]:=UpCase(S[1]);
  S:='{$IFDEF FPC_DOTTEDUNITS}'+ParamStr(1)+'.{$ENDIF}'+S;
  Write(S);
end.

