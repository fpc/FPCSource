{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Tool to lowercase values in a Name=Value lisst.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

uses sysutils, classes;

Var
  I : Integer;
  N,V : String;


begin
  With TStringList.Create do
    try
      LoadFromFile(ParamStr(1));
      For I:=0 to Count-1 do
        begin
        GetNameValue(i,N,V);
        V:=LowerCase(V);
        Strings[i]:=N+'='+V;
        end;
      SaveToFile(ParamStr(1));
    Finally
      Free;
    end;
end.
