{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    OS/2 version of custom app object routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ taken from dos unit }

function EnvStr (Index: longint): string;

var HP: PChar;

begin
    if (Index <= 0) or (Index > EnvC) then
        begin
            EnvStr := '';
            exit;
        end;
    HP := EnvP [Pred (Index)];
    EnvStr := StrPas (HP);
end;


procedure SysGetEnvironmentList(List : TStrings;NamesOnly : Boolean);

Var
  S : String;
  J,I : Integer;

begin
  List.Clear;
  For J := 1 to Pred (EnvC) do
    begin
    S:=EnvStr(J);
    If NamesOnly then
      begin
      I:=Pos('=',S);
      If (I>1) then
        S:=Copy(S,1,I-1);
      end;
    List.Add(S);
    end;
end;
