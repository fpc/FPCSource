{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Linux version of custom app object routines.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Procedure SysGetEnvironmentList(List : TStrings;NamesOnly : Boolean);

Var
  P : PPChar;
  S : String;
  I : Integer;

begin
  List.Clear;
  P:=EnvP;
  if (P<>Nil) then
    While (P^<>Nil) do
      begin
      S:=StrPas(P^);
      If NamesOnly then
        begin
        I:=Pos('=',S);
        If (I>1) then
          S:=Copy(S,1,I-1);
        end;
      List.Add(S);
      Inc(P);
    end;
end;

