{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by Karoly Balogh
    members of the Free Pascal development team.

    Command line parameter handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{ Generates correct argument array on startup }
procedure GenerateArgs;
begin
end;


{*****************************************************************************
                             ParamStr
*****************************************************************************}

{ number of args }
function ParamCount: LongInt;
begin
  ParamCount := argc - 1;
end;

{ argument number l }
function ParamStr(l: LongInt): string;
var
  s1: string;
begin
  ParamStr := '';
  if (l > 0) and (l + 1 <= argc) then
    ParamStr := StrPas(argv[l]);
end;
