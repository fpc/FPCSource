{
    This file is part of Free Pascal for Netware.
    Copyright (c) 1999-2002 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Demonstrates the use of the check-function provided by the RTL
 **********************************************************************}

{$MODE OBJFPC}
{$Description Demo CheckFunction for FreePascal Netware-RTL}
{$Version 1.0.0}
{$Copyright (c) 2002 the FreePascal development team}


{ using crt automatically calls _SetAutoScreenDestructionMode (TRUE);
  so no "Press any key to close screeen" will be shown by netware }
uses crt;

var first : boolean = true;

procedure checkfunction (var res : longint);
begin
  if first then
  begin
    ConsolePrintf (#13'It is unsafe to unload the nlm'#13#10);
    res := 1;
  end;
  first := false;
end;

begin
  WriteLn ('Press any key to unload nlm or unload via unload command');
  WriteLn ('The first unload should show a message that it is unsafe');
  WriteLn ('to unload the NLM, the second attempt should unload the');
  WriteLn ('NLM without a message.');
  System.NetwareCheckFunction := @checkfunction;
  ReadKey;
end.
