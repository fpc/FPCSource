{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Linux version of custom app object routines.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ taken from dos unit }

type  
  PPchar = ^Pchar;

function envs:PPchar;assembler;

asm
    movl envp,%eax
end ['EAX'];

function envcount:longint;assembler;
asm
    movl envc,%eax
end ['EAX'];

function envstr(index : longint) : string;

var hp:Pchar;

begin
  if (index<=0) or (index>envcount) then
    begin
    envstr:='';
    exit;
    end;
  hp:=envs[index-1];
  envstr:=strpas(hp);
end;

                                                            

Procedure SysGetEnvironmentList(List : TStrings;NamesOnly : Boolean);

Var
  S : String;  
  J,I : Integer;
  
begin
  List.Clear;
  For J:=1 to envcount-1 do
    begin
    S:=Envsttr(J);
    If NamesOnly then
      begin
      I:=Pos('=',S);
      If (I>1) then
        S:=Copy(S,1,I-1);
      end;
    List.Add(S);
    end;
end;
