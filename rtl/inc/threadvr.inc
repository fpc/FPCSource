{
    $Id: threadvr.inc,v 1.5 2005/02/14 17:13:29 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt
    member of the Free Pascal development team

    Threadvar support, platform independent part

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Threadvar support
*****************************************************************************}

{$ifdef HASTHREADVAR}

type
  pltvInitEntry = ^ltvInitEntry;
  ltvInitEntry = packed record
     varaddr : pdword;
     size    : longint;
  end;

  TltvInitTablesTable = packed record
    count  : dword;
    tables : packed array [1..32767] of pltvInitEntry;
  end;

var
  ThreadvarTablesTable : TltvInitTablesTable; external name 'FPC_THREADVARTABLES';

procedure init_unit_threadvars (tableEntry : pltvInitEntry);
begin
  while tableEntry^.varaddr <> nil do
   begin
     CurrentTM.InitThreadvar (tableEntry^.varaddr^, tableEntry^.size);
     inc (pchar (tableEntry), sizeof (tableEntry^));
   end;
end;


procedure init_all_unit_threadvars;
var
  i : integer;
begin
{$ifdef DEBUG_MT}
  WriteLn ('init_all_unit_threadvars (',ThreadvarTablesTable.count,') units');
{$endif}
  for i := 1 to ThreadvarTablesTable.count do
    init_unit_threadvars (ThreadvarTablesTable.tables[i]);
end;


procedure copy_unit_threadvars (tableEntry : pltvInitEntry);
var
  oldp,
  newp : pointer;
begin
  while tableEntry^.varaddr <> nil do
   begin
     newp:=CurrentTM.RelocateThreadVar(tableEntry^.varaddr^);
     oldp:=pointer(pchar(tableEntry^.varaddr)+sizeof(pointer));
     move(oldp^,newp^,tableEntry^.size);
     inc (pchar (tableEntry), sizeof (tableEntry^));
   end;
end;


procedure copy_all_unit_threadvars;
var
  i : integer;
begin
{$ifdef DEBUG_MT}
  WriteLn ('copy_all_unit_threadvars (',ThreadvarTablesTable.count,') units');
{$endif}
  for i := 1 to ThreadvarTablesTable.count do
    copy_unit_threadvars (ThreadvarTablesTable.tables[i]);
end;

procedure InitThreadVars(RelocProc : Pointer);

begin
   { initialize threadvars }
   init_all_unit_threadvars;
   { allocate mem for main thread threadvars }
   CurrentTM.AllocateThreadVars;
   { copy main thread threadvars }
   copy_all_unit_threadvars;
   { install threadvar handler }
   fpc_threadvar_relocate_proc:=RelocProc;
end;

{$endif HASTHREADVAR}

{
  $Log: threadvr.inc,v $
  Revision 1.5  2005/02/14 17:13:29  peter
    * truncate log

  Revision 1.4  2005/02/05 23:49:49  florian
    * threadvars on x86_64 fixed

}
