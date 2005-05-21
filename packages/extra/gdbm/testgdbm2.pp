{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of
    the Free Pascal development team

    Test strings interface to gdbm library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program testgdbm2;

{$mode objfpc}
{$h+}

uses sysutils,gdbm;

Var
  dbf : pgdbm_file;
  A,B : String;
  i : longint;

begin
  dbf:=gdbm_open('test.dat',512,GDBM_NEWDB,432,nil);
  If dbf=Nil then
    Writeln('Error when creating database');
  for I:=1 to 10 do
    begin
    A:=Format('data for string %d',[i]);
    B:=Format('string %d',[i]);
    if not gdbm_store(dbf,B,A,gdbm_insert) then
      Writeln('Error inserting data')
    else
      Writeln('Inserted string ',i)
    end;
  gdbm_firstkey(dbf,B);
  I:=0;
  While B<>'' do
    begin
    inc(i);
    A:=gdbm_fetch(dbf,B);
    writeln('Data for key ',i,' (',B,') : ',A);
    B:=gdbm_nextkey(dbf,B);
    end;
  gdbm_close(dbf);
end.
