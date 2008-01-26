{
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of
    the Free Pascal development team

    Test raw gdbm header translations.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testgdbm;

{$mode objfpc}
{$h+}

uses sysutils,gdbm;

Var
  dbf : pgdbm_file;
  Key,Data : TDatum;
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
    Data.dptr:=Pchar(A);
    Data.dsize:=length(A);
    key.dptr:=pchar(B);
    key.dsize:=length(B);
    if gdbm_store(dbf,key,data,gdbm_insert)<>0 then
      Writeln('Error inserting data')
    else
      Writeln('Inserted string ',i)
    end;
  key:=gdbm_firstkey(dbf);
  I:=0;
  While key.dptr<>nil do
    begin
    inc(i);
    data:=gdbm_fetch(dbf,key);
    writeln('Data for key ',i,' (',key.dptr,') : ',data.dptr);
    key:=gdbm_nextkey(dbf,key);
    end;
  gdbm_close(dbf);
end.
