{
    Copyright (c) 1999-2000 by Pavel Stingl <stingp1.eti@mail.cez.cz>


    Test program for OraOCI units

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program test01;

uses
  oraclew,
  oci,oratypes;

{$H+}

{
    Constants user, pass & tnsc you must set by hand to values,
    which you prefer.

    user = username
    pass = password
    tnsc = TNS connect string
 **********************************************************************}

const
  user = 'every';
  pass = 'tisova';
  tnsc = 'etil.world';

var
  x : integer;
  p : integer;
begin
  OraInit;
  OraLogin(user,pass,tnsc);
  OraSQLExec('select sysdate from sys.dual');
  writeln(OraGetFieldName(1):20);
  p := OraGetFieldCount;
  for x := 1 to p do
  write(OraGetFieldName(x), ';');
  WriteLn;
  while OraNext do
  begin
    for x := 1 to p do
      write(OraGetFieldAsString(x), ';');
    writeln;
  end;

  OraLogout;
  OraFin;
end.
