{
    $Id: md5test.pp,v 1.2 2002/09/07 15:42:52 peter Exp $
    This file is part of the Free Pascal packages.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Tests the MD5 program.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program md5test;

{$h+}

uses md5;
var
  I: byte;

const
  Suite: array[1..7] of string = (
    '',
    'a',
    'abc',
    'message digest',
    'abcdefghijklmnopqrstuvwxyz',
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789',
    '12345678901234567890123456789012345678901234567890123456789012345678901234567890'
    );

begin
  Writeln('Executing RFC 1321 test suite ...');
  for I := 1 to 7 do
    Writeln('MD5 ("',Suite[i],'") = ',MD5Print(MD5String(Suite[I])));
  Writeln();
  Writeln('md5file (50)  : ',md5print(Md5File('md5test.pas',50)));
  Writeln('md5file (def) : ',md5print(Md5File('md5test.pas')));
end.
