{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
  globala: byte;
  globalw: word;
  globaldw: longword;
  globalqw: qword;
  ga, gb, gc: byte;
  gwa, gwb, gwc: word;

procedure Test6502;
begin
  globala := 5;
  globalw := $1234;
  globaldw := $12345678;
  globalqw := $123456789ABCDEF0;
  ga := gb + gc;
  ga := gb - gc;
  ga := gb and gc;
  ga := gb or gc;
  ga := gb xor gc;
  ga := not gb;
  ga := -gb;
  gwa := -gwb;
end;
