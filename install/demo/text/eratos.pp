{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Florian Klaempfl

    Eratos Example, Calculates all Prime Numbers from 1 to max

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program eratosthenes;

  const
     max = 1000000;
  var
     a : array[1..max] of boolean;

  procedure eratos;

    var
       i,j : longint;

    begin
       a[1]:=false;
       for i:=2 to max do
         a[i]:=true;
       for i:=2 to max div 2 do
         if a[i] then
           for j:=2 to max div i do
             a[i*j]:=false;
       writeln;
       j:=0;
       for i:=1 to max do
        begin
          if a[i] then
           begin
             write(i:7);
             inc(j);
             if (j mod 10)=0 then
              writeln;
           end;
        end;
       writeln;
    end;

  begin
     write('Calculating the Prime Numbers from 1 to ',max,'...');
     eratos;
  end.

{
  $Log$
  Revision 1.2  2000-04-10 08:34:25  pierre
   1 is NOT a prime

  Revision 1.1  2000/03/09 02:49:09  alex
  moved files

  Revision 1.5  1998/09/11 10:55:21  peter
    + header+log

  Revision 1.4  1998/09/04 17:38:15  pierre
    * the algorythm was wrong (unnecessary checks were made)
}