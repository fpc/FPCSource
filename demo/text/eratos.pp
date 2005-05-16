{
    $Id: eratos.pp,v 1.2 2002/09/07 15:06:35 peter Exp $
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
  $Log: eratos.pp,v $
  Revision 1.2  2002/09/07 15:06:35  peter
    * old logs removed and tabs fixed

}
