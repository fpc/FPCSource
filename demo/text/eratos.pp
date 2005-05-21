{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-2005 by Florian Klaempfl

    Eratos Example, Calculates all Prime Numbers from 1 to max

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program eratosthenes;

  const
{$ifndef MACOS}
     max = 1000000;
{$else}
     max = 10000; {Actually it works with 100000 also, but not 1000000,}
                  {in which case the OS refuses to start it.}
{$endif}

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
  Revision 1.3  2005/05/14 11:11:33  olle
    * Smaller arrray sizes for macos

  Revision 1.2  2002/09/07 15:06:35  peter
    * old logs removed and tabs fixed

}
