{****************************************************************************
  $Id$

                   Copyright (c) 1993,94 by Florian Kl„mpfl
                   Translated By Eric Molitor (emolitor@freenet.fsu.edu)

 ****************************************************************************}

{ Demonstration Program in FPKPascal }
{ Calculates all Prime Numbers from 1 to max }

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
       for i:=1 to max do
         a[i]:=true;
       for i:=2 to max div 2 do
         for j:=2 to max div i do
           a[i*j]:=false;
       writeln;
       for i:=1 to max do
         if a[i] then
           write(i:8);
       writeln;
    end;

  begin
     write('Calculating the Prime Numbers from 1 to ',max,'...');
     eratos;
  end.

{ 
  $Log$
  Revision 1.3  1998-04-06 12:23:21  pierre
    * log problem

  Revision 1.2  1998/04/06 12:17:00  pierre
   * made array a global to avoid stack overflow
}