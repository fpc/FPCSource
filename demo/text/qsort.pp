{
    $Id: qsort.pp,v 1.2 2002/09/07 15:06:35 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by the Free Pascal Development Team

    QuickSort Example

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program quicksort;

  const
     max = 100000;

  type
     tlist = array[1..max] of longint;

  var
     data : tlist;


procedure qsort(var a : tlist);

    procedure sort(l,r: longint);
      var
         i,j,x,y: longint;
      begin
         i:=l;
         j:=r;
         x:=a[(l+r) div 2];
         repeat
           while a[i]<x do
            inc(i);
           while x<a[j] do
            dec(j);
           if not(i>j) then
             begin
                y:=a[i];
                a[i]:=a[j];
                a[j]:=y;
                inc(i);
                j:=j-1;
             end;
         until i>j;
         if l<j then
           sort(l,j);
         if i<r then
           sort(i,r);
      end;

    begin
       sort(1,max);
    end;

var
  i : longint;
begin
  write('Creating ',Max,' random numbers between 1 and 500000');
  randomize;
  for i:=1 to max do
    data[i]:=random(500000);
  writeln;
  writeln('Sorting...');
  qsort(data);
  writeln;
  for i:=1 to max do
   begin
     write(data[i]:7);
     if (i mod 10)=0 then
      writeln;
   end;
end.
{
  $Log: qsort.pp,v $
  Revision 1.2  2002/09/07 15:06:35  peter
    * old logs removed and tabs fixed

}
