{****************************************************************************

                   Copyright (c) 1993,94 by Florian Kl„mpfl
                   Translated by Eric Molitor (emolitor@freenet.fsu.edu)

 ****************************************************************************}

{ Demonstration Program in FPKPascal }

  const
     max = 1000;

  type
     tlist = array[1..max] of integer;

  var
     data : tlist;

procedure qsort(var a : tlist);

    procedure sort(l,r: integer);

      var
         i,j,x,y: integer;

      begin
         i:=l;
         j:=r;
         x:=a[(l+r) div 2];
         repeat
           while a[i]<x do i:=i+1;
           while x<a[j] do j:=j-1;
           if not(i>j) then
             begin
                y:=a[i];
                a[i]:=a[j];
                a[j]:=y;
                i:=i+1;
                j:=j-1;
             end;
         until i>j;
         if l<j then sort(l,j);
         if i<r then sort(i,r);
      end;

    begin
       sort(1,max);
    end;

  var
     i : longint;

  begin
    write('Creating ',Max,' random numbers between 1 and 30000');
    randomize;
    for i:=1 to max do
      data[i]:=random(30000);
    write(#13#10'Sorting...');
    qsort(data);
    writeln;
    for i:=1 to max do
      write(data[i]:8);
end.
