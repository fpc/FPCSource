{****************************************************************************

                   Copyright (c) 1994 by Florian Kl„mpfl

 ****************************************************************************}
 
{ Demonstrationsprogramm zu FPKPascal }
{ berechnet magische Quadrate (Summe alle Spalten, Zeilen und }
{ Diagonalen ist gleich)				      }
program magic;

  const
     maxsize = 11;
     
  type
     sqrtype = array[1..maxsize, 1..maxsize] of integer;
     
  var
     square : sqrtype;
     size, row, sum : integer;

  procedure makesquare(var sq : sqrtype;limit : integer);
  
    var
       num,r,c : integer;

    begin
       for r:=1 to limit do
         for c:=1 to limit do 
           sq[r, c] := 0;
       if (limit and 1)<>0 then
         begin
            r:=(limit+1) div 2;
            c:=limit;
            for num:=1 to limit*limit do
              begin
                 if sq[r,c]<>0 then
                   begin
                      dec(r);
                      if r<1 then 
                        r:=r+limit;
                      c:=c-2; 
                      if c<1 then 
                        c:=c+limit;
                   end;
                 sq[r,c]:=num;
                 inc(r);
                 if r>limit then 
                   r:=r-limit;
                 inc(c);
                 if c>limit then 
                   c:=c-limit;
              end; 
         end;
     end;

  procedure writesquare(var sq : sqrtype;limit : integer);
  
    var 
       row,col : integer;

    begin
       for row:=1 to Limit do
         begin
   	    for col:=1 to (limit div 2) do
	      write(sq[row,2*col-1]:4,' ',sq[row,2*col]:4,' ');
            writeln(sq[row,limit]:4);
         end;
    end;

begin
  size:=3;
  while (size<=maxsize) do
    begin
       writeln('Magisches Quadrat mit der Seitenl„nge ',size);
       writeln;
       makesquare(square,size);
       writesquare(square,size);
       writeln;
       sum:=0;
       for row:=1 to size do
         sum:=sum+square[row,1];
       writeln('Summe in den Reihen, Spalten und Diagonalen = ', sum);
       writeln;
       writeln;
       size:=size+2;
    end;
end.
