{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Florian Klaempfl

    Magic Square Example

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program magic;

{
  Calculate a magic square (sum of the row, colums and diagonals is equal
}

  const
     maxsize = 11;

  type
     sqrtype = array[1..maxsize, 1..maxsize] of longint;

  var
     square : sqrtype;
     size, row, sum : longint;

  procedure makesquare(var sq : sqrtype;limit : longint);

    var
       num,r,c : longint;

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
                        inc(r,limit);
                      dec(c,2);
                      if c<1 then
                        inc(c,limit);
                   end;
                 sq[r,c]:=num;
                 inc(r);
                 if r>limit then
                   dec(r,limit);
                 inc(c);
                 if c>limit then
                   dec(c,limit);
              end;
         end;
     end;

  procedure writesquare(var sq : sqrtype;limit : longint);

    var
       row,col : longint;

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
       writeln('Magic Square with size ',size);
       writeln;
       makesquare(square,size);
       writesquare(square,size);
       writeln;
       sum:=0;
       for row:=1 to size do
         inc(sum,square[row,1]);
       writeln('Sum of the rows,columns and diagonals = ', sum);
       writeln;
       writeln;
       inc(size,2);
    end;
end.
