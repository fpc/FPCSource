uses
   erroru;

procedure chararray2stringtest;

  var
     a : array[1..10,1..10,1..5] of char;
     i,j,k,l : integer;

  begin
     for i:=1 to 10 do
       a[i,i]:='Hello';
     i:=1;
     j:=2;
     k:=3;
     l:=4;
     { test register allocation }
     if (a[i,i]<>'Hello') or
       (a[j,j]<>'Hello') or
       (a[k,k]<>'Hello') or
       (a[l,l]<>'Hello') then
       do_error(1000);
   end;

begin
   writeln('Misc. shortstring tests');
   chararray2stringtest;
   writeln('Misc. shortstring tests successfully passed');
   halt(0);
end.
