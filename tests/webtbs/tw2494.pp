{ Source provided for Free Pascal Bug Report 2494 }
{ Submitted by "Alan Mead" on  2003-05-17 }
{ e-mail: cubrewer@yahoo.com }
program dummy;

type
  matrix_element = array[1..1] of byte;
  big_matrix = array[1..1000000,1..610] of matrix_element;

  longarray = array[0..0] of real;

{var
  a : big_matrix;}

var p:pointer;
  l : ^longarray;
  size, storage : longint;
  i,j:longint;
  done:boolean;

begin
  ReturnNilIfGrowHeapFails:=true;
  writeln('Total heap available is ',MemAvail,' bytes');
  writeln('Largest block available is ',MaxAvail,' bytes');
  done := false;
  size := 40000000;
  repeat
    size := round(size * 1.1);
    storage := size * sizeof(real);
    writeln('size=',size,' (storage=',storage,')');
    getmem(l,storage);
    if (l=nil) then
      begin
        done := true;
        writeln('getmem() failed');
      end
    else
      begin
        writeln('getmem() was successful');
        freemem(l,storage);
      end;
  until (done);
end.

