{ Source provided for Free Pascal Bug Report 2494 }
{ Submitted by "Alan Mead" on  2003-05-17 }
{ e-mail: cubrewer@yahoo.com }
uses
  erroru;

type
  matrix_element = array[1..1] of byte;
  big_matrix = array[1..1000000,1..610] of matrix_element;

  longarray = array[0..0] of real;

{var
  a : big_matrix;}

var p:pointer;
  l : ^longarray;
  size, storage : cardinal;
  i,j:longint;
  done:boolean;
  mem : sizeint;
begin
  ReturnNilIfGrowHeapFails:=true;
  domem(mem);
  done := false;
  size := 40000000;
  repeat
    size := round(size * 1.1);
    storage := size * sizeof(real);
    if storage>2000000000 then
      storage:=2000000000;
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
  domem(mem);
end.
