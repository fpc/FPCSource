{ Old file: tbs0303.pp }
{ One more InternalError(10) out of register !         OK 0.99.13 (FK) }


  type
    intarray = array[1..1000,0..1] of longint;

  procedure test;
   var
     ar : intarray;
     i : longint;
  procedure local;
   begin
    i:=4;
    ar[i,0]:=56;
    ar[i-1,0]:=pred(ar[i,0]);
   end;
  begin
    local;
  end;

begin
  test;
end.
