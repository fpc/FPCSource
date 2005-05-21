{ this test program test allocation of large pieces of stack }
{ this is especially necessary for win32                     }

procedure p1(a : array of byte);

  var
     i : longint;

  begin
     for i:=0 to high(a) do
       a[i]:=0;
  end;

procedure p2;

  var
     a : array[0..20000] of byte;
     i : longint;

  begin
     for i:=0 to high(a) do
       a[i]:=0;
  end;

procedure p3;

  var
     a : array[0..200000] of byte;
     i : longint;

  begin
     for i:=0 to high(a) do
       a[i]:=0;
  end;


var
   a : array[0..10000] of byte;

begin
   p1(a);
   p2;
   p3;
end.
