Program UnsureOptsFail;
{This program shows how unsure optimizations can cause wrong code if you
 program Silly Things (TM)

 The principle is always the same:

 you have a normal variable (local or global) and a pointer to it in one way
 or another (be it a normal pointer or a var parameter).

 a) you first cause the value from the memeory location to be loaded in a
    register (e.g. by using the normal variable as an array index)
 b) next you assign a new value to the memory location (e.g. through the
    pointer)
 c) finally, you compare the two values

 Of course you can also use the pointer as an array index and assign a new
 value to the normal variuable, that doesn't change anything).

 The problem is that the value of the first load is still in a register, so
 it isn't loaded from memory again, so you compare the old value with the
 new one.

 Note: this code doesn4t function correctly only when compiled with uncertain
 optimizations on. All other forms of optimization are completely safe.
 }

var l: longint;
    p: ^longint;
    a: Array[1..10] of byte;

Procedure ChangeIt(var t: longint);
{The same principle as in the main program, only here we have a var parameter
 instead of a "normal" pointer. If l is passed to this procedure, it doesn't
 function right.}
Begin
  t := 1;
  If t = l Then   {t gets loaded in a register (eax)}
    Begin
      l := 2;
      If t <> l then
        Writeln('She can''t take any more or she''ll blow, captain!');
    End;
End;

begin
  p := @l;     {p points to l}
  l := 1;
  a[p^] := 2;  {load p^ in a register (eax in this case)}
  l := 2;      {change the value of l}
  If p^ <> l
    Then Writeln('Houston, we have a problem!');
  ChangeIt(l);
End.
