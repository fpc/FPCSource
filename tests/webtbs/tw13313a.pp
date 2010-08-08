{ %interactive }

{ run in gdb, "print a.a7" when reaching the writeln and
  check that the output is 8
}

{$mode objfpc}

type
  ta = class
   private
    fa: array[6..10] of byte;
   public
    property a7: byte read fa[7];
  end;

var
  a: ta;
begin
  a:=ta.create;
  a.fa[7]:=8;
  writeln(a.a7);
end.
