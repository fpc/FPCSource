type t1r = record
             a, b: Byte;
           end;
     t2r = record
             l1, l2: Array[1..4] Of t1r;
           end;


Var r: t2r;

begin
  with r.l1[counter] Do
    Inc(a)
end.
