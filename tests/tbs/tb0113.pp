{ Old file: tbs0132.pp }
{ segmentation fault with type loop                     OK 0.99.7 (FK) }

type

  p=^p2;
  p2 = ^p;

  var a:p;
      a2:p2;

  begin
        a:=@a2;
        a2:=@a;
        a:=a2^;
  end.
