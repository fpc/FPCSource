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