Const
    N   = 10000;

Var
    count       : array[0..200] of int64;
    i , tot     : longint;

Begin
    fillchar(count , sizeof(count) , 0);
    for i := 1 to N do
      begin
          inc(count[random(201)]);
      end;
    tot := 0;
    for i := 0 to 200 do inc(tot , count[i]);
    if (tot <> N) then
      halt(1);
End.
