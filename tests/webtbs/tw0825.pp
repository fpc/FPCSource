{$mode tp}
{ args for destructors
  are allowed in TP mode for compatibility only PM }

program   test_destructor_with_args;

var
  z : longint;

  type
    tt = object
     constructor dummy;
     destructor done(x : longint);virtual;
    end;

  constructor tt.dummy;
    begin
    end;

  destructor tt.done;
    begin
      Writeln('x in tt.done is ',x);
      z:=x;
    end;

  var
    pt : ^tt;

begin
  Writeln('ln(5)=',ln(5));
  new(pt,dummy);
  pt^.done(4);
  if z<>4 then
    Halt(1);
  pt^.dummy;
  dispose(pt,done(5));
  if z<>5 then
    Halt(1);
end.
