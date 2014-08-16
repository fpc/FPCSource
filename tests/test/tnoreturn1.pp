{ %OPT=-Sew -vw -Oodfa}

procedure do_halt;noreturn;
  begin
    halt(0);
  end;

function f : longint;
  begin
    do_halt;
  end;

begin
  f;
end.

