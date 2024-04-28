{ %OPT=-O2 -Co- }
program tw40756;
var
  x: uint32;
begin
  x := uint32(random(0) - 1); // x = 4294967295.
  case x of
    0..1:
      Halt(1);
    4294967295:
      WriteLn('ok');
    else
      Halt(2);
  end;
end.
