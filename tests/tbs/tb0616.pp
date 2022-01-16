{ %CPU=arm}
{ %norun }
{ %OPT=-Cparmv5te }

procedure p;

begin
  asm
    .code 16
      bx lr
    .code 32
      bx lr
  end;
end;

begin
end.
