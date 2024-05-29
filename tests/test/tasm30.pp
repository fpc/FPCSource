{ %NORUN }
{ %CPU=i386,x86_64 }

program tasm30;

begin
  asm
    enter $5,$1234
  end;
end.
