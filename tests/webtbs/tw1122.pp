{ %CPU=i386 }
{$mode objfpc}
uses sysutils;
{$asmmode intel}
begin
  try
     asm
        db 0fh,0fh,0fh,0fh
     end;
  except
     halt(0);
  end;
  halt(1);
end.
