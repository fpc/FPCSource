{$mode objfpc}
begin
  try
     asm
        // invalid opcode, e.g. SSE instruction
        movaps %xmm6, %xmm7
     end;
  except
     halt(0);
  end;
  halt(1);
end.
