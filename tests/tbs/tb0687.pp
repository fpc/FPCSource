{ %cpu=aarch64 }
{ %norun }

begin
  asm
    ld1 {v20.4s-v21.4s,v22.4s},[x3]
    ld1 {v20.4s-v23.4s},[x3]
    ld1 {v20.4s-v22.4s,v23.4s},[x3]
    ld1 {v20.4s-v21.4s,v22.4s},[x3]
  end;
end.
