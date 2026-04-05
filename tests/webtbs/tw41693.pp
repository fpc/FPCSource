{ %FAIL }

program tw41693;

begin
  Volatile(777)  := 888;
  Aligned(777)   := 888;
  Unaligned(777) := 888;
end.
