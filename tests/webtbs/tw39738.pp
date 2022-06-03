{ %cpu=riscv32,riscv64,riscv128 }
begin
  asm 
    lui t0, 0x06 
  end['x5','t1','zero']; 
end.
