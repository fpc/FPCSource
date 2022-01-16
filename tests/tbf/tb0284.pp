{ %fail }
{ %cpu=aarch64 }
{ %opt=-s }  { the compiler must throw the error }

begin
  asm
    ld1 {v20.4s-v23.4s,v24.4s},[x3]
  end;
end.
