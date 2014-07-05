program tw26230;

const
    C1 = 86400000000; //MCS in a day

var
    AInput,
        Product : Int64;

begin
    AInput := 1;
    //asm int3 end; //debug trap; `disassemble`
    Product := AInput * C1;
    WriteLn(AInput, ' * ', C1, ' = ', Product);
    if Product <> 86400000000 then
    begin
      Writeln('Error!');
      Halt(1);
    end
    else
      Writeln('Ok');
end.
