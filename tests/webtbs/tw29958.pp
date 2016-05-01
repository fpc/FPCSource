{ %CPU=i386,x86_64 }
{ %norun }

const
  expected_code : array[0..2] of byte = ($0f,$01,$D0);

procedure proc;assembler;nostackframe;
  asm
    xgetbv
  end;


var
  P : pointer;
  i : integer;

begin
  for i:=0 to high(expected_code) do
    if (pbyte(@proc)+i)^<>expected_code[i] then
      begin
        writeln('Error at pos ',i,'. Expected $',hexstr(expected_code[i],2),' got $',hexstr((pbyte(@proc)+i)^,2));
        halt(1);
      end;
  writeln('ok');
end.
