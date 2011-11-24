{$codepage cp866}
program tcptypedconst3;
const 
  c : Char = '£';//= #163
begin
  writeln(Ord(c)); 
  if (Ord(c) <> 163) then begin
    writeln('error');
    Halt(1);
  end;  
end.