{ Old file: tbs0221.pp }
{ syntax parsing incompatibilities with tp7            OK 0.99.11 (PFV) }


var
  r : double;
  c : char;
begin
  r:=1.;
  c:=^.; { this compile in tp7, c should contain 'n'/#110 }
  if c<>#110 then
    begin
       Writeln('FPC does not support ^. character!');
       Halt(1);
    end;
end.
