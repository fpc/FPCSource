var
  S:String[16];
  C:Char = '?';
  inss: string = 'abc';
begin
  S:='DefineTestString';
  Insert(C,S,20);
  if (length(s)>16) or
     (s<>'DefineTestString') then
    halt(1);
  insert(inss,s,20);
  if (length(s)>16) or
     (s<>'DefineTestString') then
    halt(2);
end.
