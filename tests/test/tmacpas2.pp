{$MODE MACPAS}

{Tests of mac pascal constructs}

program tmacpas2;

var
  success: Boolean = true;


procedure Proc;

begin
  {** Exit with proc name as argument **}
  Exit(Proc);
end;

const
  a = true;
  b = true;
  c = false;

var
  p: pointer;
  l,i: longint;

begin
  {** Test & and | as alias for AND and OR **}
  if not (a & b) then
    success:= false;
  if not (c | b) then
    success:= false;

  {** Test that Ord() can take pointer values **}
  p:= pointer(4711);
  l:= Ord(p);
  if l <> 4711 then
    success:= false;

  i:= 0;
  while true do
    begin
      i:= i+1;
      if i = 1 then
        Cycle;
      Leave;
    end;
  if i<> 2 then
    success:= false;


  if success then
    Writeln('Whole test succeded')
  else
    begin
      Writeln('Whole test failed');
      halt(1);
    end;
end.
