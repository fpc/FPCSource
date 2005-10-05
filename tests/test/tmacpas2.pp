{$MODE MACPAS}

{Tests of mac pascal constructs}

program tmacpas2;

var
  success: Boolean = true;

type
  {Since we do not want to compile in the whole mac api, we
   simulate decl of FourCharCode here:}

  MyFourCharCodeType = Longword;

procedure Proc;

begin
  {** Exit with proc name as argument **}
  Exit(Proc);
end;

procedure TestFourCharCode(myFCC: MyFourCharCodeType);

begin
  Writeln('FPC creator code as number: ', hexstr(myFCC,8));
  if myFCC <> $46506173 then
    success := false;
end;

const 
  myFCCconst = 'FPas'; {Free Pascals Creator code :) }

var
  p: pointer;
  l,i: longint;
  a,b,c : Boolean;

begin
  a := true;
  b := true;
  c := false;

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

  {** Test cycle and leave **}
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

  {** Does literal four char codes work**}
  {Both directly and indirectly}
  TestFourCharCode('FPas');
  TestFourCharCode(myFCCconst);

  if success then
    Writeln('Whole test succeded')
  else
    begin
      Writeln('Whole test failed');
      halt(1);
    end;
end.
