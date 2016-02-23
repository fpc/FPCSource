{ %cpu=i8086 }

program tptrcon;
var
  hp: HugePointer = HugePointer(Ptr($FACE, $55AA));
  fp: FarPointer = Ptr($DEAD, $BEEF);
  fp2: FarPointer = FarPointer($12345678);
  np: NearPointer = NearPointer($FEED);
  hpl: LongInt absolute hp;
  fpl: LongInt absolute fp;
  fp2l: LongInt absolute fp2;
  npw: Word absolute np;

procedure Error;
begin
  Writeln('Error!');
  Halt(1);
end;

begin
  if hpl<>LongInt($FACE55AA) then
    Error;
  if fpl<>LongInt($DEADBEEF) then
    Error;
  if fp2l<>LongInt($12345678) then
    Error;
  if npw<>Word($FEED) then
    Error;
end.
