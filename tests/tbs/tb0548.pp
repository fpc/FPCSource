{$r+}
{$q+}

var
  wo: word;
  si: smallint;

begin
  wo:=$9876;
  if swap(wo)<>$7698 then
    halt(1);
  if swapendian(wo)<>$7698 then
    halt(2);
  wo:=$1290;
  if swap(wo)<>$9012 then
    halt(3);
  if swapendian(wo)<>$9012 then
    halt(4);

  si:=smallint($9876);
  if swap(si)<>$7698 then
    halt(5);
  if swapendian(si)<>$7698 then
    halt(6);
  si:=$1290;
  if swap(si)<>smallint($9012) then
    halt(7);
  if swapendian(si)<>smallint($9012) then
    halt(8);
end.
