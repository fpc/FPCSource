program tnestdynarr;

type
  tbytearray = array of byte;

procedure test(var a: tbytearray);

  procedure nest;
    begin
      a[0]:=2;
    end;

begin
  nest;
end;

var
  a: tbytearray;
begin
  setlength(a,1);
  test(a);
  if a[0]<>2 then
    halt(1);
end.
