program test;
{$pointermath on}

type
  PRec = ^TRec;
  TRec = record
  end;

begin
  argv:=@PRec(0)[0];   // OK
  argv:=@PRec(nil)[0]; // NotOK - "Can't assign values to an address"
end.
