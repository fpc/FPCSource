{ Old file: tbs0067b.pp }
{  (Work together)                         OK 0.99.1 }

unit tbs0067b;

interface


type
  tlong=record
    a : longint;
  end;

procedure p(var l:tlong);

implementation

uses tbs0067;

{ the tlong parameter is taken from unit bug0067,
  and not from the interface part of this unit.
  setting the uses clause in the interface part
  removes the problem }

procedure p(var l:tlong);
begin
  tbs0067.p(tbs0067.tlong(l));
end;

end.
