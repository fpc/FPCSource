{ Old file: tbs0067b.pp }
{  (Work together)                         OK 0.99.1 }

unit tb0060;

interface


type
  tlong=record
    a : longint;
  end;

procedure p(var l:tlong);

implementation

uses ub0060;

{ the tlong parameter is taken from unit bug0067,
  and not from the interface part of this unit.
  setting the uses clause in the interface part
  removes the problem }

procedure p(var l:tlong);
begin
  ub0060.p(ub0060.tlong(l));
end;

end.
