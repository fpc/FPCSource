unit bug0067b;

interface


type
  tlong=record
    a : longint;
  end;

procedure p(var l:tlong);

implementation

uses bug0067;

{ the tlong parameter is taken from unit bug0067,
  and not from the interface part of this unit.
  setting the uses clause in the interface part 
  removes the problem }

procedure p(var l:tlong);
begin
  bug0067.p(bug0067.tlong(l));
end;

end.
