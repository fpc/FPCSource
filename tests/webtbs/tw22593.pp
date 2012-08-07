{ %norun }

{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  tc = class
  end;
  tcc = class of tc;
  tc3 = class;

  tc2 = class
    constructor create(c: tcc = nil; c3: tc3 = nil);
  end;

  tc3 = class
  end;

constructor tc2.create(c: tcc = nil; c3: tc3 = nil);
begin
end;

begin
end.
