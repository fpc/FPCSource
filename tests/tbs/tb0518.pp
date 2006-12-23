{$mode delphi}
type
  tc = class
    constructor create(const n: ansistring);
  end;

constructor tc.create;
begin
  if (n <> 'abc') then halt(1);
end;

begin
  tc.create('abc');
end.
