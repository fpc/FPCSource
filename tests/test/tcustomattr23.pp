{ an attribute written with its unit name and its full class name, and with its
  unit name and its short name }
program tcustomattr23;

{$mode delphi}

type
  [System.RefAttribute]
  TA = class
  end;

  [System.Ref]
  TB = class
  end;

begin
  Writeln('ok');
end.
