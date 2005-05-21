{$mode delphi}
{ this should be only allowed in delphi mode; it's a delphi bug }
uses
  tb0483u;

type
  tmyclass2 = class(tmyclass1)
    procedure x(var l : longint);message 1234;
  end;

procedure tmyclass2.x(var l : longint);
  begin
    inherited;
  end;

var
  myclass2 : tmyclass2;
  l : longint;

begin
  myclass2:=tmyclass2.create;
  myclass2.x(l);
  myclass2.free;
  if testresult<>1 then
    begin
      writeln('error');
      halt(1);
    end;
  writeln('ok');
end.
