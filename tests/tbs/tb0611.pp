{$mode objfpc}
{$warn 6018 off}
type
  tmyclass = class
    procedure HelloMethod(i : longint);
  end;

procedure Hello(i : longint);
  begin
    writeln({$I %CURRENTROUTINE%});
    if {$I %CURRENTROUTINE%}<>'Hello' then
      halt(i);
  end;

procedure tmyclass.HelloMethod(i : longint);
  begin
    writeln({$I %CURRENTROUTINE%});
    if {$I %CURRENTROUTINE%}<>'HelloMethod' then
      halt(i);
  end;

var
  myclass : tmyclass;

begin
  Hello(1);
  myclass:=tmyclass.create;
  myclass.HelloMethod(1);
  myclass.Free;
  writeln('Ok');
end.
