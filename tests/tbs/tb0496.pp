{$mode delphi}
type
  tmyclass = class
    procedure m1;virtual;
    procedure m2;virtual;
  end;

  tm1 = procedure of object;

var
  res : longint;

procedure tmyclass.m1;
  begin
    res:=1;
  end;

procedure p2(m1 : tm1);
  begin
    m1;
  end;

procedure tmyclass.m2;
  begin
    p2(m1);
  end;

var
  myclass : tmyclass;
begin
  res:=longint($deadbeef);
  myclass:=tmyclass.create;
  myclass.m2;
  myclass.free;
  if res<>1 then
    halt(1);
  writeln('ok');
end.
