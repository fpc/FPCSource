program tnestproc;

{$mode delphi}

uses
  jdk15;

procedure outer(var para: byte);
  const xxx: longint = 5;
  var
    a: longint;

  procedure inner;
    begin
      if a<>1 then
        raise JLException.Create('a1');
      if para<>2 then
        raise JLException.Create('para1');
      a:=2;
      para:=3;
    end;

  begin
    a:=1;
    inner;
    if a<>2 then
      raise JLException.Create('a2');
    if para<>3 then
      raise JLException.Create('para2');
  end;

var
  x: record end;
  y: byte;
begin
  y:=2;
  outer(y);
  if y<>3 then
    raise JLException.Create('para3');
end.
