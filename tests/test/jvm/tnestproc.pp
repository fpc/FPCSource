program tnestproc;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

procedure outer(var para: byte);
  const xxx: longint = 5;
  var
    a: longint;
    called: boolean;

  procedure inner;
    begin
      if not called then
        begin
          called:=true;
          inner;
          exit;
        end;
      if a<>1 then
        raise JLException.Create('a1');
      if para<>2 then
        raise JLException.Create('para1');
      a:=2;
      para:=3;
    end;


  procedure inner2;
    var
      b: longint;
  
    procedure doubleinner;
      begin
        b:=b+1;
      end;
      
    begin
      b:=2;
      doubleinner;
      if b<>3 then
        raise JLException.Create('b');
    end;

  begin
    a:=1;
    called:=false;
    inner;
    if a<>2 then
      raise JLException.Create('a2');
    if para<>3 then
      raise JLException.Create('para2');
    inner2;
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
