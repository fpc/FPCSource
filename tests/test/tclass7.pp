{ %version=1.1 }

{$ifdef fpc}
 {$mode delphi}
{$endif}

type
 tClassA=class
  procedure DefaultHandler(var Message);override;
 end;

 tClassB=class(tClassA)
  procedure test(var m);message 1;
 end;

 tClassC=class(tClassB)
  procedure test00(var m);message 1;
 end;

var
  counter : longint;

procedure tClassA.DefaultHandler(var Message);
 begin
  writeln('DDDDDDDDDD');
  inc(counter);
 end;

procedure tClassB.test(var m);
 begin
  writeln('BBBBBBBBBB');
  inc(counter);
  inherited;
 end;

procedure tClassC.test00(var m);
 begin
  writeln('CCCCCCCCCC');
  inc(counter);
  inherited;
 end;

var
 C:tObject;
 One:longint;
begin
 One:=1;
 C:=tClassC.Create;
 C.Dispatch(One);
 C.Destroy;
 if counter<>3 then
  begin
    writeln('Error in class DefaultHandler for messages!');
{$ifdef VER1_0}
    halt(2);
{$else not VER1_0}
    halt(1);
{$endif not VER1_0}
  end;
end.
