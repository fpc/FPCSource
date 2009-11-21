{$mode objfpc}
{$modeswitch objectivec1}

{ Written by Jonas Maebe in 2009, released into the public domain }

unit uobjc30c;

interface

type
  ta = objcclass(NSObject)
    field: longint;
    function mytest(c: char): longint; message 'mystest:';
  end;

implementation

function ta.mytest(c: char): longint;
begin
  writeln(c);
  result:=field;
end;

end.
