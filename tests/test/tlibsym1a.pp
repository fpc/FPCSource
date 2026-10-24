{ %target=linux,android,darwin,win32,win64 }
{ %skiptarget=$nosharedlib }
{ %norun }

library tlibsym1a;

type
  TCallback = procedure;

procedure libsym_call(aCallback: TCallback); cdecl;
begin
  aCallback();
end;

function libsym_func(aValue: LongInt): LongInt; cdecl;
begin
  libsym_func:=aValue+1;
end;

exports
  libsym_call,
  libsym_func;

begin
end.
