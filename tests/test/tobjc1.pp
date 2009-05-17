{$mode objfpc}
{$modeswitch objectivec1}

uses
  ctypes;

var
  a: NSObject;
begin
  a:=NSObject(NSObject(NSObject.alloc).init);
  if a.respondstoselector_(selector('isKindOfClass:')) then
    writeln('ok string selector!')
  else
    halt(1);
  if a.respondstoselector_(selector(NSObject.init)) then
    writeln('ok method selector!')
  else
    halt(2);

  if (a.self<>id(a)) then
    halt(3);
  if (a.superclass<>nil) then
    halt(4);
end.
