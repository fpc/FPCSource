{ %opt=-S2 }
{ %target=win32 }
uses
  windows,messages,classes;

type
  tc1 = class
    var a : longint;
    procedure p(var msg : TMessage);
  end;

  tf = function (Window: HWND; Message, WParam: WPARAM;LParam: LPARAM): HRESULT; stdcall;

procedure tc1.p(var msg : TMessage);
  begin
    if (msg.msg<>1) or (msg.wparam<>2) or (msg.lparam<>3) then
      halt(1);
    msg.result:=4+a;
  end;

var
  f : tf;
  c : tc1;
begin
  c:=tc1.create;
  c.a:=0;

  f:=tf(MakeObjectInstance(@c.p));

  if f(0,1,2,3)<>4 then
    halt(1);
  c.a:=3;
  if f(0,1,2,3)<>7 then
    halt(2);

  c.free;

  FreeObjectInstance(f);
  writeln('ok');
end.
