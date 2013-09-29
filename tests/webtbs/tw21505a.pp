{$mode objfpc}{$H+}

type
  PPosList = ^TPosList;
  TPosList = record
    elem : Double;
    tail : ^TPosList;
  end;

  operator >< (e : single; list : PPosList) : PPosList;
  begin
    new (result);
    result^.elem := e;
    result^.tail := list;
  end;

var
  list : PPosList;
begin
// This makes Fatal: Internal error 2008022101
//  list := 1.0 >< 3.0 >< 5.0 >< 7.0 >< 9.0 >< nil;
// This says Error: Operation "><" not supported for types "ShortInt" and "Pointer"
  list := 1.0 >< (3.0 >< (5.0 >< (7.0 >< (9.0 >< nil))));
  if list^.elem<>1.0 then
    halt(1);
  if list^.tail^.elem<>3.0 then
    halt(2);
  if list^.tail^.tail^.elem<>5.0 then
    halt(3);
  if list^.tail^.tail^.tail^.elem<>7.0 then
    halt(4);
  if list^.tail^.tail^.tail^.tail^.elem<>9.0 then
    halt(5);
  if list^.tail^.tail^.tail^.tail^.tail<>nil then
    halt(6);
end.
