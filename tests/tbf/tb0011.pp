{ %FAIL }
{ Old file: tbf0087.pp }
{  shows internal error 12 - no more SegFaults           OK 0.99.1 (FK) }

{
  BP Error message is 'Pointer variable Expected'
}
type
  tobj=object
    l : longint;
    constructor init;
  end;
var
  o : tobj;
begin
  new(o);            {This will create a internal error 9999}
  new(o,init);       {This will create a Segfault and Core Dump under linux}
end.
