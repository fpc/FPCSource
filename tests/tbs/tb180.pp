{ Old file: tbs0207.pp }
{ a class destructor doesn't release the memory        OK 0.99.11 (FK) }


{$mode delphi}
 var i : longint;

begin
   for i:=1 to 100 do
     tobject.create.free;
end.
