{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

program project1;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
type

 MyObject = objcclass(NSObject)

   function getsspara(l1,l2: longint): shortstring ; message 'getss:l1:';

 end;

var
  m: MyObject;
  b: boolean;

function MyObject.getsspara(l1,l2: longint): shortstring;
begin
  if (self<>m) then
    halt(1);
  if (self.self<>m) then
    halt(2);
  if _cmd<>objcselector('getss:l1:') then
    halt(3);
  result:='';
end;


begin
 m := MyObject.alloc;
 m:=m.init;

 m.getsspara(1,2);

 m.release;
end.
