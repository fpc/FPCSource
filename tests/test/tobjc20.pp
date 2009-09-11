{ %target=darwin }
{ %cpu=powerpc,i386 }

program project1;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
type
  tr = record
    s: shortstring;
  end;

 MyObject = objcclass(NSObject)
   fss: shortstring;
   fsingle: single;
   fdouble: double;

   function getss: shortstring ; message 'getss';
   function getsspara(l1,l2: longint): shortstring ; message 'getss:l1:';
   function getsingle(l1,l2: longint): single; message 'getsingle:l1:';
   function getdouble(l1,l2: longint): double; message 'getdouble:l1:';
 end;

function MyObject.getss: shortstring;
begin
  result:=fss;
end;


function MyObject.getsspara(l1,l2: longint): shortstring;
begin
  if (l1<>1) or
     (l2<>2) then
    halt(1);
  result:=fss;
end;


function MyObject.getsingle(l1,l2: longint): single;
begin
  if (l1<>1) or
     (l2<>2) then
    halt(2);
  result:=fsingle;
end;


function MyObject.getdouble(l1,l2: longint): double;
begin
  if (l1<>1) or
     (l2<>2) then
    halt(3);
  result:=fdouble;
end;

var
  m: MyObject;
begin
 m := MyObject.alloc;
 m:=m.init;
 m.fss:='hello!';
 m.fsingle:=123.625;
 m.fdouble:=9876.0625;

 if m.getss<>'hello!' then
   halt(4);
 m.fss:='gij ook';
 if m.getsspara(1,2)<>'gij ook' then
   halt(5);
 if m.getsingle(1,2)<>123.625 then
   halt(6);
 if m.getdouble(1,2)<>9876.0625 then
   halt(7);
 m.release;
end.
