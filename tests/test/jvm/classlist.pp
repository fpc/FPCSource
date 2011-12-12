program classlist;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
 T1 = class
 end;

 CT1 = class of T1;

function test : string;
var
 T : T1;
 C : CT1;
 L : JUArrayList;
begin
 T := T1.Create;
 C := CT1(JLObject(T).getClass);
 L := JUArrayList.Create;
 L.add(JLObject(C)); // ???
 if ct1(l.get(0))<>t1 then
   raise JLException.create('error');
end;

begin
  test;
end.
