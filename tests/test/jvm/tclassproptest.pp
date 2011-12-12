program tclassproptest;

{$mode objfpc}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tclassprop = class
   strict private
    class var fx: longint;
   public
    class property x: longint read fx write fx;
    class procedure test(l: longint);
  end;

class procedure tclassprop.test(l: longint);
  begin
    if fx<>l then
      raise jlexception.create('test 1 error');
  end;


var
  c: tclassprop;
begin
  c:=tclassprop.create;
  c.x:=123;
  c.test(123);
  if c.x<>123 then
    raise jlexception.create('test 2 error');
end.
