{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }
{ %norun }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

type
  ta = objcclass external name 'NSObject' (NSObject)
    procedure test(l: longint; a: array of const); message 'class:';
    procedure test2(l: longint); varargs; message 'class:';
  end;

begin
end.
