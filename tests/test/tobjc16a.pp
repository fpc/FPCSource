{ %norun }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{ Written by Jonas Maebe in 2010, released into the public domain }

{$modeswitch objectivec1}

type
  ta = objcclass
    procedure test(a: longint); varargs; message 'test:a:';
  end; external name 'NSObject';

begin
end.
