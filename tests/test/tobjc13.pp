{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

type
  ta = objcclass(NSObject)
    procedure test(l: longint; a: array of const); message 'class:';
    procedure test2(l: longint); varargs; message 'class:';
  end; external name 'NSObject';

begin
end.
