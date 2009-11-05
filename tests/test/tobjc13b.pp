{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

type
  ta = objcclass(NSObject)
    procedure test(l: longint; a: array of const); message 'class:';
  end;

  procedure ta.test(l: longint; a: array of const);
   begin
   end;

begin
end.
