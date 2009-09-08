{ %fail }
{ %target=darwin }
{ %cpu=powerpc,i386 }
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
