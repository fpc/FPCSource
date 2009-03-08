{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %fail }

{$modeswitch objectivec1}

var
  a: sel;
begin
  a:=selector('my:method');
end.
