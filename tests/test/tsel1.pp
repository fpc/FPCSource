{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %norun }

{$modeswitch objectivec1}

var
  a: sel;
begin
  a:=selector('mymethod');
  a:=selector('mymethod');
  a:=selector('mymethod:');
  a:=selector('::');
  a:=selector('a:b:c:');
end.
