{ %target=darwin }
{ %cpu=powerpc,i386 }
{ %norun }

{$modeswitch objectivec1}

var
  a: sel;
begin
  a:=objcselector('mymethod');
  a:=objcselector('mymethod');
  a:=objcselector('mymethod:');
  a:=objcselector('::');
  a:=objcselector('a:b:c:');
end.
