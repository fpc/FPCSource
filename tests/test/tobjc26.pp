{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$modeswitch objectivec1}

uses
  uobjc26;

var
  a: ta;

begin
  a:=ta(ta.alloc).init;
  // should not be visible
  a.implementationcategorymethod;
  a.release
end.
