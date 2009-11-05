{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %recompile }
{ %norun }

{$mode objfpc}
{$modeswitch objectivec1}

library tobjcl1;

uses
  uobjcl1;

exports
  MyLibObjCClass;

end.
