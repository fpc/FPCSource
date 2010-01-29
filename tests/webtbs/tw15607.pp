{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{ %norun }

{$mode delphi}{$modeswitch objectivec1}

var
  o: id;
begin
  o.description;
end.
