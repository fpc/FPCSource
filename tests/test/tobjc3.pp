{ %fail }
{ %opt=-vh -Seh }
{ %target=darwin }
{ %cpu=powerpc,i386 }

{$modeswitch objectivec1}

type
  { should produce a hint that ta does not automatically derive from any
    other class  }
  ta = objcclass
  end; external;

var
  a: ta;
  b: nsobject;
  c: id;
begin
  { avoid hints about unused types/variables/units }
  a:=nil;
  if (a<>nil) then
    exit;
  c:=nil;
  b:=c;
  b.isEqual_(b);
end.
