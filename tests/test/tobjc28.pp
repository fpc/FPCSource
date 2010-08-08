{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }
{ %fail }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$modeswitch objectivec1}

type
  ta = objcclass(NSObject)
  end;

  ca = objccategory(ta)
    procedure categorymethod; message 'categorymethod';
  end;

procedure ca.categorymethod;
begin
end;

var
  a: NSObject;
begin
  a:=ta(ta.alloc).init;
  { should fail because the category is for ta, not for nsobject }
  a.categorymethod;
end.
