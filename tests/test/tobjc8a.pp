{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm,aarch64 }
{ %opt=-vh -Seh }
{ %norun }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$mode objfpc}
{$modeswitch objectivec1}

type
  TMyTestClass = objcclass external name 'NSObject' (NSObject)
    { should not give a hint, since we have 'override' }
    function hash: NSUInteger; override;
  end;

var
  a: id;
begin
  { avoid warnings/hints about unused types/variables }
  a:=TMyTestClass.alloc;
  tmytestclass(a).Retain;
end.
