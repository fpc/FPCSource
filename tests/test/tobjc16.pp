{ %fail }
{ %opt=-vh -Seh }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$modeswitch objectivec1}

type
  ta = objcclass external
    { should give an error about a wrong number of parameters --
      the message name suggests two parameters, but the procedure
      has only one. }
    procedure test(a: longint); message 'test:a:';
  end;

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
