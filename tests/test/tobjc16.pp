{ %fail }
{ %opt=-vh -Seh }
{ %target=darwin }
{ %cpu=powerpc,i386 }

{$modeswitch objectivec1}

type
  ta = objcclass
    { should give an error about a wrong number of parameters --
      the message name suggests two parameters, but the procedure
      has only one. }
    procedure test(a: longint); message 'test:a:';
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
