{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{ Written by Jonas Maebe in 2009, released into the public domain }

{$modeswitch objectivec1}

type
  ta = objcprotocol external
    { needs message name specification }
    procedure test; 
  end;

begin
end.
