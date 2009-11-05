{ %fail }
{ %target=darwin }
{ %cpu=powerpc,powerpc64,i386,x86_64,arm }

{$modeswitch objectivec1}

type
  ta = objcprotocol
    { needs message name specification }
    procedure test; 
  end; external;

begin
end.
