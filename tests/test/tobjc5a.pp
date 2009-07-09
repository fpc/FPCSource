{ %fail }
{ %target=darwin }
{ %cpu=powerpc,i386 }

{$modeswitch objectivec1}

type
  ta = objcprotocol
    { needs message name specification }
    procedure test; 
  end; external;

begin
end.
