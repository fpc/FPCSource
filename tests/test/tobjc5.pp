{ %fail }
{ %target=darwin }
{ %cpu=powerpc,i386 }

{$modeswitch objectivec1}

type
  ta = objcclass
    { needs message name specification }
    procedure test; 
  end; external;

begin
end.
