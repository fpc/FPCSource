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

begin
end.
