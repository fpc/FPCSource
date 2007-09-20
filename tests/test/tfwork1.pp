{ %target=darwin }
{ %cpu=i386,powerpc }

{$linkframework Carbon}

type
  CFStringRef = pointer;

function CFSTR( c: PChar ): CFStringRef; external name '___CFStringMakeConstantString'; mwpascal;

begin
  CFSTR('hello');  
end.
