{ %target=linux,solaris,freebsd,win32 }
{ %fail }
{ %opt=-Sew }

{ just some random non-darwin targets }

{$linkframework Carbon}

type
  CFStringRef = pointer;

function CFSTR( c: PChar ): CFStringRef; external name '___CFStringMakeConstantString'; mwpascal;

begin
  CFSTR('hello');  
end.
