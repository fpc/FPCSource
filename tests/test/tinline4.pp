unit tinline4;

{$inline on}

interface

uses uinline4b;

procedure acall(a,b: longint); inline;

implementation

procedure acall(a,b: longint); inline;
begin
  libbase:=nil;
end;

end.
