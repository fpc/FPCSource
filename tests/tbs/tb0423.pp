{$ifdef fpc}{$mode delphi}{$endif}

type
{$ifndef fpc}
  codepointer = pointer;
{$endif}
   tmethod = record
      code : codepointer;
      data : pointer;
   end;

var
   p : procedure(l : longint) of object;

begin
   tmethod(p).data:=nil;
end.
