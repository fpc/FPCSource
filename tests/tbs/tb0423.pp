{$ifdef fpc}{$mode delphi}{$endif}

type
   tmethod = record
      code,data : pointer;
   end;

var
   p : procedure(l : longint) of object;

begin
   tmethod(p).data:=nil;
end.
