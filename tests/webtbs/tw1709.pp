{ %version=1.1 }

{$ifdef fpc}{$mode delphi}{$endif}

var
 x: array of byte;
begin
  // This should free the dynamic array
  x := nil;
end.
