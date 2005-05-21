{$ifdef fpc}{$mode delphi}{$endif}

type
  c=class
   function Byte: Byte; virtual; abstract;
   function P(b: Byte):boolean; virtual; abstract;
  end;

begin
end.
