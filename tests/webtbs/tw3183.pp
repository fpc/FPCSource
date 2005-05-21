{$ifdef fpc}{$mode delphi}{$endif}

type
  IA=interface
   function copy:String;
  end;

  IB=interface(IA)
   function copy:integer;
  end;

begin
end.
