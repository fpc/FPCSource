{ %fail }

{$ifdef fpc}{$mode delphi}{$endif}

type
  IA=interface
   function copy:String;
  end;

  IB=interface(IA)
   function copy:integer;
  end;

  to1 = class(tinterfacedobject,ia,ib)
    function copy:string;
  end;

function to1.copy:string;
  begin
  end;

begin
end.
