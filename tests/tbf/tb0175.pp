{ %fail }
{$ifdef fpc}
  {$Mode Delphi}
{$endif}

unit tb0175;

interface

      function getvar: string;

implementation

   var
     myvar : string;

   procedure getvar;
   begin
      result := myvar;
   end;

end.
