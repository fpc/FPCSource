unit uw17950; 

{$mode delphi}

interface

implementation

type
  TFoo1 = class;

  TFoo1 = class
  public
    type
      TFoo2 = object
      private
        FField2: integer;
      end;
    public
      function GetFoo2: TFoo2;
  end;

function TFoo1.GetFoo2: TFoo2;
begin
  result.FField2 := 5;
end;

end.

