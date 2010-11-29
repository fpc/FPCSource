unit uw18087a;

interface

{$mode delphi}

type
  TFoo1 = class
  protected // it worked if "protected" was removed
    procedure Proc1; virtual;
  end;

implementation

  procedure TFoo1.Proc1;
  begin
  end;

end.