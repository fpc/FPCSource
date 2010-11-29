unit uw18087b; 

interface

{$mode delphi}

uses
  uw18087a;

type
  TFoo2 = class
  type
    TFoo3 = class(TFoo1)
    protected
      procedure Proc1; override; // was error: There is no method in an ancestor class to be overridden: "TFoo2.TFoo3.Proc1;"
    end;
  end;

implementation

procedure TFoo2.TFoo3.Proc1;
begin
end;

end.