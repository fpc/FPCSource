{ Old file: tbs0337.pp }
{  }

program vartest;

{$ifdef fpc}{$mode objfpc}{$endif}

uses
  Classes;

type
  TMyComponent = class(TComponent)
    aaaaaaaaaa: TComponent;
    b: TComponent;
  private
  public
    constructor Create(AOwner: TComponent); override;
  end;


constructor TMyComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  aaaaaaaaaa := TComponent.Create(Self);
end;

var
  MyComponent: TMyComponent;

begin
  MyComponent := TMyComponent.Create(nil);
end.
