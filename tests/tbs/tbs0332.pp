{$MODE objfpc}
uses Classes;
var
  o: TComponent;
  begin
    o := TComponent(TComponent.NewInstance);
    o.Create(nil);
    o.Free;
  end.


