{$MODE objfpc}
unit bug856u;
interface
type
  TMyClass = class
  protected
    constructor Create(x: Integer);
  end;

implementation

constructor TMyClass.Create(x: Integer);
begin
end;

end.