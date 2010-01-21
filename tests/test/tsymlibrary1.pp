{$ifdef fpc}
{$mode objfpc}
{$endif}
type
  TSomething = class
  public
    class procedure DoSomething; library;
  end;

class procedure TSomething.DoSomething;
begin
end;

begin
  TSomething.DoSomething;
end.