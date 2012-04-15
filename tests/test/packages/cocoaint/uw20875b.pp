unit uw20875b;

{$MODE Delphi}
{$modeswitch ObjectiveC1}

interface

uses
  CocoaAll;

type
  TController2 = objcclass(NSWindowController, NSWindowDelegateProtocol)
  public
    function init : id; override;
  end;

implementation

function TController2.init : id;
begin
  Result := inherited init;
end;

end.
