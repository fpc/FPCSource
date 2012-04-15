unit uw20875a;

{$MODE Delphi}
{$modeswitch ObjectiveC1}

interface

uses
  CocoaAll;

type
  TController1 = objcclass(NSWindowController, NSWindowDelegateProtocol)
  public
    function init : id; override;
  end;

implementation

function TController1.init : id;
begin
  Result := inherited init;
end;

end.
