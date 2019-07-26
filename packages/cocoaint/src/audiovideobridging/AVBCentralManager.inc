{ Parsed from AudioVideoBridging.framework AVBCentralManager.h }


{$ifdef TYPES}
type
  AVBCentralManagerPtr = ^AVBCentralManager;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  AVBNullEUI64: cuint64; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  AVBCentralManager = objcclass external (NSObject)
  private
    {$ifndef _LP64}
    _impl: pointer;
    {$endif}
  public
    procedure startControllerMatching; message 'startControllerMatching';
    procedure didAddInterface (interface_: AVBInterface); message 'didAddInterface:';
    procedure didRemoveInterface (interface_: AVBInterface); message 'didRemoveInterface:';
    function streamingEnabledInterfacesOnly: ObjCBOOL; message 'streamingEnabledInterfacesOnly';
    class function nextAvailableDynamicEntityID: cuint64; message 'nextAvailableDynamicEntityID';
    class procedure releaseDynamicEntityID (entityID: cuint64); message 'releaseDynamicEntityID:';
    class function nextAvailableDynamicEntityModelID: cuint64; message 'nextAvailableDynamicEntityModelID';
    class procedure releaseDynamicEntityModelID (entityModelID: cuint64); message 'releaseDynamicEntityModelID:';
  end;
{$endif}

