{ Parsed from GameController.framework GCControllerElement.h }


{ Types from GCControllerElement }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  GCControllerElementPtr = ^GCControllerElement;
{$endif}

{$ifdef CLASSES}

type
  GCControllerElement = objcclass external (NSObject)
  public
    {$if  not defined(__has_feature)}
    function collection: GCControllerElement; message 'collection';
    {$else}
    function collection_: GCControllerElement; message 'collection';
    {$endif}
    function isAnalog: ObjCBOOL; message 'isAnalog';
  end;
{$endif}

