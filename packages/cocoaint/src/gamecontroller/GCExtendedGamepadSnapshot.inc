{ Parsed from GameController.framework GCExtendedGamepadSnapshot.h }


{$ifdef TYPES}
type
  GCExtendedGamepadSnapshotPtr = ^GCExtendedGamepadSnapshot;
{$endif}

{$ifdef CLASSES}

type
  GCExtendedGamepadSnapshot = objcclass external (GCExtendedGamepad)
  public
    procedure setSnapshotData(newValue: NSData); message 'setSnapshotData:';
    function snapshotData: NSData; message 'snapshotData';
    function initWithSnapshotData (data: NSData): instancetype; message 'initWithSnapshotData:';
    function initWithController_snapshotData (controller__: GCController; data: NSData): instancetype; message 'initWithController:snapshotData:';
  end;
{$endif}

{$ifdef TYPES}
type
  GCExtendedGamepadSnapShotDataV100 = record
    version: cuint16;
    size: cuint16;
    dpadX: cfloat;
    dpadY: cfloat;
    buttonA: cfloat;
    buttonB: cfloat;
    buttonX: cfloat;
    buttonY: cfloat;
    leftShoulder: cfloat;
    rightShoulder: cfloat;
    leftThumbstickX: cfloat;
    leftThumbstickY: cfloat;
    rightThumbstickX: cfloat;
    rightThumbstickY: cfloat;
    leftTrigger: cfloat;
    rightTrigger: cfloat;
  end;
type
  GCExtendedGamepadSnapShotDataV100Ptr = ^GCExtendedGamepadSnapShotDataV100;
{$endif}

{$ifdef FUNCTIONS}
function GCExtendedGamepadSnapShotDataV100FromNSData(snapshotData: GCExtendedGamepadSnapShotDataV100Ptr; data: NSData): ObjCBOOL; cdecl; external;
function NSDataFromGCExtendedGamepadSnapShotDataV100(snapshotData: GCExtendedGamepadSnapShotDataV100Ptr): NSData; cdecl; external;
{$endif}

