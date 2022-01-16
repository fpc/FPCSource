{ Parsed from GameController.framework GCGamepadSnapshot.h }


{$ifdef TYPES}
type
  GCGamepadSnapshotPtr = ^GCGamepadSnapshot;
{$endif}

{$ifdef CLASSES}

type
  GCGamepadSnapshot = objcclass external (GCGamepad)
  public
    procedure setSnapshotData(newValue: NSData); message 'setSnapshotData:';
    function snapshotData: NSData; message 'snapshotData';
    function initWithSnapshotData (data: NSData): instancetype; message 'initWithSnapshotData:';
    function initWithController_snapshotData (controller__: GCController; data: NSData): instancetype; message 'initWithController:snapshotData:';
  end;
{$endif}

{$ifdef TYPES}
type
  GCGamepadSnapShotDataV100 = record
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
  end;
type
  GCGamepadSnapShotDataV100Ptr = ^GCGamepadSnapShotDataV100;
{$endif}

{$ifdef FUNCTIONS}
function GCGamepadSnapShotDataV100FromNSData(snapshotData: GCGamepadSnapShotDataV100Ptr; data: NSData): ObjCBOOL; cdecl; external;
function NSDataFromGCGamepadSnapShotDataV100(snapshotData: GCGamepadSnapShotDataV100Ptr): NSData; cdecl; external;
{$endif}

