{ Parsed from EventKit.framework EKObject.h }


{$ifdef TYPES}
type
  EKObjectPtr = ^EKObject;
{$endif}

{$ifdef CLASSES}

type
  EKObject = objcclass external (NSObject)
  private
  public
    function hasChanges: ObjCBOOL; message 'hasChanges';
    function isNew: ObjCBOOL; message 'isNew';
    procedure reset; message 'reset';
    procedure rollback; message 'rollback';
    function refresh: ObjCBOOL; message 'refresh';
  end;
{$endif}

