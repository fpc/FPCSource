{ Parsed from Automator.framework AMBundleAction.h }


{$ifdef TYPES}
type
  AMBundleActionPtr = ^AMBundleAction;
{$endif}

{$ifdef CLASSES}

type
  AMBundleAction = objcclass external (AMAction, NSCodingProtocol, NSCopyingProtocol)
  private
    _view: NSView;
    _bundle: NSBundle;
    _topLevelObjects: NSArray;
    _reserved: id;
    _reserved2: id;
    _reserved3: id;
    _reserved4: id;
  public
    function initWithDefinition_fromArchive (dict: NSDictionary; archived: ObjCBOOL): instancetype; message 'initWithDefinition:fromArchive:';
    procedure awakeFromBundle; message 'awakeFromBundle';
    function hasView: ObjCBOOL; message 'hasView';
    function view: NSView; message 'view';
    function bundle: NSBundle; message 'bundle';
    procedure setParameters(newValue: NSMutableDictionary); message 'setParameters:';
    function parameters: NSMutableDictionary; message 'parameters';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

