{ Parsed from AppKit.framework NSUserDefaultsController.h }


{$ifdef TYPES}
type
  NSUserDefaultsControllerPtr = ^NSUserDefaultsController;
{$endif}

{$ifdef CLASSES}

type
  NSUserDefaultsController = objcclass external (NSController)
  private
    {$if  not defined(__LP64__)}
    _reserved3: pointer;
    _reserved4: pointer;
    {$endif}
    _defaults: NSUserDefaults;
    _valueBuffer: NSMutableDictionary;
    _initialValues: NSDictionary;
    _userDefaultsControllerFlags: bitpacked record
      case byte of
        0: (_anonBitField__userDefaultsControllerFlags0: cuint);
        1: (
          _sharedInstance: 0..1;
          _appliesImmediately: 0..1;
          _reservedUserDefaultsController: 0..((1 shl 30)-1);
        );
      end;
  public
    class function sharedUserDefaultsController: NSUserDefaultsController; message 'sharedUserDefaultsController';
    function initWithDefaults_initialValues (defaults: NSUserDefaults; initialValues: NSDictionary): instancetype; message 'initWithDefaults:initialValues:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function defaults: NSUserDefaults; message 'defaults';
    procedure setInitialValues(newValue: NSDictionary); message 'setInitialValues:';
    function initialValues: NSDictionary; message 'initialValues';
    procedure setAppliesImmediately(newValue: ObjCBOOL); message 'setAppliesImmediately:';
    function appliesImmediately: ObjCBOOL; message 'appliesImmediately';
    function hasUnappliedChanges: ObjCBOOL; message 'hasUnappliedChanges';
    function values: id; message 'values';
    procedure revert (sender: id); message 'revert:';
    procedure save (sender: id); message 'save:';
    procedure revertToInitialValues (sender: id); message 'revertToInitialValues:';
  end;
{$endif}

