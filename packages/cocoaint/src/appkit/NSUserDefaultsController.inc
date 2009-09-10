{ Parsed from Appkit.framework NSUserDefaultsController.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSUSERDEFAULTSCONTROLLER_PAS_H}
{$define NSUSERDEFAULTSCONTROLLER_PAS_H}
type
  NSUserDefaultsControllerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSUSERDEFAULTSCONTROLLER_PAS_T}
{$define NSUSERDEFAULTSCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSUSERDEFAULTSCONTROLLER_PAS_R}
{$define NSUSERDEFAULTSCONTROLLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSUSERDEFAULTSCONTROLLER_PAS_F}
{$define NSUSERDEFAULTSCONTROLLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSUSERDEFAULTSCONTROLLER_PAS_T}
{$define NSUSERDEFAULTSCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSUserDefaultsController = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSUSERDEFAULTSCONTROLLER_PAS_C}
{$define NSUSERDEFAULTSCONTROLLER_PAS_C}

{ NSUserDefaultsController }
  NSUserDefaultsController = objcclass(NSController)
  private
    __reserved3: Pointer;
    __reserved4: Pointer;
    __defaults: NSUserDefaults;
    __valueBuffer: NSMutableDictionary;
    __initialValues: NSDictionary;
    __userDefaultsControllerFlags: bitpacked record
        _sharedInstance: 0..1;
        _appliesImmediately: 0..1;
        _reservedUserDefaultsController: 0..((1 shl 30)-1);
      end;
    
  public
    class function alloc: NSUserDefaultsController; message 'alloc';

    class function sharedUserDefaultsController: id; message 'sharedUserDefaultsController';
    function initWithDefaults_initialValues(defaults_: NSUserDefaults; initialValues_: NSDictionary): id; message 'initWithDefaults:initialValues:';
    function defaults: NSUserDefaults; message 'defaults';
    procedure setInitialValues(initialValues_: NSDictionary); message 'setInitialValues:';
    function initialValues: NSDictionary; message 'initialValues';
    procedure setAppliesImmediately(flag: Boolean); message 'setAppliesImmediately:';
    function appliesImmediately: Boolean; message 'appliesImmediately';
    function hasUnappliedChanges: Boolean; message 'hasUnappliedChanges';
    function values: id; message 'values';
    procedure revert(sender: id); message 'revert:';
    procedure save(sender: id); message 'save:';
    procedure revertToInitialValues(sender: id); message 'revertToInitialValues:';
  end; external;

{$endif}
{$endif}
