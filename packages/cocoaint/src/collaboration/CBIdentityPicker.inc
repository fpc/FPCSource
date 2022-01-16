{ Parsed from Collaboration.framework CBIdentityPicker.h }


{$ifdef TYPES}
type
  CBIdentityPickerPtr = ^CBIdentityPicker;
{$endif}

{$ifdef CLASSES}

type
  CBIdentityPicker = objcclass external (NSObject)
  private
    _reserved:array[0..1] of id;
  public
    procedure setTitle (title: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setAllowsMultipleSelection (flag: ObjCBOOL); message 'setAllowsMultipleSelection:';
    function allowsMultipleSelection: ObjCBOOL; message 'allowsMultipleSelection';
    function runModal: NSInteger; message 'runModal';
    procedure runModalForWindow_modalDelegate_didEndSelector_contextInfo (window: NSWindow; delegate: id; didEndSelector: SEL; contextInfo: pointer); message 'runModalForWindow:modalDelegate:didEndSelector:contextInfo:';
    function identities: NSArray; message 'identities';
  end;
{$endif}

