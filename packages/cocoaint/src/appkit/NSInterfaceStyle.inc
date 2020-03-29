{ Parsed from AppKit.framework NSInterfaceStyle.h }

{$ifdef TYPES}
const
  NSNoInterfaceStyle = 0;
  NSNextStepInterfaceStyle = 1;
  NSWindows95InterfaceStyle = 2;
  NSMacintoshInterfaceStyle = 3;

type
  NSInterfaceStyle = NSUInteger deprecated 'in 10_0, 10_8';
  NSInterfaceStylePtr = ^NSInterfaceStyle deprecated 'in 10_0, 10_8';
{$endif}

{$ifdef FUNCTIONS}
function NSInterfaceStyleForKey(key: NSString; responder: NSResponder): NSInterfaceStyle; cdecl; external; deprecated 'in 10_0, 10_8';
{$endif}

{$ifdef CLASSES}

type
  NSResponder_NSInterfaceStyle = objccategory external name 'NSInterfaceStyle' (NSResponder)
    function interfaceStyle: NSInterfaceStyle; message 'interfaceStyle'; deprecated 'in 10_0, 10_8';
    procedure setInterfaceStyle (interfaceStyle_: NSInterfaceStyle); message 'setInterfaceStyle:'; deprecated 'in 10_0, 10_8';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSInterfaceStyleDefault: NSString deprecated 'in 10_0, 10_8'; cvar; external;
{$endif}

