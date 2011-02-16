{ Parsed from Appkit.framework NSInterfaceStyle.h }

{$ifdef TYPES}
{$ifndef NSINTERFACESTYLE_PAS_T}
{$define NSINTERFACESTYLE_PAS_T}

{ Constants }

const
  NSNoInterfaceStyle = 0;
  NSNextStepInterfaceStyle = 1;
  NSWindows95InterfaceStyle = 2;
  NSMacintoshInterfaceStyle = 3;

{ Types }
type
  NSInterfaceStyle = NSUInteger;
  NSInterfaceStylePtr = ^NSInterfaceStyle;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSINTERFACESTYLE_PAS_R}
{$define NSINTERFACESTYLE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSINTERFACESTYLE_PAS_F}
{$define NSINTERFACESTYLE_PAS_F}

{ Functions }
function NSInterfaceStyleForKey(key: NSString; responder: NSResponder): NSInterfaceStyle; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSINTERFACESTYLE_PAS_S}
{$define NSINTERFACESTYLE_PAS_S}

{ External string constants }
var
  NSInterfaceStyleDefault: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSINTERFACESTYLE_PAS_C}
{$define NSINTERFACESTYLE_PAS_C}

{ NSInterfaceStyleCategory }
  NSInterfaceStyleCategory = objccategory external (NSResponder)
    function interfaceStyle: NSInterfaceStyle; message 'interfaceStyle';
    procedure setInterfaceStyle(interfaceStyle: NSInterfaceStyle); message 'setInterfaceStyle:';
  end;

{$endif}
{$endif}
