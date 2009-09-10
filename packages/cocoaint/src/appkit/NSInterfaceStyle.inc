{ Parsed from Appkit.framework NSInterfaceStyle.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSINTERFACESTYLE_PAS_T}
{$define NSINTERFACESTYLE_PAS_T}

{ Constants }

const
  NSNextStepInterfaceStyle = 1;
  NSWindows95InterfaceStyle = 2;
  NSMacintoshInterfaceStyle = 3;

{ Types }
type
  NSInterfaceStyle = culong;

{ CFString constants }
var
  NSInterfaceStyleDefault: CFStringRef; external name '_NSInterfaceStyleDefault';

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
function NSInterfaceStyleForKey(var key: NSString; var responder: NSResponder): NSInterfaceStyle; cdecl; external name 'NSInterfaceStyleForKey';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSINTERFACESTYLE_PAS_T}
{$define NSINTERFACESTYLE_PAS_T}

{$endif}
{$endif}
