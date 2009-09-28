{ Parsed from Appkit.framework NSInterfaceStyle.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }


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
function NSInterfaceStyleForKey(keyPointer: NSString; responderPointer: NSResponder): NSInterfaceStyle; cdecl; external name 'NSInterfaceStyleForKey';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSINTERFACESTYLE_PAS_S}
{$define NSINTERFACESTYLE_PAS_S}

{$endif}
{$endif}
