{ Parsed from Appkit.framework NSSecureTextField.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSSECURETEXTFIELD_PAS_T}
{$define NSSECURETEXTFIELD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSECURETEXTFIELD_PAS_R}
{$define NSSECURETEXTFIELD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSECURETEXTFIELD_PAS_F}
{$define NSSECURETEXTFIELD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSECURETEXTFIELD_PAS_S}
{$define NSSECURETEXTFIELD_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSecureTextField = objcclass;
  NSSecureTextFieldPointer = ^NSSecureTextField;
  NSSecureTextFieldCell = objcclass;
  NSSecureTextFieldCellPointer = ^NSSecureTextFieldCell;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSECURETEXTFIELD_PAS_C}
{$define NSSECURETEXTFIELD_PAS_C}

{ NSSecureTextField }
  NSSecureTextField = objcclass(NSTextField)
    
  public
    class function alloc: NSSecureTextField; message 'alloc';
  end; external;

{ NSSecureTextFieldCell }
  NSSecureTextFieldCell = objcclass(NSTextFieldCell)
  private
    __echosBullets: Boolean;
    
  public
    class function alloc: NSSecureTextFieldCell; message 'alloc';

    procedure setEchosBullets(flag: Boolean); message 'setEchosBullets:';
    function echosBullets: Boolean; message 'echosBullets';
  end; external;

{$endif}
{$endif}
