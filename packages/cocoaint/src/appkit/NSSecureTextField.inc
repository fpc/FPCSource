{ Parsed from Appkit.framework NSSecureTextField.h }

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
  NSSecureTextFieldPtr = NSSecureTextFieldPointer;
  NSSecureTextFieldCell = objcclass;
  NSSecureTextFieldCellPointer = ^NSSecureTextFieldCell;
  NSSecureTextFieldCellPtr = NSSecureTextFieldCellPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSECURETEXTFIELD_PAS_C}
{$define NSSECURETEXTFIELD_PAS_C}

{ NSSecureTextField }
  NSSecureTextField = objcclass external (NSTextField)
    
  public
  end;

{ NSSecureTextFieldCell }
  NSSecureTextFieldCell = objcclass external (NSTextFieldCell)
  private
    _echosBullets: Boolean;
    
  public
    procedure setEchosBullets(flag: Boolean); message 'setEchosBullets:';
    function echosBullets: Boolean; message 'echosBullets';
  end;

{$endif}
{$endif}
