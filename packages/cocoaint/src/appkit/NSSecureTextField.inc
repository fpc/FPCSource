{ Parsed from AppKit.framework NSSecureTextField.h }


{$ifdef TYPES}
type
  NSSecureTextFieldPtr = ^NSSecureTextField;
  NSSecureTextFieldCellPtr = ^NSSecureTextFieldCell;
{$endif}

{$ifdef CLASSES}

type
  NSSecureTextField = objcclass external (NSTextField)
  private
  end;

type
  NSSecureTextFieldCell = objcclass external (NSTextFieldCell)
  private
    _echosBullets: ObjCBOOL;
    _csMode: ObjCBOOL;
  public
    procedure setEchosBullets(newValue: ObjCBOOL); message 'setEchosBullets:';
    function echosBullets: ObjCBOOL; message 'echosBullets';
  end;
{$endif}

