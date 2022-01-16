{ Parsed from AppKit.framework NSStatusBarButton.h }


{$ifdef TYPES}
type
  NSStatusBarButtonPtr = ^NSStatusBarButton;
{$endif}

{$ifdef CLASSES}

type
  NSStatusBarButton = objcclass external (NSButton)
  private
    _statusBarButtonPrivate: id;
  public
    procedure setAppearsDisabled(newValue: ObjCBOOL); message 'setAppearsDisabled:';
    function appearsDisabled: ObjCBOOL; message 'appearsDisabled';
  end;
{$endif}

