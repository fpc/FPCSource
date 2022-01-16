{ Parsed from AppKit.framework NSPathControlItem.h }


{$ifdef TYPES}
type
  NSPathControlItemPtr = ^NSPathControlItem;
{$endif}

{$ifdef CLASSES}

type
  NSPathControlItem = objcclass external (NSObject)
  private
    _secretCell: NSPathComponentCell;
  public
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setAttributedTitle(newValue: NSAttributedString); message 'setAttributedTitle:';
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    function URL: NSURL; message 'URL';
  end;
{$endif}

