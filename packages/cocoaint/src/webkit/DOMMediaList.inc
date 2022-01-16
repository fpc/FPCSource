{ Parsed from WebKit.framework DOMMediaList.h }


{$ifdef TYPES}
type
  DOMMediaListPtr = ^DOMMediaList;
{$endif}

{$ifdef CLASSES}

type
  DOMMediaList = objcclass external (DOMObject)
  public
    procedure setMediaText(newValue: NSString); message 'setMediaText:';
    function mediaText: NSString; message 'mediaText';
    function length: cuint; message 'length';
    function item (index: cuint): NSString; message 'item:';
    procedure deleteMedium (oldMedium: NSString); message 'deleteMedium:';
    procedure appendMedium (newMedium: NSString); message 'appendMedium:';
  end;
{$endif}

