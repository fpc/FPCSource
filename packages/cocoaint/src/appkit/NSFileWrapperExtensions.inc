{ Parsed from AppKit.framework NSFileWrapperExtensions.h }

{$ifdef CLASSES}

type
  NSExtensions = objccategory external (NSFileWrapper)
    procedure setIcon(newValue: NSImage); message 'setIcon:';
    function icon: NSImage; message 'icon';
  end;
{$endif}

