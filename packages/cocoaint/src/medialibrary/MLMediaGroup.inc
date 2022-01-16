{ Parsed from MediaLibrary.framework MLMediaGroup.h }


{$ifdef TYPES}
type
  MLMediaGroupPtr = ^MLMediaGroup;
{$endif}

{$ifdef CLASSES}

type
  MLMediaGroup = objcclass external (NSObject)
  public
    function mediaLibrary: MLMediaLibrary; message 'mediaLibrary';
    function parent: MLMediaGroup; message 'parent';
    function mediaSourceIdentifier: NSString; message 'mediaSourceIdentifier';
    function name: NSString; message 'name';
    function identifier: NSString; message 'identifier';
    function typeIdentifier: NSString; message 'typeIdentifier';
    function attributes: NSDictionary; message 'attributes';
    function childGroups: NSArray; message 'childGroups';
    function URL: NSURL; message 'URL';
    function modificationDate: NSDate; message 'modificationDate';
    function iconImage: NSImage; message 'iconImage';
    function mediaObjects: NSArray; message 'mediaObjects';
  end;
{$endif}

