{ Parsed from CloudKit.framework CKAsset.h }


{$ifdef TYPES}
type
  CKAssetPtr = ^CKAsset;
{$endif}

{$ifdef CLASSES}

type
  CKAsset = objcclass external (NSObject)
  public
    function init: instancetype; message 'init';
    function initWithFileURL (fileURL: NSURL): instancetype; message 'initWithFileURL:';
    function fileURL: NSURL; message 'fileURL';
  end;
{$endif}

