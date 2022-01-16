{ Parsed from QuickLookUI.framework QLPreviewItem.h }


{$ifdef TYPES}
type
  QLPreviewItemProtocolPtr = ^QLPreviewItemProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  QLPreviewItemProtocol = objcprotocol external name 'QLPreviewItem' (NSObjectProtocol)
  required
    function previewItemURL: NSURL; message 'previewItemURL';
  optional
    function previewItemTitle: NSString; message 'previewItemTitle';
    function previewItemDisplayState: id; message 'previewItemDisplayState';
  end;
{$endif}

{$ifdef CLASSES}

type
  QLPreviewConvenienceAdditions = objccategory external (NSURL)
  end;
{$endif}

