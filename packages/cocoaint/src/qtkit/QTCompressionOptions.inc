{ Parsed from QTKit.framework QTCompressionOptions.h }


{$ifdef TYPES}
type
  QTCompressionOptionsPtr = ^QTCompressionOptions;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef CLASSES}

type
  QTCompressionOptions = objcclass external (NSObject)
  private
    _internal: QTCompressionOptionsInternal;
    _reserved: clong;
  public
    class function compressionOptionsIdentifiersForMediaType (mediaType: NSString): NSArray; message 'compressionOptionsIdentifiersForMediaType:';
    class function compressionOptionsWithIdentifier (identifier: NSString): id; message 'compressionOptionsWithIdentifier:';
    function mediaType: NSString; message 'mediaType';
    function localizedDisplayName: NSString; message 'localizedDisplayName';
    function localizedCompressionOptionsSummary: NSString; message 'localizedCompressionOptionsSummary';
    function isEqualToCompressionOptions (compressionOptions: QTCompressionOptions): ObjCBOOL; message 'isEqualToCompressionOptions:';
  end;
{$endif}

{$endif}
