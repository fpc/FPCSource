{ Parsed from QTKit.framework QTDataReference.h }


{$ifdef TYPES}
type
  QTDataReferencePtr = ^QTDataReference;
{$endif}

{$ifdef CLASSES}

type
  QTDataReference = objcclass external (NSObject, NSCodingProtocol)
  private
    {$if defined(__LP64__)}
    _proxy: cint32;
    {$else}
    _fileName: NSString;
    _url: NSURL;
    _data: NSData;
    _name: NSString;
    _MIMEType: NSString;
    _dataRef: Handle;
    _dataRefType: OSType;
    {$endif}
    _reserved1: clong;
    _reserved2: clong;
    _reserved3: clong;
  public
    class function dataReferenceWithDataRef_type (dataRef: Handle; type_: NSString): id; message 'dataReferenceWithDataRef:type:';
    class function dataReferenceWithDataRefData_type (dataRefData: NSData; type_: NSString): id; message 'dataReferenceWithDataRefData:type:';
    class function dataReferenceWithReferenceToFile (fileName: NSString): id; message 'dataReferenceWithReferenceToFile:';
    class function dataReferenceWithReferenceToURL (url: NSURL): id; message 'dataReferenceWithReferenceToURL:';
    class function dataReferenceWithReferenceToData (data: NSData): id; message 'dataReferenceWithReferenceToData:';
    class function dataReferenceWithReferenceToData_name_MIMEType (data: NSData; name: NSString; MIMEType: NSString): id; message 'dataReferenceWithReferenceToData:name:MIMEType:';
    function initWithDataRef_type (dataRef: Handle; type_: NSString): id; message 'initWithDataRef:type:';
    function initWithDataRefData_type (dataRefData: NSData; type_: NSString): id; message 'initWithDataRefData:type:';
    function initWithReferenceToFile (fileName: NSString): id; message 'initWithReferenceToFile:';
    function initWithReferenceToURL (url: NSURL): id; message 'initWithReferenceToURL:';
    function initWithReferenceToData (data: NSData): id; message 'initWithReferenceToData:';
    function initWithReferenceToData_name_MIMEType (data: NSData; name: NSString; MIMEType: NSString): id; message 'initWithReferenceToData:name:MIMEType:';
    function dataRef: Handle; message 'dataRef';
    function dataRefData: NSData; message 'dataRefData'; 
    function dataRefType: NSString; message 'dataRefType';
    function referenceFile: NSString; message 'referenceFile';
    function referenceURL: NSURL; message 'referenceURL';
    function referenceData: NSData; message 'referenceData';
    function name: NSString; message 'name';
    function MIMEType: NSString; message 'MIMEType';
    procedure setDataRef (dataRef: Handle); message 'setDataRef:';
    procedure setDataRefType (type_: NSString); message 'setDataRefType:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

