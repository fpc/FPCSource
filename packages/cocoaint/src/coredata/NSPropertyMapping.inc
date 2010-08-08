{ Parsed from Coredata.framework NSPropertyMapping.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Fri Oct 2 11:46:33 EDT 2009 }


{$ifdef TYPES}
{$ifndef NSPROPERTYMAPPING_PAS_T}
{$define NSPROPERTYMAPPING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPROPERTYMAPPING_PAS_R}
{$define NSPROPERTYMAPPING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPROPERTYMAPPING_PAS_F}
{$define NSPROPERTYMAPPING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPROPERTYMAPPING_PAS_S}
{$define NSPROPERTYMAPPING_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPropertyMapping = objcclass;
  NSPropertyMappingPointer = ^NSPropertyMapping;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPROPERTYMAPPING_PAS_C}
{$define NSPROPERTYMAPPING_PAS_C}

{ NSPropertyMapping }
  NSPropertyMapping = objcclass(NSObject)
  private
    __reserved: Pointer;
    __reserved1: Pointer;
    __reserved2: Pointer;
    __name: NSString;
    __valueExpression: NSExpression;
    __userInfo: NSDictionary;
    __propertyMappingFlags: bitpacked record
        _isInUse: 0..1;
        _reservedPropertyMapping: 0..((1 shl 31)-1);
      end;
    
  public
    class function alloc: NSPropertyMapping; message 'alloc';

    function name: NSString; message 'name';
    procedure setName(name_: NSString); message 'setName:';
    function valueExpression: NSExpression; message 'valueExpression';
    procedure setValueExpression(expression: NSExpression); message 'setValueExpression:';
    function userInfo: NSDictionary; message 'userInfo';
    procedure setUserInfo(userInfo_: NSDictionary); message 'setUserInfo:';
  end; external;

{$endif}
{$endif}
