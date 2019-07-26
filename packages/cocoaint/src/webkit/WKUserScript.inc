{ Parsed from WebKit.framework WKUserScript.h }


{$ifdef TYPES}
type
  WKUserScriptPtr = ^WKUserScript;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef TYPES}
type
  WKUserScriptInjectionTime = NSInteger;
  WKUserScriptInjectionTimePtr = ^WKUserScriptInjectionTime;

const
  WKUserScriptInjectionTimeAtDocumentStart = 0;
  WKUserScriptInjectionTimeAtDocumentEnd = 1;
{$endif}

{$ifdef CLASSES}

type
  WKUserScript = objcclass external (NSObject, NSCopyingProtocol)
  public
    function source: NSString; message 'source';
    function injectionTime: WKUserScriptInjectionTime; message 'injectionTime';
    function isForMainFrameOnly: ObjCBOOL; message 'isForMainFrameOnly';
    function initWithSource_injectionTime_forMainFrameOnly (source_: NSString; injectionTime_: WKUserScriptInjectionTime; forMainFrameOnly: ObjCBOOL): instancetype; message 'initWithSource:injectionTime:forMainFrameOnly:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$endif}
