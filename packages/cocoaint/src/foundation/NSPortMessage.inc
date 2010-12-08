{ Parsed from Foundation.framework NSPortMessage.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSPORTMESSAGE_PAS_T}
{$define NSPORTMESSAGE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPORTMESSAGE_PAS_R}
{$define NSPORTMESSAGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPORTMESSAGE_PAS_F}
{$define NSPORTMESSAGE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPORTMESSAGE_PAS_S}
{$define NSPORTMESSAGE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPortMessage = objcclass;
  NSPortMessagePointer = ^NSPortMessage;
  NSPortMessagePtr = NSPortMessagePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPORTMESSAGE_PAS_C}
{$define NSPORTMESSAGE_PAS_C}

{ NSPortMessage }
  NSPortMessage = objcclass(NSObject)
  private
    localPort: NSPort;
    remotePort: NSPort;
    _components: NSMutableArray;
    _msgid: cuint32;
    reserved2: Pointer;
    reserved: Pointer;
    
  public
    class function alloc: NSPortMessage; message 'alloc';

    function initWithSendPort_receivePort_components(sendPort_: NSPort; replyPort: NSPort; components_: NSArray): id; message 'initWithSendPort:receivePort:components:';
    function components: NSArray; message 'components';
    function receivePort: NSPort; message 'receivePort';
    function sendPort: NSPort; message 'sendPort';
    function sendBeforeDate(date: NSDate): Boolean; message 'sendBeforeDate:';
    function msgid: cuint32; message 'msgid';
    procedure setMsgid(msgid_: cuint32); message 'setMsgid:';
  end; external;

{$endif}
{$endif}
