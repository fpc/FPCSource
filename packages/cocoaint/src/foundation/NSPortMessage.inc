{ Parsed from Foundation.framework NSPortMessage.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSPORTMESSAGE_PAS_C}
{$define NSPORTMESSAGE_PAS_C}

{ NSPortMessage }
  NSPortMessage = objcclass(NSObject)
  private
    _localPort: NSPort;
    _remotePort: NSPort;
    _components: NSMutableArray;
    _msgid: cardinal;
    _reserved2: Pointer;
    _reserved: Pointer;
    
  public
    class function alloc: NSPortMessage; message 'alloc';

    function initWithSendPort_receivePort_components(sendPort_: NSPort; replyPort: NSPort; components_: NSArray): id; message 'initWithSendPort:receivePort:components:';
    function components: NSArray; message 'components';
    function receivePort: NSPort; message 'receivePort';
    function sendPort: NSPort; message 'sendPort';
    function sendBeforeDate(date: NSDate): Boolean; message 'sendBeforeDate:';
    function msgid: cardinal; message 'msgid';
    procedure setMsgid(msgid_: cardinal); message 'setMsgid:';
  end; external;

{$endif}
{$endif}
