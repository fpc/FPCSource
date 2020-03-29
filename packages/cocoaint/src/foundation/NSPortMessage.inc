{ Parsed from Foundation.framework NSPortMessage.h }


{$ifdef TYPES}
type
  NSPortMessagePtr = ^NSPortMessage;
{$endif}

{$ifdef CLASSES}

type
  NSPortMessage = objcclass external (NSObject)
  private
    localPort: NSPort;
    remotePort: NSPort;
    components: NSMutableArray;
    msgid: cuint32;
    reserved2: pointer;
    reserved: pointer;
  public
    function initWithSendPort_receivePort_components (sendPort: NSPort; replyPort: NSPort; components_: NSArray): instancetype; message 'initWithSendPort:receivePort:components:'; { NS_DESIGNATED_INITIALIZER }
    function components_: NSArray; message 'components';
    function receivePort: NSPort; message 'receivePort';
    function sendPort: NSPort; message 'sendPort';
    function sendBeforeDate (date: NSDate): ObjCBOOL; message 'sendBeforeDate:';
    procedure setMsgid(newValue: cuint32); message 'setMsgid:';
    function msgid_: cuint32; message 'msgid';
  end;
{$endif}

