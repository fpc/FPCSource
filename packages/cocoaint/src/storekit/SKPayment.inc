{ Parsed from StoreKit.framework SKPayment.h }


{$ifdef TYPES}
type
  SKPaymentPtr = ^SKPayment;
  SKMutablePaymentPtr = ^SKMutablePayment;
{$endif}

{$ifdef CLASSES}

type
  SKPayment = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _internal: id;
  public
    class function paymentWithProduct (product: SKProduct): id; message 'paymentWithProduct:';
    function productIdentifier: NSString; message 'productIdentifier';
    function requestData: NSData; message 'requestData';
    function quantity: NSInteger; message 'quantity';
    function applicationUsername: NSString; message 'applicationUsername';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  SKMutablePayment = objcclass external (SKPayment)
  private
  public
    procedure setProductIdentifier(newValue: NSString); message 'setProductIdentifier:';
    function productIdentifier: NSString; message 'productIdentifier';
    procedure setQuantity(newValue: NSInteger); message 'setQuantity:';
    function quantity: NSInteger; message 'quantity';
    procedure setRequestData(newValue: NSData); message 'setRequestData:';
    function requestData: NSData; message 'requestData';
    procedure setApplicationUsername(newValue: NSString); message 'setApplicationUsername:';
    function applicationUsername: NSString; message 'applicationUsername';
  end;
{$endif}

