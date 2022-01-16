{ Parsed from StoreKit.framework SKReceiptRefreshRequest.h }


{$ifdef TYPES}
type
  SKReceiptRefreshRequestPtr = ^SKReceiptRefreshRequest;
{$endif}

{$ifdef CLASSES}

type
  SKReceiptRefreshRequest = objcclass external (SKRequest)
  private
    _properties: NSDictionary;
  public
    function initWithReceiptProperties (properties: NSDictionary): id; message 'initWithReceiptProperties:';
    function receiptProperties: NSDictionary; message 'receiptProperties';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  SKReceiptPropertyIsExpired: NSString; cvar; external;
  SKReceiptPropertyIsRevoked: NSString; cvar; external;
  SKReceiptPropertyIsVolumePurchase: NSString; cvar; external;
{$endif}

