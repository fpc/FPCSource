{ Parsed from StoreKit.framework SKProductsRequest.h }


{$ifdef TYPES}
type
  SKProductsRequestPtr = ^SKProductsRequest;
  SKProductsResponsePtr = ^SKProductsResponse;
  SKProductsRequestDelegateProtocolPtr = ^SKProductsRequestDelegateProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  SKProductsRequestDelegateProtocol = objcprotocol external name 'SKProductsRequestDelegate' (SKRequestDelegateProtocol)
  required
    procedure productsRequest_didReceiveResponse (request: SKProductsRequest; response: SKProductsResponse); message 'productsRequest:didReceiveResponse:';
  end;
{$endif}

{$ifdef CLASSES}

type
  SKProductsRequest = objcclass external (SKRequest)
  private
    _productsRequestInternal: id;
  public
    function initWithProductIdentifiers (productIdentifiers: NSSet): id; message 'initWithProductIdentifiers:';
    procedure setDelegate(newValue: SKProductsRequestDelegateProtocol); message 'setDelegate:';
    function delegate: SKProductsRequestDelegateProtocol; message 'delegate';
  end;

type
  SKProductsResponse = objcclass external (NSObject)
  private
    _internal: id;
  public
    function products: NSArray; message 'products';
    function invalidProductIdentifiers: NSArray; message 'invalidProductIdentifiers';
  end;
{$endif}

