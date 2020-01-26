{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}

unit DefinedClassesStoreKit;
interface

type
  SKDownload = objcclass external;
  SKMutablePayment = objcclass external;
  SKPayment = objcclass external;
  SKPaymentQueue = objcclass external;
  SKPaymentTransaction = objcclass external;
  SKProduct = objcclass external;
  SKProductsRequest = objcclass external;
  SKProductsResponse = objcclass external;
  SKReceiptRefreshRequest = objcclass external;
  SKRequest = objcclass external;
  SKPaymentTransactionObserverProtocol = objcprotocol external name 'SKPaymentTransactionObserver';
  SKProductsRequestDelegateProtocol = objcprotocol external name 'SKProductsRequestDelegate';
  SKRequestDelegateProtocol = objcprotocol external name 'SKRequestDelegate';

implementation
end.
