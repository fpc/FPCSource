{ Parsed from StoreKit.framework SKPaymentTransaction.h }


{$ifdef TYPES}
type
  SKPaymentTransactionPtr = ^SKPaymentTransaction;
{$endif}

{$ifdef TYPES}
const
  SKPaymentTransactionStatePurchasing = 0;
  SKPaymentTransactionStatePurchased = 1;
  SKPaymentTransactionStateFailed = 2;
  SKPaymentTransactionStateRestored = 3;
  SKPaymentTransactionStateDeferred = 4;

type
  SKPaymentTransactionState = NSInteger;
  SKPaymentTransactionStatePtr = ^SKPaymentTransactionState;
{$endif}

{$ifdef CLASSES}

type
  SKPaymentTransaction = objcclass external (NSObject)
  private
    _internal: id;
  public
    function error: NSError; message 'error';
    function originalTransaction: SKPaymentTransaction; message 'originalTransaction';
    function payment: SKPayment; message 'payment';
    function transactionDate: NSDate; message 'transactionDate';
    function transactionIdentifier: NSString; message 'transactionIdentifier';
    function downloads: NSArray; message 'downloads';
    function transactionState: SKPaymentTransactionState; message 'transactionState';
  end;
{$endif}

