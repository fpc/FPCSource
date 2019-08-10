{ Parsed from StoreKit.framework SKPaymentQueue.h }


{$ifdef TYPES}
type
  SKPaymentQueuePtr = ^SKPaymentQueue;
  SKPaymentTransactionObserverProtocolPtr = ^SKPaymentTransactionObserverProtocol;
{$endif}

{$ifdef CLASSES}

type
  SKPaymentQueue = objcclass external (NSObject)
  private
    _internal: id;
  public
    class function defaultQueue: SKPaymentQueue; message 'defaultQueue';
    class function canMakePayments: ObjCBOOL; message 'canMakePayments';
    procedure addPayment (payment: SKPayment); message 'addPayment:';
    procedure restoreCompletedTransactions; message 'restoreCompletedTransactions';
    procedure restoreCompletedTransactionsWithApplicationUsername (username: NSString); message 'restoreCompletedTransactionsWithApplicationUsername:';
    procedure finishTransaction (transaction: SKPaymentTransaction); message 'finishTransaction:';
    procedure addTransactionObserver (observer: SKPaymentTransactionObserverProtocol); message 'addTransactionObserver:';
    procedure removeTransactionObserver (observer: SKPaymentTransactionObserverProtocol); message 'removeTransactionObserver:';
    function transactions: NSArray; message 'transactions';
    procedure startDownloads (downloads: NSArray); message 'startDownloads:';
    procedure pauseDownloads (downloads: NSArray); message 'pauseDownloads:';
    procedure resumeDownloads (downloads: NSArray); message 'resumeDownloads:';
    procedure cancelDownloads (downloads: NSArray); message 'cancelDownloads:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  SKPaymentTransactionObserverProtocol = objcprotocol external name 'SKPaymentTransactionObserver' (NSObjectProtocol)
  required
    procedure paymentQueue_updatedTransactions (queue: SKPaymentQueue; transactions: NSArray); message 'paymentQueue:updatedTransactions:';
  optional
    procedure paymentQueue_removedTransactions (queue: SKPaymentQueue; transactions: NSArray); message 'paymentQueue:removedTransactions:';
    procedure paymentQueue_restoreCompletedTransactionsFailedWithError (queue: SKPaymentQueue; error: NSError); message 'paymentQueue:restoreCompletedTransactionsFailedWithError:';
    procedure paymentQueueRestoreCompletedTransactionsFinished (queue: SKPaymentQueue); message 'paymentQueueRestoreCompletedTransactionsFinished:';
    procedure paymentQueue_updatedDownloads (queue: SKPaymentQueue; downloads: NSArray); message 'paymentQueue:updatedDownloads:';
  end;
{$endif}

