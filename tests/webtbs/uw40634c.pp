unit uw40634c;
{$mode ObjFPC}{$H+}
interface

type

  generic TLazFifoQueue<T> = class
  private
    FList: array of T;
    FQueueSize: integer;
  //protected
  //  function PushItem(const AItem: TFpThreadWorkerItem): Boolean; virtual;
  end;

  { TLazThreadedQueue }

  generic TLazThreadedQueue<T> = class
  protected
  type
    TLazTypedFifoQueue = specialize TLazFifoQueue<T>;
  end;


implementation

end.

