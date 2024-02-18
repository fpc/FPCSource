{ %NORUN }

program tw40634b;
{$mode ObjFPC}{$H+}
uses uw40634c;
type
  TFpThreadWorkerItem = class
  end;

  TFpThreadWorkerQueue = class(specialize TLazThreadedQueue<TFpThreadWorkerItem>)
  protected type
    TFpDbgTypedFifoQueue = class(TLazTypedFifoQueue)
      //function PushItem(const AItem: TFpThreadWorkerItem): Boolean; override;
    end;
  end;


begin
end.

