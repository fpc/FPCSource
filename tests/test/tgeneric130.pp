{ %NORUN }

program tgeneric130;

{$mode delphi}

type
  TNotifyEvent = procedure(aSender: TObject) of object;
  TNotifyEvent<T> = procedure(aSender: TObject; aArg: T) of object;

  IEvent<T> = interface
    procedure Add(aArg: T);
  end;

  INotifyEvent = IEvent<TNotifyEvent>;

  INotifyEvent<T> = interface(IEvent<TNotifyEvent<T>>)
  ['{3B510AF8-DC7F-4C41-9118-2591E38D30D7}']
  end;

  TTest<T> = class(TInterfacedObject, INotifyEvent<T>)
    procedure Add(aArg: TNotifyEvent<T>);
  end;

procedure TTest<T>.Add(aArg: TNotifyEvent<T>);
begin
end;

var
  intf: INotifyEvent<LongInt>;
begin
  intf := (TTest<LongInt>.Create) as INotifyEvent<LongInt>;
end.
