{ %NORUN }

program tw30939a;

{$MODE delphi}

Type
  TGData<T> = record
    b: T
  end;

  TGWrapper<T> = record
    a: TGData<T>
  end;

Function DoSomething<T>: TGWrapper<T>;
  Begin
    result.a.b := default(T)
  End;

Begin
  DoSomething<LongInt>;
End.
