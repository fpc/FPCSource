{ %NORUN }

program tw30939a;

{$MODESWITCH result}

Type
  generic TGData<T> = record
    b: T
  end;

  generic TGWrapper<T> = record
    a: specialize TGData<T>
  end;

generic Function DoSomething<T>: specialize TGWrapper<T>;
  Begin
    result.a.b := default(T)
  End;

Begin
  specialize DoSomething<LongInt>;
End.
