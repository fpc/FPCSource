{ %NORUN }

program tw31033;

{$MODESWITCH RESULT}
{$MODESWITCH ADVANCEDRECORDS}

Type
  generic TGData<T> = record
    public type
      // FIXME: Compiler bug, details see:
      // http://lists.freepascal.org/pipermail/fpc-pascal/2016-November/049444.html [^]

      TSelf = specialize TGData<T>;

      PSelf = ^TSelf;

    public
      d: T;
      n: PSelf
  end;

generic Function Init<T>: specialize TGData<T>.PSelf; forward;

generic Function Init<T>: specialize TGData<T>.PSelf;
  Begin
    new(result);

    result^.d := default(T);
    result^.n := nil
  End;

var
  t: ^specialize TGData<LongInt>;
Begin
  t := specialize Init<LongInt>;
  dispose(t);
End.
