{ %NORUN }

program tw22468;

{$MODE DELPHI}

type
  TArray<T> = array of T;

  TWrapper<TValue> = record
  private
    type TValueArray = TArray<TValue>;
  public
    class procedure Z; static;
  end;


{$PUSH}{$MACRO ON}
{$DEFINE TWrapper__Z :=
  var
    a: TValueArray;
  begin
  end
}

class procedure TWrapper<TValue>.Z;
TWrapper__Z;
{$POP}

begin
end.
