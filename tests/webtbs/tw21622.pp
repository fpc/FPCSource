{ %NORUN }

program tw21622;

{$MODE DELPHI}
{$DEFINE CAUSE_ERROR}

type
  TProceduralMethod<T> = procedure (arg: T) of object;

  TWrapper<T> = class
  strict private
    type
      TOnChanging = TProceduralMethod<T>;
      { Replace T with e.g. Integer, the problem persists }
  strict private
  {$IFDEF CAUSE_ERROR}
    FOnChanging: TOnChanging;
      { Error: Generics without specialization cannot be used as a type for
        a variable }
  {$ELSE}
    FOnChanging: TProceduralMethod<T>;
  {$ENDIF}
  end;

begin
end.
