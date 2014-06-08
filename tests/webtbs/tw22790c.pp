{ %NORUN }

program tw22790c;

{$MODE DELPHI}

type
  TPredicateMethod<TOperand> = function (const x: TOperand): Boolean of object;

  TWrapper<TOperand> = class
  strict private
    type
      POperand = ^TOperand;
      TPredicateMethodForPOperand = TPredicateMethod<POperand>;
      TDereferencingAdapter = class
        function F(const x: POperand): Boolean;
      end;
  public
    procedure Z;
  end;

function TWrapper<TOperand>.TDereferencingAdapter.F(const x: POperand): Boolean;
begin
end;

procedure TWrapper<TOperand>.Z;
var
  pred: TPredicateMethodForPOperand;
    { Error: Generics without specialization cannot be used as ... }
begin
  with TDereferencingAdapter.Create do begin
    pred := F;
    Free;
  end;
end;

begin
end.
