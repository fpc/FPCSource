{ %NORUN }

program tw22790b;

{$MODE DELPHI}

type
  TPredicateMethod<TOperand> = function (const x: TOperand): Boolean of object;

  TWrapper<TOperand> = class
  strict private
    type
      POperand = ^TOperand;
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
  pred: TPredicateMethod<POperand>;
begin
  with TDereferencingAdapter.Create do begin
    pred := F;  { Error: Incompatible types ... }
    Free;
  end;
end;

begin
end.
