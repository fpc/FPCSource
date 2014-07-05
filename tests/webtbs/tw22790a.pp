{ %NORUN }

program tw22790a;

{$MODE DELPHI}

type
  TBinaryRelationMethod<TOperand> =
    function (const a, b: TOperand): Boolean of object;

  TWrapper1<TOperand> = record
    class procedure Sort(lessThan: TBinaryRelationMethod<TOperand>); static;
  end;

  TWrapper2<TOperand> = class
  strict private
    type
      POperand = ^TOperand;

      TDereferencingAdapter = class
        function LessThan(const a, b: POperand): Boolean;
      end;
  public
    procedure Sort;
  end;

class procedure TWrapper1<TOperand>.Sort(
  lessThan: TBinaryRelationMethod<TOperand>);
begin
end;

function TWrapper2<TOperand>.TDereferencingAdapter.LessThan(
  const a, b: POperand): Boolean;
begin
end;

procedure TWrapper2<TOperand>.Sort;
begin
  with TDereferencingAdapter.Create do begin
    TWrapper1<POperand>.Sort(LessThan);  { Error: Incompatible types ... }
    Free;
  end;
end;

begin
end.
