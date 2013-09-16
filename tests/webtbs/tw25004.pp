program tw25004;

{$MODE DELPHI}

uses
  SysUtils;

type
  TExpression = class(TObject)
  end;

  TLiteralInteger = class(TExpression)
  public
    Value: integer;

    constructor Create(Value: integer); virtual;
    function ToString(): string; override;
  end;

  TVariableReference = class(TExpression)
  public
    Identifier: string;

    constructor Create(const Identifier: string); virtual;
    function ToString(): string; override;
  end;

  TBinaryOperator = (boAdd, boGreaterThan);

  TBinaryOperatorExpression = class(TExpression)
  protected
    function OperatorToString(Operator_: TBinaryOperator): string;
  public
    Operator_: TBinaryOperator;
    Left: TExpression;
    Right: TExpression;

    constructor Create(Operator_: TBinaryOperator; Left: TExpression; Right: TExpression); virtual;
    destructor Destroy(); override;
    function ToString(): string; override;
  end;

  TExpressionBuilder = record
  public
    Expression: TExpression;

    class operator  Implicit(Operand: TExpression): TExpressionBuilder;
    class operator  Implicit(Value: integer): TExpressionBuilder;
    class operator  Add(const Left: TExpressionBuilder; const Right: TExpressionBuilder): TExpressionBuilder;
    class operator  GreaterThan(const Left: TExpressionBuilder; const Right: TExpressionBuilder): TExpressionBuilder;
  end;

  { TLiteralInteger }

  constructor TLiteralInteger.Create(Value: integer);
  begin
    inherited Create();

    Self.Value := Value;
  end;

  function TLiteralInteger.ToString(): string;
  begin
    Result := IntToStr(Value);
  end;

  { TVariableReference }

  constructor TVariableReference.Create(const Identifier: string);
  begin
    inherited Create();

    Self.Identifier := Identifier;
  end;

  function TVariableReference.ToString(): string;
  begin
    Result := Identifier;
  end;

  { TBinaryOperatorExpression }

  constructor TBinaryOperatorExpression.Create(Operator_: TBinaryOperator; Left, Right: TExpression);
  begin
    inherited Create();

    Self.Operator_ := Operator_;
    Self.Left := Left;
    Self.Right := Right;
  end;

  destructor TBinaryOperatorExpression.Destroy();
  begin
    Left.Free();
    Right.Free();

    inherited Destroy();
  end;

  function TBinaryOperatorExpression.OperatorToString(Operator_: TBinaryOperator): string;
  begin
    case Operator_ of
      boAdd:
        Result := '+';
      boGreaterThan:
        Result := '>';
      else
        raise Exception.Create('Unknown operator');
    end;
  end;

  function TBinaryOperatorExpression.ToString(): string;
  begin
    Result := Left.ToString() + ' ' + OperatorToString(Operator_) + ' ' + Right.ToString();
  end;

  { TExpressionBuilder }

  class operator TExpressionBuilder.Add(const Left: TExpressionBuilder; const Right: TExpressionBuilder): TExpressionBuilder;
  begin
    Result := TBinaryOperatorExpression.Create(boAdd, Left.Expression, Right.Expression);
  end;

  class operator TExpressionBuilder.Implicit(Operand: TExpression): TExpressionBuilder;
  begin
    Result.Expression := Operand;
  end;

  class operator TExpressionBuilder.GreaterThan(const Left: TExpressionBuilder; const Right: TExpressionBuilder): TExpressionBuilder;
  begin
    Result := TBinaryOperatorExpression.Create(boGreaterThan, Left.Expression,
      Right.Expression);
  end;

  class operator TExpressionBuilder.Implicit(Value: integer): TExpressionBuilder;
  begin
    Result := TLiteralInteger.Create(Value);
  end;

var
  Variable1: TExpressionBuilder;
  Variable2: TExpressionBuilder;
  Formula: TExpressionBuilder;
begin
  Variable1 := TVariableReference.Create('a');
  Variable2 := TVariableReference.Create('b');
  Formula := Variable1 + 1 > 5 + Variable2;
  if Formula.Expression.ToString() <> 'a + 1 > 5 + b' then
    halt(1);
  Formula.Expression.Free();
end.
