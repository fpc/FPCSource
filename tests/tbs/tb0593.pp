{$mode objfpc}
type
  TBlockType = (
    btNone,
    btBegin,
    btAsm,
    btEdgedBracket,
    btRoundBracket,
    btTry,
    btFinally,
    btExcept,
    btCase,
    btCaseOf,
    btCaseColon,
    btCaseElse,
    btRepeat,
    btIf,
    btIfElse,
    btClass,
    btInterface,
    btObject,
    btRecord
    );
  TBlock = record
    Typ: TBlockType;
    StartPos: integer;
    InnerIndent: integer;
    InnerStartPos: integer;
  end;
  PBlock = ^TBlock;
  TBlockStack = record
    Stack: PBlock;
    Capacity: integer;
    Top: integer;
  end;


function TopBlockType(const Stack: TBlockStack): TBlockType;
  begin
    if Stack.Top>=0 then
      Result:=Stack.Stack[Stack.Top].Typ
    else
      Result:=btNone;
  end;  

begin
end.
