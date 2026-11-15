{ methods of classes declared in another unit converted to function references:
  an RTL method, an inherited method through Self and through an instance,
  a function with a parameter and a class function (issue 42000) }
program tfuncref66;

{$mode delphi}

uses
  ufuncref66;

type
  TProc = reference to procedure;
  THandlerRef = reference to procedure(out aIntf: IInterface; out aText: String);
  TAddRef = reference to function(aValue: LongInt): LongInt;
  TNameRef = reference to function: String;

  TChild = class(TBase)
    procedure Run;
  end;

var
  gFailed: LongInt = 0;

procedure Check(const aRow: String; aOk: Boolean);
begin
  if not aOk then
    begin
      Writeln(aRow, ' failed');
      Inc(gFailed);
    end;
end;

procedure Take(const aRow: String; aRef: THandlerRef);
var
  lIntf: IInterface;
  lText: String;
begin
  aRef(lIntf, lText);
  Check(aRow + ': text', lText = 'called');
  Check(aRow + ': interface', Assigned(lIntf));
end;

procedure TChild.Run;
begin
  Take('inherited method through Self', Handler);
end;

var
  lObj: TObject;
  lFree: TProc;
  lChild: TChild;
  lAdd: TAddRef;
  lName: TNameRef;
begin
  lObj := TObject.Create;
  lFree := lObj.Free;
  lFree();

  lChild := TChild.Create;
  lChild.Run;
  Take('inherited method through an instance', lChild.Handler);
  Check('handler calls', lChild.N = 2);
  lAdd := lChild.Add;
  Check('function with a parameter', lAdd(5) = 7);
  lName := lChild.Name;
  Check('class function', lName() = 'TChild');
  lChild.Free;

  if gFailed <> 0 then
    Halt(1);
  Writeln('ok');
end.
