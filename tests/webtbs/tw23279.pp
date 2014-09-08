{ %NORUN }

program tw23279;

{$MODE DELPHI}

type
  TPolicy<T> = class abstract
    procedure Z(const a: T); virtual; abstract;
  end;

  TWrapper<T, S> = class
  private
    FPolicy: TPolicy<T>;
  public
    constructor Create(policy: TPolicy<T>);
    procedure W(const a: T);
  end;

  TSpecialWrapper<S> = class(TWrapper<Integer, S>)
  strict private
    type
      TSpecialPolicy = class(TPolicy<Integer>)
        procedure Z(const a: Integer); override;
      end;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TWrapper<T, S> }

constructor TWrapper<T, S>.Create;
begin
end;

procedure TWrapper<T, S>.W(const a: T);
begin
  FPolicy.Z(a);
end;

{ TSpecialWrapper<S>.TSpecialPolicy }

procedure TSpecialWrapper<S>.TSpecialPolicy.Z(const a: Integer);
begin
  Writeln(a);
end;

{ TSpecialWrapper<S> }

constructor TSpecialWrapper<S>.Create;
begin
  inherited Create(TSpecialPolicy.Create);
    { Warning: Constructing a class "TSpecialPolicy" with abstract method "Z";
      if Z() is CLASS or CLASS STATIC the warning is suppressed but the code
      is still faulty }
end;

destructor TSpecialWrapper<S>.Destroy;
begin
  FPolicy.Free;
end;

begin
  with TSpecialWrapper<Byte>.Create do begin
    W(0);
    Free;
  end;
end.

