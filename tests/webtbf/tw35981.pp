{ %FAIL }

program tw35981;

{$mode Delphi}

uses Classes;

type
  TFoo<T: TPersistent> = class(TPersistent)
  public
    C: T;
    constructor Create;
    destructor Destroy; override;
  end;

  constructor TFoo<T>.Create;
  begin
    inherited Create;
    C := T.Create;
  end;

  destructor TFoo<T>.Destroy;
  begin
    C.Free;
    inherited Destroy;
  end;

  // note the *working* specialize here, in {$mode Delphi} !!!
  function Test<T: TPersistent>: specialize TFoo<T>;
  begin
    Result := TFoo<T>.Create;
  end;

begin
  with Test<TStrings> do begin
    WriteLn(C.ClassName);
    Free;
  end;
end.
