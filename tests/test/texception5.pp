{ %version=1.1}
{$IFDEF FPC}
{$MODE OBJFPC}
{$ENDIF}
program texception5;
uses
  SysUtils;


type
  ETestException = class(Exception)
    constructor Create;
    destructor Destroy; override;
  end;


var
  exc_destroyed: boolean;
  exc          : ETestException;

constructor ETestException.Create;
begin
  exc_destroyed := false;
  exc := Self;
end;


destructor ETestException.Destroy;
begin
  inherited;
  exc_destroyed := true;
end;

var
  exc2: Exception;

begin
  // first test, exception should not be freed
  try
    raise ETestException.Create;
  except
    exc2 := Exception(AcquireExceptionObject);
    if exc <> exc2 then halt(11);
  end;
  if exc_destroyed then halt(12);
  if exc <> exc2 then halt(13);
  exc2.Free;

  // second test, exception should be freed
  try
    raise ETestException.Create;
  except
    exc2 := Exception(AcquireExceptionObject);
    if exc <> exc2 then halt(21);
    ReleaseExceptionObject;
  end;
  if not exc_destroyed then halt(22);

  // third test, exception should not be freed
  try
    raise ETestException.Create;
  except
    AcquireExceptionObject;
    AcquireExceptionObject;
    ReleaseExceptionObject;
  end;
  if exc_destroyed then halt(31);

  // exception should be freed
  try
    raise ETestException.Create;
  except
    AcquireExceptionObject;
    AcquireExceptionObject;
    ReleaseExceptionObject;
    ReleaseExceptionObject;
  end;
  if not exc_destroyed then halt(41);

  // exception should be freed, refcount zeroed when re-raising
  try
    try
      raise ETestException.Create;
    except
      on e: exception do begin
        AcquireExceptionObject;
        raise;
      end;
    end;
  except
  end;
  if not exc_destroyed then halt(51);

  // same as before but without explicit block
  try
    try
      raise ETestException.Create;
    except
      AcquireExceptionObject;
      raise;
    end;
  except
  end;
  if not exc_destroyed then halt(61);
end.
