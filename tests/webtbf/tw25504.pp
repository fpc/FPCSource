{ %fail }
program test;

{$MODE DELPHI}

{ Commenting the next block of code causes compiler to properly show error message in "Data.HelloWorld".
  Otherwise, it throws error:
    Fatal: Compilation aborted
    An unhandled exception occurred at $00437C5E:
    EAccessViolation: Access violation
      $00437C5E
}
type
// {
  TDateTimeStamp = record
    Value: Int64;
  end;

const
  InvalidDateTimeStampValue = Low(Int64);

type
  TDateTimeStampHelper = record helper for TDateTimeStamp
    const Invalid: TDateTimeStamp = (Value: InvalidDateTimeStampValue);
  end;
// }

  TSomeOtherClass = class
  end;

  TSomeClass = class
  private
    Data: array of TSomeOtherClass;
  public
    procedure Test;
  end;

procedure TSomeClass.Test;
begin
  Data.HelloWorld;
end;

begin
end.
