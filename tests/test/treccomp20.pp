program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

uses
  SysUtils;

type
  TChildClass = class
  public
    B: Integer;
  end;

  TComposed = record
    A: Integer;
    contains cls: TChildClass;
    C: Integer;
  end;

var
  c: TComposed;
begin
  c.cls:=nil;
  try
    c.B:=42;
  except on e: EAccessViolation do
    begin
      WriteLn('Exception: ', e.message);
      WriteLn('ok');
      halt(0);
    end;
  end;
  halt(1);
end.
