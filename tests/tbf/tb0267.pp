{ %fail }

{$mode objfpc}{$h+}
{$interfaces corba}

type
  tintf = interface
    procedure test(l: longint);
    procedure test(s: string);
  end;

  tp = class
    procedure test(l: longint); virtual;
    procedure test(s: string); virtual;
  end;

  tc = class(tp, tintf)
    procedure test(l: longint); override;
  end;

procedure tp.test(l: longint);
  begin
  end;

procedure tp.test(s: string);
  begin
  end;

procedure tc.test(l: longint);
  begin
  end;

begin
end.
