program tb0577;

{$mode delphi}

type
  tc = class
    procedure test(b: byte);virtual;overload;
  end;

  tc2 = class(tc)
   strict protected
    procedure test(b: byte; l: longint = 1234);virtual;overload;
   public
    procedure test(l: longint);virtual;overload;
  end;

  tc3 = class(tc2)
    procedure test(b: byte);override;overload;
  end;

var
  glob: longint;

  procedure tc.test(b: byte);
    begin
      glob:=2;
    end;

  procedure tc2.test(l: longint);
    begin
      glob:=1;
    end;

  procedure tc2.test(b: byte; l: longint = 1234);
    begin
      glob:=3;
    end;

  procedure tc3.test(b: byte);
    begin
      inherited;
    end;

var
  c: tc;
begin
  c:=tc3.create;
  c.test(byte(4));
  if glob<>2 then
    halt(1);
end.
