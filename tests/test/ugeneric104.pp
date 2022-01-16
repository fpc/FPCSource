unit ugeneric104;

{$mode objfpc}{$H+}

interface

type
  generic TGeneric<T> = class
    procedure Test;
  end;

generic procedure TestProc<T>;

implementation

{ TGeneric }

procedure TGeneric.Test;

  procedure SubTest;
  begin

  end;

begin
  SubTest;
end;

generic procedure TestProc<T>;

  procedure SubTest;
  begin

  end;

begin
  SubTest;
end;

end.

