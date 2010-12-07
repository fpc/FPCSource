{ %norun }
unit terecs_u1;

{$mode delphi}

interface

type
  TFoo = record
  private
    F1: Integer;
    F2: Byte;
  public
    const
      C = 1;
    var
      F3: Integer;
      F4: Byte;
  end;

implementation

end.

