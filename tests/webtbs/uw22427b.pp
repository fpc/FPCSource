unit uw22427b;

{$MODE DELPHI}

interface

type
  TWrapper<T> = record
    class procedure Test; static;
  end;

implementation

uses uw22427a;

{$PUSH}{$MACRO ON}
{$DEFINE TWrapper__Test :=
  begin
    Writeln(SMessage);
  end
}

class procedure TWrapper<T>.Test;
TWrapper__Test;

{$POP}

end.
