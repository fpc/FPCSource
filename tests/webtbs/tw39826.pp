{ %NORUN }

program tw39826;
{$mode delphi}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
type
  TGR1<T> = record
  public type
    TGRUpdateProc = reference to procedure (var ARec: TGR1<T>);
  public
    Val: T;
  end;
var gr1: TGR1<integer>;
begin
end.

