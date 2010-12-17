{ %fail}
{ %norun}
program terecs2;

{$mode delphi}
type
  TFoo = record
  private
    F1: Integer;
    F2: Byte;
  public
    F3: Integer;
    F4: Byte;
  published // record can't have published fields
    F5: String;
  end;

begin
end.