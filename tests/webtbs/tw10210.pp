{ %norun }

unit tw10210;
{$mode objfpc}
interface

type
  generic TSomeList<TElem> = class
  end;

  TSomeClassList = specialize TSomeList<integer>;

implementation

begin
end.
