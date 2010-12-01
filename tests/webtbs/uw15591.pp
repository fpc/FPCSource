unit uw15591;
{$mode objfpc}
interface

type
  generic GSmartArray<TSomeType> = class
  private
    fItems :array of TSomeType;
  public
    function Length() :Integer;
  end;

  TBooleanSmartArray = specialize GSmartArray<Boolean>;

implementation

function GSmartArray.Length() :Integer;
begin
  Result := System.Length(fItems);
end;

end.
