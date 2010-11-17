program tw17945;
{$mode delphi}
type
  TFoo = class
  public
    type
      TEnumerator = object
      private
        FFoo: TFoo; //Was error: Illegal expression
      end;
  end;

begin
end.