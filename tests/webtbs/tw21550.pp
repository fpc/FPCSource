{%OPT=-gl}

program tw21550;

{$mode objfpc}
{$H+}

type
  generic TEnumMetaClassTemplate<_TEnum> = class
  private
    FMetaInfo:pointer;static;
  public
  end;

  TRowRenderMethod = (
  rrmLines
  );
  TRowRenderMethodMeta = specialize TEnumMetaClassTemplate<TRowRenderMethod>;

begin
end.

