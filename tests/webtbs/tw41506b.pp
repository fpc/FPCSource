{ %NORUN }

program tw41506b;
{$mode ObjFPC}{$H+}

uses uw41506;

type
  generic TGenClassCommon<T> = class F:T; end;

  TCommon1 = specialize TGenClassCommon{declaration:TGenClassCommon}
  <byte>;
  TCommon2 = tw41506b.specialize TGenClassCommon{declaration:TGenClassCommon}
  <byte>;
  TCommon3 = uw41506.specialize TGenClassCommon{declaration:u_specialize_inline.TGenClassCommon}
  <byte>;

begin

end.
