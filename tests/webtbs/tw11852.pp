unit tw11852;

interface

function _hc_test( inp: integer ): integer; CDecl;

implementation

uses
  Classes, Types, SysUtils;

function _hc_test( inp: integer ): integer; CDecl;
begin
  _hc_test := inp;
end;

procedure __func; local;
begin
  WriteLn( 'local function' );
end;

initialization
  __func;

end.

//= END OF FILE ===============================================================

