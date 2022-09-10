{$mode delphi}
uses
  sysutils;
type
  glenum=word;
  TglGetError = function(): GLenum; cdecl;

function test: glenum; cdecl;
begin
  result:=42;
end;

var glgeterror : TGlGeterror;
begin
  glgeterror:=test;
  if inttostr(glgeterror)<>'42' then
    halt(1);
end.
