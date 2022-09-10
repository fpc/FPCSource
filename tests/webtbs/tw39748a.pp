{$mode delphi}
type
  glenum=word;
  TglGetError = function(): GLenum; cdecl;

function test: glenum; cdecl;
begin
  result:=42;
end;

procedure call(e: longint); overload;
begin
  writeln('longint');
  halt(1);
end;

procedure call(p :tglgeterror); overload;
begin
  writeln('procvar');
end;

var glgeterror : TGlGeterror;
begin
  glgeterror:=test;
  call(glgeterror)
end.

