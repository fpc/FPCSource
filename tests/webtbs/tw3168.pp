{ Source provided for Free Pascal Bug Report 3168 }
{ Submitted by "Henri Gourvest" on  2004-06-15 }
{ e-mail: hgourvest@progdigy.com }
program Untitled1;
{$MODE objfpc}
type
  TClazz = class
  private
    f: procedure(x: Integer); cdecl;
  public
    x: integer;
  end;

begin
end.
