{
}
unit initc;

interface

type
 libcint   = longint;
 plibcint = ^libcint;

function fpgetCerrno:libcint;
procedure fpsetCerrno(err:libcint);

property cerrno:libcint read fpgetCerrno write fpsetcerrno;


implementation

function geterrnolocation: Plibcint; cdecl;external 'cygwin1.dll' name '__errno';

function fpgetCerrno:libcint;
begin
  fpgetCerrno:=geterrnolocation^;
end;

procedure fpsetCerrno(err:libcint);
begin
  geterrnolocation^:=err;
end;

end.
