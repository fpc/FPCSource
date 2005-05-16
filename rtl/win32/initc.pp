{
  $Id: initc.pp,v 1.16 2005/02/14 17:13:32 peter Exp $
}
unit initc;

interface

type
 libcint   = longint;
 plibcint = ^libcint;

function fpgetCerrno:libcint;
procedure fpsetCerrno(err:libcint);

{$ifndef ver1_0}
property cerrno:libcint read fpgetCerrno write fpsetcerrno;
{$endif}


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
{
  $Log: initc.pp,v $
  Revision 1.16  2005/02/14 17:13:32  peter
    * truncate log

}
