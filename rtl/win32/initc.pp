{
  $Id$
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
  $Log$
  Revision 1.15  2004-12-06 12:27:48  michael
  * fix __errno loading from cygwin1.dll (from Peter)

  Revision 1.14  2004/11/04 17:15:01  peter
   * wcygprt is now used for cygwin (libc) linking, initc contains only cerrno

  Revision 1.13  2004/11/04 09:32:31  peter
  ErrOutput added

  Revision 1.12  2004/09/14 20:08:58  hajny
    * use errno from cygwin (like in fixes branch)

  Revision 1.11  2004/09/12 17:41:40  hajny
    * hopefully fixed the problem with missing __error symbol

  Revision 1.10  2003/12/11 09:21:52  marco
   * patch from peter

  Revision 1.9  2003/11/03 09:42:28  marco
   * Peter's Cardinal<->Longint fixes patch

  Revision 1.8  2003/09/08 18:25:45  peter
    * popstack to cdecl

  Revision 1.7  2002/09/07 16:01:28  peter
    * old logs removed and tabs fixed

}
