{ Source provided for Free Pascal Bug Report 2926 }
{ Submitted by "Johannes Berg" on  2004-01-31 }
{ e-mail: bugs@johannes.sipsolutions.de }
program test;

uses
(*$IFDEF LINUX *)
   Unix,
(*$ENDIF *)

(*$IFDEF MSWINDOWS *)
   Windows,
(*$ENDIF *)
  SysUtils, Classes;

begin
end.
