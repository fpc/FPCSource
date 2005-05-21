{ Source provided for Free Pascal Bug Report 1401 }
{ Submitted by "Bill" on  2001-02-10 }
{ e-mail: lingolanguage@hotmail.com }

(*
Compiles using Delphi 4 (v12):
  dcc32 fpc5
Does not compile in FPC (1.0.4) using:
  ppc386 -Sd fpc5
*)

program fpc10;

{
  uses sysutils,windows;
}

const BUFMAX = 10;
      CHSIZE = 1;
var fFile:file;
    buffer:string[10];
    uiNumread:cardinal;

procedure test;
begin
  (* don't run this! *)
  blockread (fFile, buffer, BUFMAX*CHSIZE, uiNumread);
  blockread (fFile, buffer, cardinal(BUFMAX*CHSIZE), uiNumread);
  blockwrite (fFile, buffer, BUFMAX*CHSIZE, uiNumread);
  blockwrite (fFile, buffer, cardinal(BUFMAX*CHSIZE), uiNumread);
end;

begin
  Writeln('Test for cardinal version of blockread/blockwrite');
end.
