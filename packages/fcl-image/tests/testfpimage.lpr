{
    Console test runner for fcl-image.
    See the file COPYING.FPC, included in this distribution, for details.
}
program testfpimage;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

uses
{$IFDEF UNIX}
{$IFDEF UNICODERTL}
  cwstring,
{$ENDIF}
{$ENDIF}
{$IFDEF FPC_DOTTEDUNITS}
  FpcUnit.Runners.Console,
{$ELSE}
  consoletestrunner,
{$ENDIF}
  fpimgtests,
  tccolor, tcpalette, tcmemimage, tccompactimg, tchandlers,
  tcbmp, tcpng, tctga, tcpcx, tcpnm, tcxpm, tcqoi, tcico, tcframes, tcapng,
  tcjpeg, tctiff, tcpsd, tcxwd,
  tccanvas, tcpscanvas, tcinterp, tcgauss, tcquantize,
  tccolorspace, tcmisc, tcqrcode, tcbarcodedraw,
{$IF defined(linux) or defined(darwin) or defined(freebsd) or defined(openbsd) or defined(netbsd)
  or defined(solaris) or defined(haiku) or defined(beos) or defined(aix) or defined(dragonfly)
  or defined(android) or defined(iphonesim) or defined(ios) or defined(win32) or defined(win64)}
  tcftfont,
{$ENDIF}
  tcbarcodes, tcgifwrite, tcgifread;

var
  Application: TTestRunner;

begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  Application := TTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'fcl-image test runner';
    Application.Run;
  finally
    Application.Free;
  end;
end.
