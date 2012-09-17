{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2001 by the Free Pascal development team

    This file contains the OS independent routines of the system unit
    for unix styled systems

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$I system.inc}
{$i winiconv.inc}

function get_locale_charset: AnsiString;
var
  p: SizeInt;
begin
  // Get one of non-empty environment variables in the next order:
  // LC_ALL, LC_CTYPE, LANG. Default is 'ASCII'.
  Result:=FpGetEnv('LC_ALL');
  if Result='' then
    Result:=FpGetEnv('LC_CTYPE');
  if Result='' then
    Result:=FpGetEnv('LANG');
  if Result='' then
    Result:='ASCII'
  else begin
    // clean up, for example en_US.UTF-8 => UTF-8
    p:=Pos('.',Result);
    if p>0 then Delete(Result,1,p);
    p:=Pos('@',Result);
    if p>0 then Delete(Result,p,length(Result));
  end;
end;

procedure InitUnixStrings;
begin
  DefaultSystemCodepage:=iconv2win(get_locale_charset);
  initunicodestringmanager;
end;

