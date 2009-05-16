program doiconv;
{
    Copyright (c) 2008 by Marco van de Voort(marco@freepascal.org)
    member of the Free Pascal development team

    Test program for the iconvenc package.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Seems not to have memory leaks atm. If you experience them, check procedure
newcalc first.
}

{$mode objfpc}{$H+}

uses
  SysUtils,
  iconvenc;

// some random Hebrew string for testing in CP1255
Const InputString : array[0..21] of char = 
          (#$e0,#$e1,#$e2,#$e3,#$e4,#$e5,#$e6,#$e7,
	   #$e8,#$e9,#$eb,#$ec,#$ee,#$f0,#$f1,#$f2,
  	   #$f4,#$f6,#$f7,#$f8,#$f9,#$fa);
      InputEncoding = 'CP1255';
   
procedure DoOneConversion(TargetEncoding:string);
var
  fn,res: string;
  f1:text;
  convres: integer;
begin

  // note that while the iconvert function is easy, it opens and closes
  // a iconv handle each time, and also requires exceptions.
  // I do not know how costly this is.
  // also iconvert skips unknown chars (EILSEQ).
  convres:=Iconvert(inputstring,res,inputencoding,targetencoding);
  if convres=0  then
   begin
    fn:='result-'+targetencoding+'.txt';
    Writeln('Succes: writing file ',fn,' with results');
    assignfile(f1,fn);
    rewrite(f1);
    Write(f1,res);
    closefile(f1); 
   end
  else
   Writeln('Failure for ',TargetEncoding,' error: ',convres);
end;

var s : string;
begin
  {$IFDEF LOADDYNAMIC}
  if not InitIconv(s) then
    begin
      Writeln('Iconv initialization failed:',s);
      halt;
    end ;
 {$ENDIF}
 DoOneConversion('UTF-8');
 DoOneConversion('UTF-16');
end.

