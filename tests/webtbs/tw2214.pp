{ Source provided for Free Pascal Bug Report 2214 }
{ Submitted by "Marco" on  2002-11-01 }
{ e-mail: marco@freepascal.org }

{$mode objfpc}

Uses SysUtils;

function LongLatToDMS(longlat : longint; hemis : string):string;
Var ldeg, lmin, lsec, lmsec : extended;
    hemi                    : char;
begin
  ldeg:=1;
  lmin:=2;
  lsec:=3;
  lmsec:=4;
  hemi:='n';
  result := Format('%d %02d %02d.%03d',
               [round(ldeg), round(lmin), round(lsec),
                round(lmsec)]) + ' ' + hemi;
end;

begin
   if LongLatToDMS(1,'a')<>'1  2  3.  4 n' then
     begin
       writeln('Problem with format');
       halt(1);
     end;
end.
