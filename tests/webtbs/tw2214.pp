{ Source provided for Free Pascal Bug Report 2214 }
{ Submitted by "Marco" on  2002-11-01 }
{ e-mail: marco@freepascal.org }

{$mode objfpc}

Uses SysUtils;

function LongLatToDMS(longlat : longint; hemis : string):string;
Var ldeg, lmin, lsec, lmsec : extended;
    hemi                    : char;
begin
  result := Format('%d %02d %02d.%03d',
               [round(ldeg), round(lmin), round(lsec),
                round(lmsec)]) + ' ' + hemi;
end;

begin
end.
