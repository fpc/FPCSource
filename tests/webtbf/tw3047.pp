{ %fail }
{ Source provided for Free Pascal Bug Report 3047 }
{ Submitted by "GBD" on  2004-04-12 }
{ e-mail:  }
{$mode delphi}

function JenkinsHashBuffer(const buffer; length : Integer; initVal : Integer) : longword;
begin
end;

function JenkinsHashString(const s : String) : longword;
begin
                result := JenkinsHashBuffer(PChar(s), Length(s), 0)
end;

begin
end.
