{ %fail }

{ Source provided for Free Pascal Bug Report 2972 }
{ Submitted by "Michalis Kamburelis" on  2004-02-13 }
{ e-mail: michalis@camelot.homedns.org }
type
  TEnum = (one, two);

var s:String;
begin
 s:=one;
end.
