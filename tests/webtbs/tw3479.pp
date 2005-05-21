{ Source provided for Free Pascal Bug Report 3479 }
{ Submitted by "Michalis Kamburelis" on  2004-12-26 }
{ e-mail: michalis@camelot.homedns.org }
{ Two cases where BreakStr = #10 hang WrapText.
  After applying my fix all things work OK.
}

uses SysUtils;
const
  Line1 = 'one blahblah blah bb b';
  Line2 = 'two hhhhh sss sssss wwwww';
begin
 Writeln(WrapText(Line1 +   #10+ Line2,    #10, [' '], 40));
 Writeln(WrapText(Line1 +#13#10+ Line2, #13#10, [' '], 40));
 Writeln(WrapText(Line1 +   #10+ Line2,    #10, [' '], 10));
 Writeln(WrapText(Line1 +#13#10+ Line2, #13#10, [' '], 10));
end.
