program SetsFUBAR;

uses
  SysUtils;

var
  FCommentChars: TSysCharSet;
  s: string;

begin
  s := '#';
  FCommentChars := [';','#'];
  if not (s[1] in FCommentChars) then
    halt(1);
end. 

