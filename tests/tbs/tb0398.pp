{ %version=1.1}
{$codepage cp850}
begin
   if ord(widechar(#196))<>9472 then
     halt(1);
   if ord(#0196)<>196 then
     halt(1);
   if ord(widechar(#$a6))<>170 then
     halt(1);
   if ord(#$0a6)<>166 then
     halt(1);
   halt(0);
end.
