var  outfile:text;
     err:boolean;
begin
writeln('there should be three errors below:');

assign(outfile,'notexist.fil');
{$i-}
Append(outfile);
//rewrite(outfile);
{$i+}
//write(ioresult);  // 2 file not found
if IOResult <> 0 then writeln('err append')
else
 err:=true;

{$i-}
writeln(outfile,'----------------------');
{$i+}
//write(ioresult);   // 103 file not open
if IOResult <> 0 then writeln('err write')
else
 err:=true;

{$i-}
close(outfile);
{$i+}
//write(ioresult);  // 103 file not open
if IOResult <> 0 then writeln('err close')
else
 err:=true;

   if err then
     halt(1)
   else
     writeln('success');
end.
