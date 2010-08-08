uses
 SysUtils;

var
 Info: TSearchRec;
begin
 // should not match, is a directory
 if FindFirst('..',faArchive,Info)=0 then
   halt(1);
 FindClose(Info);
end.

