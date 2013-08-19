var
 r: rawbytestring;
begin
 r:='abc';
 setcodepage(r,850);
 setlength(r,length(r)+1);
 if stringcodepage(r)<>850 then
   halt(1);
 setcodepage(r,437);
 setlength(r,length(r)+1000);
 if stringcodepage(r)<>437 then
   halt(2);
end.

