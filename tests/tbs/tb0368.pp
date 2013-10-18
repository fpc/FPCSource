type
  tproc = procedure of object;
  trec = record
    l1 : codeptrint;
    l2 : ptrint;
  end;
var
  pfn : tproc;

begin
  pfn:=nil;
  if (trec(pfn).l1<>0) or
     (trec(pfn).l2<>0) then
   begin
     writeln('Error!');
     halt(1);
   end;
end.
