type
  trtabrec=record
    name : string[12];
    idx  : longint;
  end;

const
  trtab : array[0..10] of trtabrec=(
    (name:'general';idx:1000),
    (name:'scan';idx:2000),
    (name:'parser';idx:3000),
    (name:'type';idx:4000),
    (name:'sym';idx:5000),
    (name:'cg';idx:6000),
    (name:'asmr';idx:7000),
    (name:'asmw';idx:8000),
    (name:'exec';idx:9000),
    (name:'unit';idx:10000),
    (name:'option';idx:11000)
  );

var
  t,f : text;
  s,hs : string;
  i,j,k : longint;
begin
  assign(t,paramstr(1));
  reset(t);
  assign(f,'New.msg');
  rewrite(f);
  while not eof(t) do
   begin
     readln(t,s);
     if (s<>'') and not(s[1] in ['#','%']) then
      begin
        for i:=0 to 10 do
         if Copy(s,1,length(trtab[i].name))=trtab[i].name then
          begin
            j:=pos('=',s);
            if j>0 then
             begin
               inc(j);
               if s[j] in ['0'..'9'] then
                begin
                  k:=j;
                  while (s[k] in ['0'..'9']) do
                   inc(k);
                  if s[k]='_' then
                   inc(k);
                  delete(s,j,k-j);
                end;
               str(trtab[i].idx,hs);
               while length(hs)<5 do
                hs:='0'+hs;
               hs:=hs+'_';
               inc(trtab[i].idx);
               insert(hs,s,j);
             end;
            break;
          end;
      end;
     writeln(f,s);
   end;
  close(f);
  close(t);
end.
