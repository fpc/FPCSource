{ Old file: tbs0145.pp }
{ typed files with huges records (needs filerec.size:longint) OK 0.99.7 (PFV) }

{$I+}
const
  Mb=512;
  siz=1024*Mb;

type
  buf=array[1..siz] of byte;

var
  fin,
  fout : file of buf;
  b1,a1 : buf;

begin
  fillchar(a1,sizeof(a1),1);
  assign(fout,'tmp.tmp');
  rewrite(fout);
  write(fout,a1);
  close(fout);

  assign(fin,'tmp.tmp');
  reset(fin);
  read(fin,b1);
  close(fin);
  erase(fin);
  if not b1[512*Mb]=1 then
   begin
      writeln('data err');
      Halt(1);
   end;
end.
