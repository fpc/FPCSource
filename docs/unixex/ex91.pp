program Example30;

{ Program to demonstrate the FSStat function. }

uses BaseUnix,Unix,UnixType;

var s : string;
    fd : cint;
    info : tstatfs;

begin
  writeln ('Info about current partition : ');
  s:='.';
  while s<>'q' do
    begin
    Fd:=fpOpen(S,O_RDOnly);
    if (fd>=0) then
      begin
      if fstatfs (fd,info)<>0 then
        begin
        writeln('Fstat failed. Errno : ',fpgeterrno);
        halt (1);
        end;
      FpClose(fd);
      writeln;
      writeln ('Result of fsstat on file ''',s,'''.');
      writeln ('fstype  : ',info.fstype);
      writeln ('bsize   : ',info.bsize);
      writeln ('bfree   : ',info.bfree);
      writeln ('bavail  : ',info.bavail);
      writeln ('files   : ',info.files);
      writeln ('ffree   : ',info.ffree);
      {$ifdef FreeBSD}
      writeln ('fsid    : ',info.fsid[0]);
      {$else}
      writeln ('fsid    : ',info.fsid[0]);
      writeln ('Namelen : ',info.namelen);
      {$endif}
      write ('Type name of file to do fsstat. (q quits) :');
      readln (s)

      end;
    end;
end.