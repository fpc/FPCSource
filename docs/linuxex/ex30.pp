program Example30;

{ Program to demonstrate the FSStat function. }

uses BaseUnix,Unix;
    
var s : string; 
    info : tstatfs;
    
begin
  writeln ('Info about current partition : ');
  s:='.';
  while s<>'q' do 
    begin
    if statfs (s,info)<>0 then
       begin
         writeln('Fstat failed. Errno : ',fpgeterrno);
         halt (1);
       end;
    writeln;
    writeln ('Result of fsstat on file ''',s,'''.');
    writeln ('fstype  : ',info.ftype);
    writeln ('bsize   : ',info.bsize);
    writeln ('bfree   : ',info.bfree);
    writeln ('bavail  : ',info.bavail);
    writeln ('files   : ',info.files);
    writeln ('ffree   : ',info.ffree);
    {$ifdef FreeBSD}
    writeln ('fsid    : ',info.fsid[0]);
    {$else}
    writeln ('fsid    : ',info.fsid);
    writeln ('Namelen : ',info.namelen);
    {$endif}
    write ('Type name of file to do fsstat. (q quits) :');
    readln (s)
    end;
end.