program Example30;

{ Program to demonstrate the FSStat function. }

uses BaseUnix,Unix,UnixType;
    
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
    writeln ('fstype  : ',info.fstype);
    writeln ('bsize   : ',info.bsize);
    writeln ('bfree   : ',info.bfree);
    writeln ('bavail  : ',info.bavail);
    writeln ('files   : ',info.files);
    writeln ('ffree   : ',info.ffree);
    writeln ('fsid    : ',info.fsid[0]);
    writeln ('Namelen : ',info.namelen);
    write ('Type name of file to do fsstat. (q quits) :');
    readln (s)
    end;
end.