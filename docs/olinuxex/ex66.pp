Program Example66;

{ Program to demonstrate the MMap function. }

Uses oldlinux;

Var S : String;
    fd,Len : Longint;
    args : tmmapargs;
    P : PChar;

begin
  S:='This is a string'#0;
  Len:=Length(S);
  fd:=fdOpen('testfile.txt',Open_wrOnly or open_creat);
  If fd=-1 then
    Halt(1);
  If fdWrite(fd,S[1],Len)=-1 then
    Halt(2);
  fdClose(fd);
  fdOpen('testfile.txt',Open_rdOnly);
  if fd=-1 then
    Halt(3);
  args.address:=0;
  args.offset:=0;
  args.size:=Len+1;
  args.fd:=Fd;
  args.flags:=MAP_PRIVATE;
  args.prot:=PROT_READ or PROT_WRITE;
  P:=Pchar(mmap(args));
  If longint(P)=-1 then
    Halt(4);
  Writeln('Read in memory  :',P);
  fdclose(fd);
  if Not MUnMap(P,Len) Then
    Halt(LinuxError);
end.
