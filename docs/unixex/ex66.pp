Program Example66;

{ Program to demonstrate the MMap function. }

Uses BaseUnix,Unix;

Var S    : String;
    fd   : cint;
    Len  : longint;
//    args : tmmapargs;
    P    : PChar;

begin
  s:='This is the string';
  Len:=Length(S);
  fd:=fpOpen('testfile.txt',O_wrOnly or o_creat);
  If fd=-1 then
    Halt(1);
  If fpWrite(fd,S[1],Len)=-1 then
    Halt(2);
  fpClose(fd);
  fd:=fpOpen('testfile.txt',O_rdOnly);
  if fd=-1 then
    Halt(3);
  P:=Pchar(fpmmap(nil,len+1 ,PROT_READ or PROT_WRITE,MAP_PRIVATE,fd,0));

  If longint(P)=-1 then
    Halt(4);
  Writeln('Read in memory  :',P);
  fpclose(fd);
  if fpMUnMap(P,Len)<>0 Then
    Halt(fpgeterrno);
end.
