program asltest;

uses exec,easyasl, linklist, strings, amigautils;

{
   How to get more files than one with easyasl.
   Just remeber that you have to use linklist and
   not an ordinary list.

   24 Jan 2000.

   nils.sjoholm@mailbox.swipnet.se
}

VAR

  pdummy   : array [0..108] of char;

  path     : PChar;
  dummy    : boolean;
  mylist   : pList;
  mynode   : pFPCNode;
  temp     : Longint;

begin

  path := @pdummy;
  CreateList(mylist);
  StrpCopy(path,'sys:');
  dummy := GetMultiAsl('test of getmulti',path,mylist,nil,nil);
  If dummy then begin
      writeln;
      writeln('Number of files picked ',NodesInList(mylist));
      writeln('And the winner are:');
      PrintList(mylist);
      writeln(chr(10) + 'Press Return' + chr(10));
      readln;

      writeln('And now path plus file');
      mynode := GetFirstNode(mylist);
      FOR temp := 1 TO NodesInList(mylist) DO BEGIN
         writeln(PathAndFile(path,GetNodeData(mynode)));
         mynode := GetNextNode(mynode);
      END;
  end else writeln('You didn''t pick any files');
  DestroyList(mylist);
END.
