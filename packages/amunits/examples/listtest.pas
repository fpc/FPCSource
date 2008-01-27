PROGRAM test;

{
    A small test of linklist unit.

    nils.sjoholm@mailbox.swipnet.se
}

uses
{$ifdef Amiga}
    exec,
{$endif}
    linklist, strings;

    VAR

    Mylist   : pList;
    MyNode   : pFPCNode;
    i        : Longint;
    temp     : Longint;
    buffer   : PChar;
    bufsize  : Longint;
    templist : pList;


BEGIN
    CreateList(Mylist);

    AddNewNode(Mylist,'Monday');
    AddNewNode(Mylist,'Tuesday');
    AddNewNode(Mylist,'Wednesday');
    AddNewNode(Mylist,'Thursday');
    AddNewNode(Mylist,'Friday');
    AddNewNode(Mylist,'Saterday');
    AddNewNode(Mylist,'Sunday');

    writeln;
    WriteLN('This is the list');
    PrintList(Mylist);

    writeln;
    WriteLN('Now we are going to remove the last node');
    WriteLN('>> Press return');
    readln;
    RemoveLastNode(Mylist);
    PrintList(Mylist);
    writeln;

    WriteLN('>> Press return to get the size of the list');
    writeln;
    readln;
    WriteLN('The size of allocated list is ', SizeOfList(Mylist));
    writeln;

    writeln('Now we are going to print all strings' +#10+ 'in the list with the internal commands');
    WriteLN('>> Press return');
    readln;

    i := NodesInList(Mylist);
    MyNode := GetFirstNode(Mylist);
    FOR temp := 1 TO i DO BEGIN
        WriteLN(MyNode^.ln_Name);
        MyNode := GetNextNode(MyNode);
    END;

    writeln;
    WriteLN('We will move the last node to the top');
    WriteLN('>> Press return');
    readln;
    MyNode := GetLastNode(Mylist);
    MoveNodeTop(Mylist,MyNode);
    PrintList(Mylist);
    writeln;

    WriteLN('We shall change the value in one node');
    WriteLN('>> Press return');
    readln;
    MyNode := GetFirstNode(Mylist);
    MyNode := GetNextNode(MyNode);
    UpDateNode(MyNode,'This is the new day');
    PrintList(Mylist);
    writeln;

    MyNode := GetNextNode(MyNode);
    WriteLN('Now we delete one node');
    WriteLN('>> Press return');
    readln;
    WriteLN('This node is going to be deleted ',GetNodeData(MyNode));
    DeleteNode(MyNode);
    PrintList(Mylist);

    writeln;
    WriteLN('Sort the list');
    WriteLN('>> Press return');
    readln;
    SortList(Mylist);
    PrintList(Mylist);

    writeln;
    writeln('Search for a node, in this case Friday');
    WriteLN('>> Press return');
    readln;
    MyNode := FindNodeData(Mylist,'Friday');
    IF MyNode <> NIL THEN BEGIN
        WriteLN('found the node ',MyNode^.ln_Name);
        { or writeln('found the node ',GetNodeData(MyNode));  }
    END ELSE BEGIN
        WriteLN('Node not found');
    END;

    writeln;
    WriteLN('And now copy the list to a stringbuffer' +#10+ 'and print it');
    WriteLN('>> Press return');
    readln;
    bufsize := SizeOfList(Mylist);
    buffer := StrAlloc(bufsize);
    ListToBuffer(Mylist,buffer);
    WriteLN(buffer);

    writeln;
    WriteLN('Now we try to copy the list to a new list');
    WriteLN('>> Press return');
    readln;
    templist := CopyList(Mylist);
    IF templist <> NIL THEN BEGIN
        WriteLN('That went well, the new list is here');
        PrintList(templist);
        DestroyList(templist);
    END ELSE BEGIN
        WriteLN('no copy of list');
    END;

    writeln;
    WriteLN('Press return to destroy the list');
    readln;
    DestroyList(Mylist);
    writeln;
    WriteLN('All done');
END.
