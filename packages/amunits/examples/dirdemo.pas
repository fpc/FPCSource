PROGRAM DirDemo;

{
    How you can use unit linklist.
    21 Mar 2001.

    Changed to use printf in amigalib.
    25 Nov 2002.

    nils.sjoholm@mailbox.swipnet
}

uses Amigados, exec, strings, linklist,pastoc, amigalib;

CONST BufferSize = 2048;
      CSI      = chr($9b);

VAR ExData       : pExAllData;
    PData        : pExAllData;
    EAC          : pExAllControl;
    MyLock       : FileLock;
    AnyMore      : BOOLEAN;
    FileList     : pList;
    DirList      : pList;
    tempnode     : pFPCNode;
    Buffer       : PChar;
    i,temp       : longint;
    TotalSize    : longint;
    TheDir       : string;

PROCEDURE CleanUp(TheMsg : STRING; ErrCode : INTEGER);
BEGIN
    IF EAC <> NIL THEN FreeDosObject(DOS_EXALLCONTROL,EAC);
    IF MyLock <> 0 THEN UnLock(MyLock);
    IF ExData <> NIL THEN ExecFreeMem(ExData,BufferSize);
    IF DirList <> NIL THEN DestroyList(DirList);
    IF FileList <> NIL THEN DestroyList(FileList);
    IF Buffer <> NIL THEN StrDispose(Buffer);
    IF TheMsg <> '' THEN WriteLN(TheMsg);
    Halt(ErrCode);
END;

PROCEDURE Usage;
BEGIN
    Write(CSI, '1m', 'DirDemo'#10,CSI,'0m', 'For FPC Pascal USAGE: DirDemo ThePath'#10);
    CleanUp('',0);
END;

BEGIN
    Buffer := StrAlloc(255);
    IF ParamCount <> 1 then Usage;
    TheDir := ParamStr(1);
    CreateList(FileList);
    CreateList(DirList);
    TotalSize := 0;

    EAC := AllocDosObject(DOS_EXALLCONTROL,NIL);
    IF EAC = NIL THEN CleanUp('No AllocDosObject',10);

    ExData := AllocMem(BufferSize,0);
    EAC^.eac_LastKey := 0;
    EAC^.eac_MatchString := NIL;
    EAC^.eac_MatchFunc := NIL;
    MyLock:=Lock(pas2c(TheDir),SHARED_LOCK);
    IF MyLock=0 THEN CleanUp('No lock on directory',10);

    REPEAT
        AnyMore := ExAll(MyLock,ExData,BufferSize,ED_SIZE,EAC);
        temp := IOErr;
        PData := ExData;
        FOR i := 1 TO EAC^.eac_Entries DO BEGIN
            IF PData^.ed_Type >= 0 THEN BEGIN
                tempnode := AddNewNode(DirList,PData^.ed_Name);
            END ELSE BEGIN
                tempnode := AddNewNode(FileList,PData^.ed_Name);
                tempnode^.ln_Size := PData^.ed_Size;
            END;
            PData := PData^.ed_Next;
        END;
    UNTIL (AnyMore=FALSE) AND (temp=ERROR_NO_MORE_ENTRIES);

    SortList(DirList);
    SortList(FileList);

    Write(CSI, '1m');
    Write(CSI, '32m');
    WriteLN('Directory of: "', TheDir,'"');
    tempnode := GetFirstNode(DirList);

    FOR i := 1 TO NodesInList(DirList) DO BEGIN
        printf('%-30s  <DIR>'#10,[long(GetNodeData(tempnode))]);
        tempnode := GetNextNode(tempnode);
    END;
    Write(CSI, '0m');
    tempnode := GetFirstNode(FileList);
    FOR i := 1 TO NodesInList(FileList) DO BEGIN
        printf('%-30s%7ld'#10 ,[long(GetNodeData(tempnode)),tempnode^.ln_Size]);
        TotalSize := TotalSize + tempnode^.ln_Size;
        tempnode := GetNextNode(tempnode);
    END;

    WriteLN('The total size is ',TotalSize,' Byte.');
    CleanUp('',0);
END.
