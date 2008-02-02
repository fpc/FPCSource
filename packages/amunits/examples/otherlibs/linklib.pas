PROGRAM LinkLib;

uses exec, triton, tritonmacros, linklist,
           amigautils,strings, easyasl, utility;

{
   A demo in FPC Pascal using triton.library

   Updated for fpc 1.0.7
   09 Jan 2003.

   nils.sjoholm@mailbox.swipnet.se
}



VAR
     Project  : pTR_Project;
     mylist   : pList;
     llist    : pList;
     pdummy   : ARRAY [0..108] OF Char;
     path     : PChar;
     Triton_App : pTR_App;

const

    LibListGadID   = 1;
    AddGadID       = 2;
    RemoveGadID    = 3;
    RemAllGadID    = 4;
    UpGadID        = 5;
    DownGadID      = 6;
    OkButton       = 7;
    CancelButton   = 8;


PROCEDURE CleanExit(errstring : STRING; rc : Longint);
BEGIN
    IF assigned(Project) THEN TR_CloseProject(Project);
    IF Assigned(mylist) THEN DestroyList(mylist);
    IF Assigned(llist) THEN DestroyList(llist);
    IF errstring <> '' THEN WriteLn(errstring);
    Halt(rc)
END;

PROCEDURE disablegads;
VAR
   dummy : Longint;
BEGIN
   IF NodesInList(mylist) > 0 THEN dummy := 0
      ELSE dummy := 1;

   TR_SetAttribute(Project,RemoveGadID,TRAT_Disabled,dummy);
   TR_SetAttribute(Project,RemAllGadID,TRAT_Disabled,dummy);
   TR_SetAttribute(Project,UpGadID,TRAT_Disabled,dummy);
   TR_SetAttribute(Project,DownGadID,TRAT_Disabled,dummy);
END;

PROCEDURE readinlist;
VAR
   dummy : BOOLEAN;
   temp : pFPCNode;
BEGIN
   dummy := FileToList('ram:fpclistoffiles',mylist);
   IF dummy THEN BEGIN
      temp := GetFirstNode(mylist);
      IF temp <> NIL THEN StrCopy(path,PathOf(GetNodeData(temp)));
      temp := GetLastNode(mylist);
      IF StrLen(GetNodeData(temp)) = 0 THEN RemoveLastNode(mylist);
   END;
END;

PROCEDURE addfiles;

VAR
  dummy    : BOOLEAN;
  mynode,tempnode   : pFPCNode;
  temp  : Longint;

BEGIN
  dummy := GetMultiAsl('Pick a file or two :)',path,llist,NIL,NIL);
  IF dummy THEN BEGIN
     mynode := GetFirstNode(llist);
     FOR temp := 1 TO NodesInList(llist) DO BEGIN
        tempnode := AddNewNode(mylist,(PathAndFile(path,GetNodeData(mynode))));
        mynode := GetNextNode(mynode);
     END;
     TR_UpdateListView(Project,LibListGadID,mylist);
     TR_SetValue(Project,LibListGadID,0);
     disablegads;
     ClearList(llist);
  END;
END;

PROCEDURE removelib;
VAR
   num : Longint;
   mynode : pFPCNode;
   strbuf : ARRAY [0..255] OF Char;
   buffer : PChar;
   dummy : Longint;
BEGIN
   buffer := @strbuf;
   num := TR_GetValue(Project,LibListGadID);
   mynode := GetNodeNumber(mylist,num);

   dummy := TR_EasyRequestTags(Triton_App,'Sure you want to delete'+#10+
                            strpas(GetNodeData(mynode)),'_Remove|_Cancel',[
                            TREZ_LockProject,Project,
                            TREZ_Title,'Delete this file?',
                            TREZ_Activate,1,
                            TAG_END]);
   IF dummy = 1 THEN BEGIN
      DeleteNode(mynode);
      TR_UpdateListView(Project,LibListGadID,mylist);
      TR_SetValue(Project,LibListGadID,0);
      disablegads;
   END;
END;

PROCEDURE removeall;
VAR
   dummy : Longint;
BEGIN
   dummy := TR_EasyRequestTags(Triton_App,'Sure you want to remove all files?',
                                      '_Remove|_Cancel',[
                                      TREZ_LockProject,Project,
                                      TREZ_Title,'Delete all?',
                                      TREZ_Activate,1,
                                      TAG_END]);
   IF dummy = 1 THEN BEGIN
      ClearList(mylist);
      TR_UpdateListView(Project,LibListGadID,mylist);
      disablegads;
   END;
END;

PROCEDURE savethelist;
VAR
   dummy : BOOLEAN;
BEGIN
   dummy := ListToFile('Ram:fpclistoffiles',mylist);
END;

PROCEDURE movedown;
VAR
   num : INTEGER;
   mynode : pFPCNode;
BEGIN
   num := TR_GetValue(project,LibListGadID);
   IF num < (NodesInList(mylist)-1) THEN BEGIN
      mynode := GetNodeNumber(mylist,num);
      IF mynode <> NIL THEN BEGIN
          MoveNodeDown(mylist,mynode);
          TR_UpdateListView(Project,LibListGadID,mylist);
          TR_SetValue(Project,LibListGadID,num + 1);
      END;
   END;
END;

PROCEDURE moveup;
VAR
   num : Longint;
   mynode : pFPCNode;
BEGIN
   num := TR_GetValue(project,LibListGadID);
   IF num > 0 THEN BEGIN
      mynode := GetNodeNumber(mylist,num);
      IF mynode <> NIL THEN BEGIN
          MoveNodeUp(mylist,mynode);
          TR_UpdateListView(Project,LibListGadID,mylist);
          TR_SetValue(Project,LibListGadID,num-1);
      END;
   END;
END;

PROCEDURE do_demo;
VAR
    close_me : BOOLEAN;
    trmsg : pTR_Message;
    dummy : Longint;

BEGIN
    ProjectStart;
               WindowID(1);
               WindowPosition(TRWP_CENTERDISPLAY);
               WindowTitle('TritonListViewDemo in FPC Pascal');
                  HorizGroupAC;
                     Space;
                     VertGroupAC;
                     Space;
                     NamedSeparator('List of files');
                        Space;
                        ListSSM(mylist,LibListGadID,0,0,25);
                        Space;
                     EndGroup;
                     Space;
                     VertSeparator;
                     Space;
                     SetTRTag(TRGR_Vert, TRGR_ALIGN OR TRGR_FIXHORIZ);
                        Space;
                        Button('_Add...',AddGadID);
                        SpaceS;
                        Button('_Remove...',RemoveGadID);
                        SpaceS;
                        Button('Re_move All...',RemAllGadID);
                        SpaceS;
                        Button('_Up',UpGadID);
                        SpaceS;
                        Button('_Down',DownGadID);
                        VertGroupS;Space;EndGroup;
                        Button('_Ok',OkButton);
                        SpaceS;
                        Button('_Cancel',CancelButton);
                        Space;
                     EndGroup;
                     Space;
                  EndGroup;
               EndProject;

    Project := TR_OpenProject(Triton_App,@tritontags);
    IF Project <> NIL THEN BEGIN
      disablegads;
      close_me := FALSE;
      WHILE NOT close_me DO BEGIN
        dummy := TR_Wait(Triton_App,0);
        REPEAT
          trmsg := TR_GetMsg(Triton_App);
          IF trmsg <> NIL THEN BEGIN
            IF (trmsg^.trm_Project = Project) THEN BEGIN
               CASE trmsg^.trm_Class OF
                 TRMS_CLOSEWINDOW : close_me := True;
                 TRMS_ERROR:        WriteLN(TR_GetErrorString(trmsg^.trm_Data));
                 TRMS_ACTION :
                 BEGIN
                 CASE trmsg^.trm_ID OF
                   AddGadID : addfiles;
                   UpGadID : moveup;
                   DownGadID : movedown;
                   RemoveGadID : removelib;
                   RemAllGadID : removeall;
                   OkButton : BEGIN savethelist; close_me := True; END;
                   CancelButton : close_me := True;
                 END;
               END;
               ELSE
               END;
            END;
            TR_ReplyMsg(trmsg);
          END
        UNTIL close_me OR (trmsg = NIL);
      END;
    END ELSE WriteLN(TR_GetErrorString(TR_GetLastError(Triton_App)));
END;


BEGIN  { Main }
        Triton_App := TR_CreateAppTags([
                       TRCA_Name,'Triton ListView Demo',
                       TRCA_LongName,'Demo of ListView in Triton, made in FPC Pascal',
                       TRCA_Version,'0.01',
                       TRCA_Info,'Uses tritonsupport',
                       TRCA_Release,'11',
                       TRCA_Date,'03-02-1998',
                       TAG_END]);
        if Triton_App <> nil then begin
        path := @pdummy;
        StrpCopy(path,'sys:');
        CreateList(mylist);
        CreateList(llist);
        readinlist;
        do_demo;
        CleanExit('',0);
     END
     ELSE CleanExit('Can''t create application',20);
END.
