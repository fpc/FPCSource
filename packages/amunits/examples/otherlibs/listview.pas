PROGRAM ListView;


uses exec, triton, tritonmacros, linklist, utility;



{
   A demo in FPC Pascal using triton.library

   Updated for fpc1.0.7
   11 Jan 2003.

   nils.sjoholm@mailbox.swipnet.se
}

const

     weekday : array [0..6] of pchar =  (
                'Monday',
                'Tuesday',
                'Wendsday',
                'Thursday',
                'Friday',
                'Saturday',
                'Sunday');

VAR
     Project  : pTR_Project;
     close_me : BOOLEAN;
     trmsg : pTR_Message;
     dummy : longint;
     mylist : pList;
     mynode : pFPCNode;
     num : longint;
     Triton_App : pTr_App;


PROCEDURE CleanExit(errstring : STRING; rc : Longint);
BEGIN
    if assigned(Triton_App) then TR_DeleteApp(Triton_App);
    IF assigned(Project) THEN TR_CloseProject(Project);
    IF Assigned(mylist) THEN DestroyList(mylist);
    IF errstring <> '' THEN WriteLn(errstring);
    Halt(rc)
END;

begin

    Triton_App := TR_CreateAppTags([
                     TRCA_Name,'Triton ListView',
                     TRCA_Release,'1.0',
                     TRCA_Date,'03-02-1998',
                     TAG_DONE]);

    if Triton_App = nil then Cleanexit('Can''t create application',20);

    CreateList(mylist);
    for dummy := 0 to 6 do begin
        mynode := AddNewNode(mylist,weekday[dummy]);
    end;
      ProjectStart;
      WindowID(1);
      WindowPosition(TRWP_CENTERDISPLAY);
      WindowTitle('Listview');
         HorizGroupA;
            Space;
            VertGroupA;
               Space;
               CenteredTextID('_List',7);
               Space;
               ListSS(mylist,7,0,2);
               Space;
            EndGroup;
            Space;
         EndGroup;
      EndProject;

  Project := TR_OpenProject(Triton_App,@tritontags);
    IF Project = NIL THEN CleanExit('Can''t create project',20);
      close_me := FALSE;
      WHILE NOT close_me DO BEGIN
        dummy := TR_Wait(Triton_App,0);
        REPEAT
          trmsg := TR_GetMsg(Triton_App);
          IF trmsg <> NIL THEN BEGIN
            IF (trmsg^.trm_Project = Project) THEN BEGIN
               CASE trmsg^.trm_Class OF
                 TRMS_CLOSEWINDOW : begin
                                       num := TR_GetValue(Project,7);
                                       mynode := GetNodeNumber(mylist,num);
                                       writeln('You picked number: ',num,' and the text was: ',GetNodeData(mynode));
                                       close_me := True;
                                    end;
                 TRMS_ERROR:        WriteLN(TR_GetErrorString(trmsg^.trm_Data));
                 TRMS_NEWVALUE    : IF trmsg^.trm_ID = 7 then writeln('You picked number: ',TR_GetValue(Project,7));
               END;
            END;
            TR_ReplyMsg(trmsg);
          END;
        UNTIL close_me OR (trmsg = NIL);
      END;
   Cleanexit('',0);
end.
