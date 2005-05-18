PROGRAM Stringtest;

uses triton, tritonmacros, utility;

{
   A demo in FPC Pascal using triton.library

   Updated for fpc 1.0.7
   11 Jan 2003.

   nils.sjoholm@mailbox.swipnet.se
}



VAR
     Project  : pTR_Project;
     close_me : BOOLEAN;
     trmsg : pTR_Message;
     dummy : longint;
     App : pTR_App;

begin

    App := TR_CreateAppTags([
                     TRCA_Name,'Triton String Demo',
                     TRCA_Release,'1.0',
                     TRCA_Date,'03-06-1998',
                     TAG_DONE]);
    if App <> nil then begin
      ProjectStart;
      WindowID(1);
      WindowTitle('String');
         VertGroupA;
            Space;
            HorizGroupAC;
               Space;
               TextID('_String',3);
               Space;
               StringGadget('Please change',3);
               Space;
            EndGroup;
            Space;
         EndGroup;
      EndProject;

  Project := TR_OpenProject(App,@tritontags);
    IF Project <> NIL THEN BEGIN
      close_me := FALSE;
      WHILE NOT close_me DO BEGIN
        dummy := TR_Wait(App,0);
        REPEAT
          trmsg := TR_GetMsg(App);
          IF trmsg <> NIL THEN BEGIN
            IF (trmsg^.trm_Project = Project) THEN BEGIN
               CASE trmsg^.trm_Class OF
                 TRMS_CLOSEWINDOW : begin
                                       writeln('The text was: ',TR_GetString(Project,3));
                                       close_me := True;
                                    end;
                 TRMS_ERROR:        WriteLN(TR_GetErrorString(trmsg^.trm_Data));
                 TRMS_NEWVALUE    : IF trmsg^.trm_ID = 3 then writeln('<RETURN> or <TAB> was pressed in stringgadget');
               END;
            END;
            TR_ReplyMsg(trmsg);
          END;
        UNTIL close_me OR (trmsg = NIL);
      END;
     TR_CloseProject(Project);
   end;
   TR_DeleteApp(App);
   END ELSE writeln('Can''t create application');
end.
