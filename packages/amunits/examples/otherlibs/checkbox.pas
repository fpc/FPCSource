PROGRAM CheckBoxGad;

uses triton, tritonmacros, utility;

{
   A demo in FPC Pascal using triton.library

   Updated for fpc 1.0.7
   09 Jan 2003

   nils.sjoholm@mailbox.swipnet.se
}



VAR
     App      : pTR_App;
     Project  : pTR_Project;
     close_me : BOOLEAN;
     trmsg    : pTR_Message;
     dummy    : Longint;

procedure CleanUp(why : string; err : longint);
begin
    if assigned(Project) then TR_CloseProject(Project);
    if assigned(App) then TR_DeleteApp(App);
    if why <> '' then writeln(why);
    halt(err);
end;

begin

     App := TR_CreateAppTags([TRCA_Name,'Triton CheckBox',
                              TRCA_Release,'1.0',
                              TRCA_Date,'03-06-1998',
                              TAG_DONE]);

     if App = nil then CleanUp('Can''t create application',20);

      ProjectStart;
      WindowID(1);
      WindowTitle('CheckBox');
         VertGroupA;
            Space;
            HorizGroupAC;
               Space;
               TextID('_CheckBox',10);
               Space;
               CheckBox(10);
               Space;
            EndGroup;
            Space;
         EndGroup;
      EndProject;

  Project := TR_OpenProject(App,@tritontags);
    IF Project = NIL THEN CleanUp('Can''t create Project',20);
      close_me := FALSE;
      WHILE NOT close_me DO BEGIN
        dummy := TR_Wait(App,0);
        REPEAT
          trmsg := TR_GetMsg(App);
          IF trmsg <> NIL THEN BEGIN
            IF (trmsg^.trm_Project = Project) THEN BEGIN
               CASE trmsg^.trm_Class OF
                 TRMS_CLOSEWINDOW : begin
                                     if TR_GetCheckBox(Project,10) then
writeln('CheckBox was on')
                                        else writeln('CheckBox was off');
                                     close_me := True;
                                    end;
                 TRMS_ERROR:        WriteLN(TR_GetErrorString(trmsg^.trm_Data));
                 TRMS_NEWVALUE    : begin
                                      IF trmsg^.trm_ID = 10 then begin
                                          if trmsg^.trm_Data = 0 then
writeln('CheckBox off')
                                            else writeln('CheckBox on');
                                      end;
                                    end;
               END;
            END;
            TR_ReplyMsg(trmsg);
          END;
        UNTIL close_me OR (trmsg = NIL);
      END;
   CleanUp('',0);
end.
