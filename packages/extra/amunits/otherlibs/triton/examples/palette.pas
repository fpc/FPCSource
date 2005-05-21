PROGRAM Palette;

uses triton,tritonmacros,utility;

{
  A demo in FPC Pascal using triton.library

  Updated for fpc 1.0.7
  11 Jan 2003.

  nils.sjoholm@mailbox.swipnet.se
}



var
  Triton_App : pTR_App;
  Project : pTR_Project;
  close_me : boolean;
  trmsg : pTR_Message;
  dummy : longint;


begin

    Triton_App := TR_CreateAppTags([
                     TRCA_Name,'Triton Palette Demo',
                     TRCA_Release,'1.0',
                     TRCA_Date,'03-06-1998',
                     TAG_DONE]);

    if Triton_App <> nil then begin
      ProjectStart;
      WindowID(1);
      WindowTitle('Palette');
         HorizGroupA;
            Space;
            VertGroupA;
               Space;
               CenteredTextID('_Palette',7);
               Space;
               PaletteGadget(1,7);
               Space;
            EndGroup;
            Space;
         EndGroup;
      EndProject;

  Project := TR_OpenProject(Triton_App,@tritontags);
    IF Project <> NIL THEN BEGIN
      close_me := FALSE;
      WHILE NOT close_me DO BEGIN
        dummy := TR_Wait(Triton_App,0);
        REPEAT
          trmsg := TR_GetMsg(Triton_App);
          IF trmsg <> NIL THEN BEGIN
            IF (trmsg^.trm_Project = Project) THEN BEGIN
               CASE trmsg^.trm_Class OF
                 TRMS_CLOSEWINDOW : begin
                                       writeln('Your final colour was ',TR_GetValue(Project,7));
                                       close_me := True;
                                    end;
                 TRMS_ERROR:        WriteLN(TR_GetErrorString(trmsg^.trm_Data));
                 TRMS_NEWVALUE    : IF trmsg^.trm_ID = 7 then writeln('You picked colour ',trmsg^.trm_Data);
               END;
            END;
            TR_ReplyMsg(trmsg);
          END;
        UNTIL close_me OR (trmsg = NIL);
      END;
     TR_CloseProject(Project);
   end;
   TR_DeleteApp(Triton_App);
   END ELSE writeln('Can''t create Application',20);
end.
