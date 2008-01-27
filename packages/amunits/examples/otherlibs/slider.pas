PROGRAM Slider;

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
     Triton_App : pTR_App;

Function IntToStr (I : Longint) : String;

     Var S : String;

     begin
      Str (I,S);
      IntToStr:=S;
     end;

begin

    Triton_App := TR_CreateAppTags([
                     TRCA_Name,' Triton Slider Demo' ,
                     TRCA_Release,' 1.0' ,
                     TRCA_Date,' 03-08-1998' ,
                     TAG_DONE]);

    if Triton_App <> nil then begin
      ProjectStart;
      WindowID(1);
      WindowTitle(' Slider' );
         VertGroupA;
            Space;
            HorizGroupAC;
               Space;
               TextID(' _Slider' ,1);
               Space;
               SliderGadget(1,100,50,1);
               Space;
               TextID(' 50' ,2); SetTRTag(TRAT_MinWidth,3);
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
                                       writeln(' The final number was: ' ,TR_GetValue(Project,1));
                                       close_me := True;
                                    end;
                 TRMS_ERROR:        WriteLN(TR_GetErrorString(trmsg^.trm_Data));
                 TRMS_NEWVALUE    : begin
                                       IF trmsg^.trm_ID = 1 then begin
                                          writeln(' The number is: ' ,trmsg^.trm_Data);
                                          TR_SetText(Project,2,IntToStr(trmsg^.trm_Data));
                                       end;
                                    end;
               END;
            END;
            TR_ReplyMsg(trmsg);
          END;
        UNTIL close_me OR (trmsg = NIL);
      END;
     TR_CloseProject(Project);
   end;
   TR_DeleteApp(Triton_App);
   END ELSE writeln(' Cant create Application');
end.
