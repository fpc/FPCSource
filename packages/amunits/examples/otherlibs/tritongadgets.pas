PROGRAM GadgetDemo;

uses triton, tritonmacros, utility;

{
    If you have seen gadtoolsgadgets.c in the RKRM's then you
    have seen this program. One diffs is that this example
    is made in Triton. Much better.:)
    Jun 06 1998

    Updated for fpc 1.0.7
    11 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se
}




CONST
    MYGAD_SLIDER     = 1;
    MYGAD_SLIDERTEXT = 10;
    MYGAD_STRING1    = 2;
    MYGAD_STRING2    = 3;
    MYGAD_STRING3    = 4;
    MYGAD_BUTTON     = 5;

(* Range for the slider: *)
    SLIDER_MIN  = 1;
    SLIDER_MAX  = 20;
    SLIDER_DEF  = 10;
VAR
    Project     : pTR_Project;
    trmsg       : pTR_Message;
    quit        : Boolean;
    dummy       : longint;
    Triton_App  : pTR_App;

Function longToStr (I : Longint) : String;

     Var S : String;

     begin
      Str (I,S);
      longToStr:=S;
     end;


PROCEDURE CleanExit(errstring : STRING; rc : Integer);
BEGIN
    IF Project <> NIL THEN TR_CloseProject(Project);
    if Triton_App <> nil then TR_DeleteApp(Triton_App);
    IF errstring <> '' THEN WriteLn(errstring);
    Halt(rc)
END;



begin

    Triton_App := TR_CreateAppTags([
                     TRCA_Name,'TritonGadtools',
                     TRCA_LongName,'GadToolsDemo in Triton',
                     TRCA_Version,'0.01',
                     TRCA_Info,'Just a test of Triton',
                     TRCA_Release,'1.0',
                     TRCA_Date,'26-05-1998',
                     TAG_DONE]);

     if Triton_App = nil then CleanExit('Can''t create Application',20);

     ProjectStart;
     WindowID(1);
     WindowTitle('Instead of GadTools :)');
     WindowPosition(TRWP_CENTERTOP);
         HorizGroupA;
             Space;
             VertGroupA;
                 Space;
                LineArray;
                    BeginLine;
                        Space;
                        TextID('_Volume:',MYGAD_SLIDER); SetTRTag(TRAT_Flags,TROF_RIGHTALIGN);
                        Space;
                        SliderGadget(SLIDER_MIN,SLIDER_MAX,5,MYGAD_SLIDER);
                        Space;
                        TextID('5',MYGAD_SLIDERTEXT); SetTRTag(TRAT_MinWidth, 2);
                        Space;
                    EndLine;
                    SpaceS;
                    BeginLine;
                        Space;
                        TextID('_First:',MYGAD_STRING1); SetTRTag(TRAT_Flags, TROF_RIGHTALIGN);
                        Space;
                        StringGadget('Try pressing',MYGAD_STRING1); SetTRTag(TRAT_Value,50);
                        Space;
                    EndLine;
                    SpaceS;
                    BeginLine;
                        Space;
                        TextID('_Second:',MYGAD_STRING2); SetTRTag(TRAT_Flags, TROF_RIGHTALIGN);
                        Space;
                        StringGadget('TAB or Shift-TAB',MYGAD_STRING2); SetTRTag(TRAT_Value,50);
                        Space;
                    EndLine;
                    SpaceS;
                    BeginLine;
                        Space;
                        TextID('_Third:',MYGAD_STRING3); SetTRTag(TRAT_Flags, TROF_RIGHTALIGN);
                        Space;
                        StringGadget('To see what happens!',MYGAD_STRING3); SetTRTag(TRAT_Value,50);
                        Space;
                    EndLine;
                EndArray;
                Space;
                        CenteredButton('_Click Here',MYGAD_BUTTON);
                Space;
            EndGroup;
            Space;
        EndGroup;
     EndProject;



    Project := TR_OpenProject(Triton_App,@tritontags);
    IF Project = NIL THEN CleanExit('No project',20);

    quit := False;
    WHILE NOT quit DO BEGIN
      dummy := TR_Wait(Triton_App,0);
      REPEAT
        trmsg := TR_GetMsg(Triton_App);
        IF trmsg <> NIL THEN BEGIN
          IF (trmsg^.trm_Project = Project) THEN BEGIN
             CASE trmsg^.trm_Class OF

               TRMS_CLOSEWINDOW : quit := True;

               TRMS_NEWVALUE    :
                       BEGIN
                       case trmsg^.trm_ID of
                         MYGAD_SLIDER  : begin
                                         TR_SetText(Project,MYGAD_SLIDERTEXT,LongToStr(trmsg^.trm_Data));
                                         writeln('Slider at level ',trmsg^.trm_Data);
                                         end;
                         MYGAD_STRING1 : writeln('String Gadget 1: "',TR_GetString(Project,MYGAD_STRING1),'".');
                         MYGAD_STRING2 : writeln('String Gadget 2: "',TR_GetString(Project,MYGAD_STRING2),'".');
                         MYGAD_STRING3 : writeln('String Gadget 3: "',TR_GetString(Project,MYGAD_STRING3),'".');
                       END;
                       END;
               TRMS_ACTION :
                      BEGIN
                      if trmsg^.trm_ID = MYGAD_BUTTON then begin
                                           TR_SetValue(Project,MYGAD_SLIDER,SLIDER_DEF);
                                           TR_SetText(Project,MYGAD_SLIDERTEXT,LongToStr(SLIDER_DEF));
                                           writeln('Button was pressed, slider reset to 10.');
                                         end;
                      END;
               TRMS_ERROR:        WriteLN(TR_GetErrorString(trmsg^.trm_Data));
             END;
           END;
           TR_ReplyMsg(trmsg);
         END;
       UNTIL quit OR (trmsg = NIL);
    END;
    CleanExit('',0);
END.
