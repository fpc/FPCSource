PROGRAM AslTest;

uses Exec, Utility, Asl;

{$I tagutils.inc}

VAR
    fr    : pFileRequester;
    dummy : BOOLEAN;
    thetags : array [0..3] of tTagItem;
BEGIN
    AslBase := OpenLibrary(AslName,37);
    IF AslBase <> NIL THEN BEGIN
       thetags[0] := TagItem(ASLFR_InitialPattern,Longint(PChar('#?'#0)));
       thetags[1] := TagItem(ASLFR_TitleText,Longint(PChar('Test av ASL-Requester by NS'#0)));
       thetags[2] := TagItem(ASLFR_DoPatterns,1);
       thetags[3].ti_Tag := TAG_DONE;

       fr := AllocAslRequest(ASL_FileRequest,@thetags);
       IF fr <> nil THEN BEGIN
           dummy := AslRequest(fr,NIL);
           if dummy then begin
              writeln('The path is     :',fr^.rf_Dir);
              writeln('And the file is :',fr^.rf_File);
           end else writeln('You canceled');
           FreeAslRequest(fr);
       END;
    CloseLibrary(AslBase);
    END else writeln('no asl.library');
END.



