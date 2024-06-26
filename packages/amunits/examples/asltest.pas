PROGRAM AslTest;

uses Exec, Utility, Asl, amsgbox;


{
     History:
     Now use TAGS and pas2c.
     Removed the opening of asl.library,
     handled by unit asl.
     1 Nov 1998.

     Added MessageBox for report.
     31 Jul 2000.

     Changed to use systemvartags and
     AllocAslRequestTags.
     09 Nov 2002.

     nils.sjoholm@mailbox.swipnet.se
}

VAR
    fr    : pFileRequester;
    dummy : BOOLEAN;
BEGIN

    fr := AllocAslRequestTags(ASL_FileRequest,[
                          ASLFR_InitialPattern, AsTag('#?'),
                          ASLFR_TitleText, AsTag('Test av ASL-Requester by NS'),
                          ASLFR_DoPatterns, AsTag(True),
                          TAG_DONE]);

    IF fr <> nil THEN BEGIN
        dummy := AslRequest(fr,NIL);
        if dummy then begin
           MessageBox('Test of Asl',
                      ' The path is:" ' +
                      ShortString(fr^.rf_Dir) + '"' +
                      chr(10) +
                      'And the file is: "' +
                      ShortString(fr^.rf_File) + '"',
                      'OK');
        end else MessageBox('Test of Asl','You canceled','OK');
        FreeAslRequest(fr);
    END;
END.
