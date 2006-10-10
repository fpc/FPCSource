
{ Count Lines/Words/Chars }

program wc;


uses SysUtils;

var
    nl, nw, nc: longint;
    Buf: array[1..4096] of byte;
    NumRead: Integer;

    A: Integer;
    Tmp: String;
    TmpPos : Byte;
    Ch: String;
    InWord: Boolean;
begin
    nl := 0;
    nc := 0;
    nw := 0;
    InWord := False;
    NumRead := FileRead(StdInputHandle, Buf, 4096);
    While NumRead > 0 Do
    begin
        Inc(nc, NumRead);
        For A := 1 To NumRead Do
        begin
            if Buf[A] = 10 Then Inc(nl);
            if Buf[A] = 13 Then Dec(nc);
            if (Buf[A] = 32) Or (Buf[A] = 10) Or (Buf[A] = 13) Or (Buf[A] = 9) Then
                InWord := False
            else
            begin
                If InWord = False Then
                begin
                    Inc(nw);
                    InWord := True;
                end;
            end;
        end;
        NumRead := FileRead(StdInputHandle, Buf, 4096);
    end;
    WriteLn(IntToStr(nl) + ' ' + IntToStr(nw) + ' ' + IntToStr(nc));
end.
