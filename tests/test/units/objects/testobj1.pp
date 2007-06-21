Program Testobj1;

uses Objects;

const
 { Possible error codes returned to DOS by this program }
 EXIT_NOERROR   = 0;
 EXIT_NOTIFF    = 1;
 EXIT_DOSERROR  = 2;


(*************************************************************************)
(* Create a stream error procedure which will be called on error of the  *)
(* stream. Will Terminate executing program, as well as display info     *)
(* on the type of error encountered.                                     *)
(*************************************************************************)
Procedure StreamErrorProcedure(Var S: TStream); FAR;
Begin
 If S.Status = StError then
 Begin
  WriteLn('ERROR: General Access failure. Halting');
  Halt(EXIT_DOSERROR);
 end;
 If S.Status = StInitError then
 Begin
  Write('ERROR: Cannot Init Stream. Halting. ');
  { SPECIFIC TO DOS STREAMS }
  Case S.ErrorInfo of
  2: WriteLn('File not found.');
  3: WriteLn('Path not found.');
  5: Writeln('Access denied.');
  else
    WriteLn;
  end;
  Halt(EXIT_DOSERROR);
 end;
 If S.Status = StReadError then
 Begin
  WriteLn('ERROR: Read beyond end of Stream. Halting');
  Halt(EXIT_DOSERROR);
 end;
 If S.Status = StWriteError then
 Begin
  WriteLn('ERROR: Cannot expand Stream. Halting');
  Halt(EXIT_DOSERROR);
 end;
 If S.Status = StGetError then
 Begin
  WriteLn('ERROR: Get of Unregistered type. Halting');
  Halt(EXIT_DOSERROR);
 end;
 If S.Status = StPutError then
 Begin
  WriteLn('ERROR: Put of Unregistered type. Halting');
  Halt(EXIT_DOSERROR);
 end;
end;

Procedure WriteInformation;
{ Writes information about the program }
Begin
   WriteLn('Usage: fname.ext to check');
   Halt(EXIT_NOERROR);
end;

{ Program to demonstrate the TDosStream object. }


Const S : String = '0123456789';
      P : Pchar = '9876543210';

Var Stream : TBufStream;
    Buf : String;
    L : word;
    f : file;

begin
  StreamError:= @StreamErrorProcedure;
  Writeln ('Writing to stream : "01234567899876543210"');
  Stream.Init('testobj.tmp',stCreate,8);
  Stream.WriteStr (@S);
  Stream.StrWrite (P);
  Writeln ('Closing stream.');
  Stream.Done;
  Writeln ('Reading from stream : ');
  Stream.Init('testobj.tmp',StOpenRead,8);
  WriteLn('After opening');
  Writeln ('Reading (',S,') : ',Stream.ReadStr^);
  Writeln ('Reading (',P,') : ',Stream.StrRead);
  Writeln ('Closing stream.');
  Stream.Done;
  Writeln ('Same thing, using raw read method : ');
  Writeln ('Reading from stream : ');
  Stream.Init('testobj.tmp',StOpenRead,8);
  Stream.Read (Buf,11);
  Writeln ('Reading (',S,') : ',Buf);
  Stream.Read  (L,2);
  Stream.Read (Buf[1],L);
  Buf[0]:=chr(L);
  Writeln ('Reading (',P,') : ',Buf);
  Writeln ('Closing stream.');
  Stream.Done;
  Writeln ('Statistics about stream : ');
  Stream.Init('testobj.tmp',StOpenRead,8);
  Writeln ('Size     : ',Stream.GetSize);
  Writeln ('Position : ',Stream.GetPos);
  Writeln ('Reading (',S,') : ',Stream.ReadStr^);
  L:=Stream.GetPos;
  Writeln ('Position : ',L);
  Writeln ('Closing stream.');
  Stream.Done;
  Writeln ('Reading from stream : ');
  Stream.Init('testobj.tmp',StOpenRead,8);
  Writeln ('Seek to position :',L);
  Stream.Seek(L);
  Writeln ('Reading (',P,') : ',Stream.StrRead);
  Writeln ('Closing stream.');
  Stream.Done;
  Writeln ('Truncating stream to zero length.');
  Stream.Init('testobj.tmp',StOpenWrite,8);
  Stream.Truncate;
  Stream.Done;
  Assign(f,'testobj.tmp');
  Erase(f);
end.
