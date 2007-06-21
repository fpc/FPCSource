{ Source provided for Free Pascal Bug Report 2043 }
{ Submitted by "Luis Castedo" on  2002-07-16 }
{ e-mail: castedo@elai.upm.es }
program tb1;

{$MODE TP}

uses
  Objects;

const
  csFName1 = 'tb1_1.tmp';
  csFName2 = 'tb1_2.tmp';

var
  pStream1: PStream;
  pStream2: PStream;
  lAux    : Longint;
  error : boolean;
  f : file;
begin
  error := false;
  Write('Error checking for object streams...');
  { Legal operation on pStream1 }
  pStream1 := New(PDosStream, Init(csFName1, stCreate));
  { Faulty operation on pStream2 }
  pStream2 := New(PDosStream, Init(csFName2, stOpenRead));
  if pStream2^.Status = stOk then
     error := true;

  { Legal operation on pStream1 }
  pStream1^.Write(lAux, SizeOf(lAux));
  { Normally, if the values are not shared, this should be ok! }
  if pStream1^.Status <> stOk then
     error := true;

  pStream2^.Free;
  pStream1^.Free;

  Assign(f,csFName1);
  Erase(f);

  if error then
    Begin
      WriteLn('FAILED! Errors are mixed up!');
      halt(1);
    end
  else
      Writeln('Success!');
end.
