{ %FAIL }
{ Source provided for Free Pascal Bug Report 2053 }
{ Submitted by "Luis Castedo" on  2002-07-24 }
{ e-mail: castedo@elai.upm.es }
program tb2;

{$MODE TP}
{$C+}

type

  TMyRecord = record
    mr_sglDummy1: array[0..3] of Single;
    mr_lDummy2  : Longint;
    mr_iDummy3  : Integer;
    mr_iDummy4  : Integer;
  end;

{  TMyRecordArray = array[Integer] of TMyRecord;} { Error }
  TMyRecordArray = array[Longint] of TMyRecord; { OK }
  PMyRecordArray = ^TMyRecordArray;

var

  pArray: PMyRecordArray;

begin
  GetMem(pArray, 50 * SizeOf(TMyRecord));
  Assert(Assigned(pArray));

  WriteLn('pArray = ', Longint(pArray));
  WriteLn('@(pArray^[0]) = ', Longint(@(pArray^[0])));
  pArray^[0].mr_lDummy2 := 0;

  FreeMem(pArray, 50 * SizeOf(TMyRecord));

end.

{
   $Log$
   Revision 1.4  2002-10-15 15:48:25  carl
    - remove Pierre's diff.

   Revision 1.3  2002/10/15 06:38:29  pierre
    * really try to allocate more than 2Gb

   Revision 1.2  2002/10/09 16:56:46  carl
     * some cpu specific tests not run under other CPU's

   Revision 1.1  2002/09/27 21:09:56  carl
     + new bug report

}
