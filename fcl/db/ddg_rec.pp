unit DDG_Rec;

interface

uses sysutils;

type

  // arbitary-length array of char used for name field
  TNameStr = array[0..31] of char;

  // this record info represents the "table" structure:
  PDDGData = ^TDDGData;
  TDDGData = record
    Name: TNameStr;
    Height: Extended;
    LongField : Longint;
    ShoeSize: SmallInt;
    WordField : Word;
    DatetimeField : TDateTime;
    TimeField : TDateTime;
    DateField : TDateTime;
    Even : Boolean;
  end;

  // Pascal file of record which holds "table" data:
  TDDGDataFile = file of TDDGData;


implementation

end.
  $Log: ddg_rec.pp,v $
  Revision 1.4  2005/02/14 17:13:12  peter
    * truncate log

}
