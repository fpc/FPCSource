unit DDG_Rec;

interface

type

  // arbitary-length array of char used for name field
  TNameStr = array[0..31] of char;

  // this record info represents the "table" structure:
  PDDGData = ^TDDGData;
  TDDGData = record
    Name: TNameStr;
    Height: Extended;
    ShoeSize: Integer;
  end;

  // Pascal file of record which holds "table" data:
  TDDGDataFile = file of TDDGData;


implementation

end.
