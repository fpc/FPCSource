program const_internal_error;

const
  ACONSTANT = 3;

  b         = 5;

  { can do this: }

  {$if ACONSTANT <> b}
    {$MESSAGE 'ACONSTANT is not equal to b'}
  {$endif}


type
  PRECORD = ^TRECORD;
  TRECORD = record
    FirstField  : integer;
    SecondField : DWORD;
  end;


const
  { can define constants that represent the field offsets                     }

  RECORD_FIRSTFIELD_OFFSET  = SizeInt(@PRECORD(nil)^.FirstField);
  RECORD_SECONDFIELD_OFFSET = SizeInt(@PRECORD(nil)^.SecondField);

  { CAN NOT do this: }

  {$if RECORD_FIRSTFIELD_OFFSET <> 5}
    {$MESSAGE 'RECORD_FIRSTFIELD_OFFSET is not equal to 5'}
  {$endif}

  { NOR this:        }

  {$if RECORD_FIRSTFIELD_OFFSET <> RECORD_SECONDFIELD_OFFSET}
    {$MESSAGE 'RECORD_FIRSTFIELD_OFFSET is not equal to RECORD_SECONDFIELD_OFFSET'}
  {$endif}


begin

  readln;
end.
