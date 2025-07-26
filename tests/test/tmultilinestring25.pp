program tmultilinestring25;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft Auto}

const
  Str1 = `SELECT o.*, C.Company
          from Orders O
          join Customer C
            on o.CustNo=C.ID
          where
            O.saledate=DATE '2001.03.20'`;

const
  Str2 =
    `SELECT o.*, C.Company
     from Orders O
     join Customer C
       on o.CustNo=C.ID
     where
       O.saledate=DATE '2001.03.20'`;

begin
  WriteLn(Str1);
  WriteLn(Str2);
end.
