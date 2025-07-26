program tmultilinestring27;

{$modeswitch MultiLineStrings}
{$MultiLineStringTrimLeft Auto}

resourcestring S =
    `This
       is
     a
       multi-line
     resource
       string`;

begin
  Write(S);
end.