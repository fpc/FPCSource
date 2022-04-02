{ %norun }


{$r+}

{$mode delphi}

    type
      TLanguages = (
        lOne,
        lTwo,
        lThree,
        lFour
      );

    const
      LANGUAGE_NONE = TLanguages(255);
    var
      Lang: TLanguages;

    begin
     Lang := LANGUAGE_NONE;  //line 20
    end.
