{$IFNDEF FPC_DOTTEDUNITS}
unit buildcollations;
{$ENDIF FPC_DOTTEDUNITS}

  interface

{$IFDEF FPC_DOTTEDUNITS}
    uses
      System.Collations.De, System.Collations.Es, System.Collations.Fr_ca, System.Collations.Ja, 
      System.Collations.Ko, System.Collations.Ru, System.Collations.Sv, System.Collations.Zh;
{$ELSE FPC_DOTTEDUNITS}
    uses
      collation_de, collation_es, collation_fr_ca, collation_ja, 
      collation_ko, collation_ru, collation_sv, collation_zh;
{$ENDIF FPC_DOTTEDUNITS}

  implementation

end.
