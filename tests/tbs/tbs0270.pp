unit tbs0270;

{$mode tp}

interface

const
   s='df';

{$IFDEF VDE}
   SFilterOpen      = ' (*.nnn)|*.nnn' + '|' + 'Alle Files (*.*)|*.*';
   SFilterSave      = ' (*.nnn)|*.nnn';
   SFilterOpen2     = ' (*.vvv)|*.vvv' + '|' + 'All Files (*.*)|*.*';
   SFilterSave2     = ' (*.vvv)|*.vvv';
   SFilterOpen3     = ' (*.eee)|*.eee' + '|' + 'All Files (*.*)|*.*';
   SFilterSave3     = ' (*.eee)|*.eee';
{$ENDIF}

implementation

end.
