unit dbf_str;

interface

{$I dbf_common.inc}
{$I dbf_str.inc}

implementation

initialization

  STRING_FILE_NOT_FOUND               := 'Otwórz: brak pliku: "%s".';
  STRING_VERSION                      := 'TDbf V%d.%d';
  STRING_FEATURE_NOT_SUPPORTED_THIS_TABLELEVEL := 'Ta funkcja nie jest obs³ugiwana w typie bazy (tablelevel) %d';

  STRING_RECORD_LOCKED                := 'Rekord zablokowany.';
  STRING_WRITE_ERROR                  := 'Wyst¹pi³ b³¹d podczas zapisu. (Brak miejsca na dysku?)';
  STRING_KEY_VIOLATION                := 'Konflikt klucza. (Klucz jest obecny w pliku).'+#13+#10+
                                         'Indeks: %s'+#13+#10+'Rekord=%d Klucz=''%s''.';

  STRING_INVALID_DBF_FILE             := 'Nieprawid³owy plik DBF.';
  STRING_INVALID_DBF_FILE_FIELDERROR  := 'Nieprawid³owy plik DBF. B³êdna definicja pola.'; 
  STRING_FIELD_TOO_LONG               := 'Wartoœæ jest za d³uga: %d znaków (maksymalnie %d).';
  STRING_INVALID_FIELD_COUNT          := 'Nieprawid³owa liczba pól: %d (dozwolone 1 do 4095).';
  STRING_INVALID_FIELD_TYPE           := 'Nieprawid³owy typ pola ''%s'' dla pola ''%s''.';
  STRING_INVALID_VCL_FIELD_TYPE       := 'Nie mo¿na utworzyæ pola "%s", typ pola VCL %x nie jest obs³ugiwany przez DBF.';


  STRING_INDEX_BASED_ON_UNKNOWN_FIELD := 'Indeks bazuje na nieznanym polu "%s".';
  STRING_INDEX_BASED_ON_INVALID_FIELD := 'Typ pola "%s" jest nieprawid³owy dla indeksów.';
  STRING_INDEX_EXPRESSION_TOO_LONG    := 'Wynik indeksu dla "%s" jest za d³ugi, >100 znaków (%d).';
  STRING_INVALID_INDEX_TYPE           := 'Nieprawid³owy typ indeksu: tylko string lub float.';
  STRING_CANNOT_OPEN_INDEX            := 'Nie mogê otworzyæ indeksu: "%s".';
  STRING_TOO_MANY_INDEXES             := 'Nie mogê utworzyæ indeksu: za du¿o indeksów w pliku.';
  STRING_INDEX_NOT_EXIST              := 'Indeks "%s" nie istnieje.';
  STRING_NEED_EXCLUSIVE_ACCESS        := 'Wymagany jest wy³¹czny dostêp dla tej operacji.';
end.

