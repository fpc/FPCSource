
The MemDS unit contains a TMemDataset class. It implements an in-memory
TDataset class. 

- The data can be loaded from/to Stream or file.
  This can be automated by setting the 'FileName' property.
- The data can be copied from another dataset.

Extra methods:

Property FileName : String;

If set, the data is loaded from this file when opening the dataset, and saved
when it is closed (and modified).

procedure CreateTable;

Creates the needed in-memory structures after fielddefs were added.
See testpop.pp for an example.

function DataSize : Integer;

Size of data in memory.

procedure Clear(ClearDefs : Boolean);
procedure Clear;

Clears the data. If cleardefs=True, then fielddefs are cleared.
If the definitions are cleared (default), the dataset is closed.

Procedure SaveToFile(AFileName : String);
Procedure SaveToFile(AFileName : String; SaveData : Boolean);

Save Data to file. If SaveData=False, then only metadata is written.

Procedure SaveToStream(F : TStream);
Procedure SaveToStream(F : TStream; SaveData : Boolean);

Save Data to stream. If SaveData=False, then only metadata is written.

Procedure LoadFromStream(F : TStream);

Load Data from a file created with savetofile.

Procedure LoadFromFile(AFileName : String);

Load Data from a stream, created with saveto stream.

Procedure CopyFromDataset(DataSet : TDataSet);
Procedure CopyFromDataset(DataSet : TDataSet; CopyData : Boolean);

Copy data from another dataset. Only fields (not fielddefs) are copied.
The dataset must be open. If CopyData is false, only the field definitions
are copied, but no data is copied. By default, data is also copied.

Property Modified : Boolean Read FModified;

Indicates whether the in-memory data was modified. When data is modified,
and the dataset is closed, the data is saved to stream if the 'filename'
property is set.
