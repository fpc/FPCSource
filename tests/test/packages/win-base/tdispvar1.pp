{ %TARGET=win32,win64 }
{ %NOTE=This test requires an installed OpenOffice }
program ttt;

{$ifdef fpc}
{$mode delphi}
{$endif fpc}

uses
  Windows, SysUtils, Classes, ComObj, ActiveX, Variants;

var StarOffice : Variant;
	Document : Variant;

function TSampleCode_Connect() : boolean;
begin
    if  VarIsEmpty(StarOffice) then
        StarOffice := CreateOleObject('com.sun.star.ServiceManager');

    Result := not (VarIsEmpty(StarOffice) or VarIsNull(StarOffice));
end;

function TSampleCode_CreateDocument(bReadOnly : boolean) : boolean;
var
    StarDesktop : Variant;
    LoadParams : Variant;
    CoreReflection : Variant;
    PropertyValue : Variant;
    AutoObject : Variant;
    TextObject : Variant;
    Cursor : Variant;
begin
   StarDesktop := StarOffice.createInstance('com.sun.star.frame.Desktop');

   if (bReadOnly) then begin
        LoadParams := VarArrayCreate([0, 0], varVariant);
        CoreReflection := StarOffice.createInstance('com.sun.star.reflection.CoreReflection');

        CoreReflection.forName('com.sun.star.beans.PropertyValue').
			createObject(PropertyValue); // CoreReflection().forName().createObject() bring to "Illegal qualifier"
        AutoObject := CoreReflection.forName('com.sun.star.beans.PropertyValue');
	AutoObject.createObject(PropertyValue);

	PropertyValue.Name := 'ReadOnly'; 	// "Arg cant be assigned" and
        PropertyValue.Value := true;		//	"Incompatimle types: const string, untyped expected"

        LoadParams[0] := PropertyValue;
   end
   else
        LoadParams := VarArrayCreate([0, -1], varVariant);

   Document := StarDesktop.LoadComponentFromURL( 'private:factory/swriter', '_blank', 0,  LoadParams);
   if not bReadOnly then begin
       TextObject := Document.Text;
       Cursor := TextObject.createTextCursor;
       TextObject.insertString(Cursor,'Output of FPC Test tdispvar1.pp',False);
	  // works with D7, but not FPC
   end;

   Result := not (VarIsEmpty(Document) or VarIsNull(Document));
end;

begin
	CoInitialize(nil);
	TSampleCode_Connect();
        TSampleCode_CreateDocument(false);
end.
