{ Parsed from Appkit.framework NSKeyValueBinding.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:02 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSKEYVALUEBINDING_PAS_T}
{$define NSKEYVALUEBINDING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSKEYVALUEBINDING_PAS_R}
{$define NSKEYVALUEBINDING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSKEYVALUEBINDING_PAS_F}
{$define NSKEYVALUEBINDING_PAS_F}

{ Functions }
function NSIsControllerMarker(object_: id): Boolean; cdecl; external name 'NSIsControllerMarker';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSKEYVALUEBINDING_PAS_T}
{$define NSKEYVALUEBINDING_PAS_T}

{ External symbols }
var
  NSMultipleValuesMarker: id; external name '_NSMultipleValuesMarker';
  NSNoSelectionMarker: id; external name '_NSNoSelectionMarker';
  NSNotApplicableMarker: id; external name '_NSNotApplicableMarker';

{$endif}
{$endif}
