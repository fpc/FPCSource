{ Parsed from Appkit.framework NSKeyValueBinding.h }

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
function NSIsControllerMarker(object_: id): Boolean; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSKEYVALUEBINDING_PAS_S}
{$define NSKEYVALUEBINDING_PAS_S}

{ External symbols }
var
  NSMultipleValuesMarker: id; cvar; external;
  NSNoSelectionMarker: id; cvar; external;
  NSNotApplicableMarker: id; cvar; external;

{ External string constants }
var
  NSObservedObjectKey: NSString; cvar; external;
  NSObservedKeyPathKey: NSString; cvar; external;
  NSOptionsKey: NSString; cvar; external;
  NSAlignmentBinding: NSString; cvar; external;
  NSAlternateImageBinding: NSString; cvar; external;
  NSAlternateTitleBinding: NSString; cvar; external;
  NSAnimateBinding: NSString; cvar; external;
  NSAnimationDelayBinding: NSString; cvar; external;
  NSArgumentBinding: NSString; cvar; external;
  NSAttributedStringBinding: NSString; cvar; external;
  NSContentArrayBinding: NSString; cvar; external;
  NSContentArrayForMultipleSelectionBinding: NSString; cvar; external;
  NSContentBinding: NSString; cvar; external;
  NSContentDictionaryBinding: NSString; cvar; external;
  NSContentHeightBinding: NSString; cvar; external;
  NSContentObjectBinding: NSString; cvar; external;
  NSContentObjectsBinding: NSString; cvar; external;
  NSContentSetBinding: NSString; cvar; external;
  NSContentValuesBinding: NSString; cvar; external;
  NSContentWidthBinding: NSString; cvar; external;
  NSCriticalValueBinding: NSString; cvar; external;
  NSDataBinding: NSString; cvar; external;
  NSDisplayPatternTitleBinding: NSString; cvar; external;
  NSDisplayPatternValueBinding: NSString; cvar; external;
  NSDocumentEditedBinding: NSString; cvar; external;
  NSDoubleClickArgumentBinding: NSString; cvar; external;
  NSDoubleClickTargetBinding: NSString; cvar; external;
  NSEditableBinding: NSString; cvar; external;
  NSEnabledBinding: NSString; cvar; external;
  NSExcludedKeysBinding: NSString; cvar; external;
  NSFilterPredicateBinding: NSString; cvar; external;
  NSFontBinding: NSString; cvar; external;
  NSFontBoldBinding: NSString; cvar; external;
  NSFontFamilyNameBinding: NSString; cvar; external;
  NSFontItalicBinding: NSString; cvar; external;
  NSFontNameBinding: NSString; cvar; external;
  NSFontSizeBinding: NSString; cvar; external;
  NSHeaderTitleBinding: NSString; cvar; external;
  NSHiddenBinding: NSString; cvar; external;
  NSImageBinding: NSString; cvar; external;
  NSIncludedKeysBinding: NSString; cvar; external;
  NSInitialKeyBinding: NSString; cvar; external;
  NSInitialValueBinding: NSString; cvar; external;
  NSIsIndeterminateBinding: NSString; cvar; external;
  NSLabelBinding: NSString; cvar; external;
  NSLocalizedKeyDictionaryBinding: NSString; cvar; external;
  NSManagedObjectContextBinding: NSString; cvar; external;
  NSMaximumRecentsBinding: NSString; cvar; external;
  NSMaxValueBinding: NSString; cvar; external;
  NSMaxWidthBinding: NSString; cvar; external;
  NSMinValueBinding: NSString; cvar; external;
  NSMinWidthBinding: NSString; cvar; external;
  NSMixedStateImageBinding: NSString; cvar; external;
  NSOffStateImageBinding: NSString; cvar; external;
  NSOnStateImageBinding: NSString; cvar; external;
  NSPredicateBinding: NSString; cvar; external;
  NSRecentSearchesBinding: NSString; cvar; external;
  NSRepresentedFilenameBinding: NSString; cvar; external;
  NSRowHeightBinding: NSString; cvar; external;
  NSSelectedIdentifierBinding: NSString; cvar; external;
  NSSelectedIndexBinding: NSString; cvar; external;
  NSSelectedLabelBinding: NSString; cvar; external;
  NSSelectedObjectBinding: NSString; cvar; external;
  NSSelectedObjectsBinding: NSString; cvar; external;
  NSSelectedTagBinding: NSString; cvar; external;
  NSSelectedValueBinding: NSString; cvar; external;
  NSSelectedValuesBinding: NSString; cvar; external;
  NSSelectionIndexesBinding: NSString; cvar; external;
  NSSelectionIndexPathsBinding: NSString; cvar; external;
  NSSortDescriptorsBinding: NSString; cvar; external;
  NSTargetBinding: NSString; cvar; external;
  NSTextColorBinding: NSString; cvar; external;
  NSTitleBinding: NSString; cvar; external;
  NSToolTipBinding: NSString; cvar; external;
  NSTransparentBinding: NSString; cvar; external;
  NSValueBinding: NSString; cvar; external;
  NSValuePathBinding: NSString; cvar; external;
  NSValueURLBinding: NSString; cvar; external;
  NSVisibleBinding: NSString; cvar; external;
  NSWarningValueBinding: NSString; cvar; external;
  NSWidthBinding: NSString; cvar; external;
  NSAllowsEditingMultipleValuesSelectionBindingOption: NSString; cvar; external;
  NSAllowsNullArgumentBindingOption: NSString; cvar; external;
  NSAlwaysPresentsApplicationModalAlertsBindingOption: NSString; cvar; external;
  NSConditionallySetsEditableBindingOption: NSString; cvar; external;
  NSConditionallySetsEnabledBindingOption: NSString; cvar; external;
  NSConditionallySetsHiddenBindingOption: NSString; cvar; external;
  NSContinuouslyUpdatesValueBindingOption: NSString; cvar; external;
  NSCreatesSortDescriptorBindingOption: NSString; cvar; external;
  NSDeletesObjectsOnRemoveBindingsOption: NSString; cvar; external;
  NSDisplayNameBindingOption: NSString; cvar; external;
  NSDisplayPatternBindingOption: NSString; cvar; external;
  NSContentPlacementTagBindingOption: NSString; cvar; external;
  NSHandlesContentAsCompoundValueBindingOption: NSString; cvar; external;
  NSInsertsNullPlaceholderBindingOption: NSString; cvar; external;
  NSInvokesSeparatelyWithArrayObjectsBindingOption: NSString; cvar; external;
  NSMultipleValuesPlaceholderBindingOption: NSString; cvar; external;
  NSNoSelectionPlaceholderBindingOption: NSString; cvar; external;
  NSNotApplicablePlaceholderBindingOption: NSString; cvar; external;
  NSNullPlaceholderBindingOption: NSString; cvar; external;
  NSRaisesForNotApplicableKeysBindingOption: NSString; cvar; external;
  NSPredicateFormatBindingOption: NSString; cvar; external;
  NSSelectorNameBindingOption: NSString; cvar; external;
  NSSelectsAllWhenSettingContentBindingOption: NSString; cvar; external;
  NSValidatesImmediatelyBindingOption: NSString; cvar; external;
  NSValueTransformerNameBindingOption: NSString; cvar; external;
  NSValueTransformerBindingOption: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSKEYVALUEBINDING_PAS_C}
{$define NSKEYVALUEBINDING_PAS_C}

{ NSKeyValueBindingCreationCategory }
  NSKeyValueBindingCreationCategory = objccategory external (NSObject)
    class procedure exposeBinding(binding: NSString); message 'exposeBinding:';
    function exposedBindings: NSArray; message 'exposedBindings';
    function valueClassForBinding(binding: NSString): Pobjc_class; message 'valueClassForBinding:';
    procedure bind_toObject_withKeyPath_options(binding: NSString; observable: id; keyPath: NSString; options: NSDictionary); message 'bind:toObject:withKeyPath:options:';
    procedure unbind(binding: NSString); message 'unbind:';
    function infoForBinding(binding: NSString): NSDictionary; message 'infoForBinding:';
    function optionDescriptionsForBinding(aBinding: NSString): NSArray; message 'optionDescriptionsForBinding:';
  end;

{ NSPlaceholdersCategory }
  NSPlaceholdersCategory = objccategory external (NSObject)
    class procedure setDefaultPlaceholder_forMarker_withBinding(placeholder: id; marker: id; binding: NSString); message 'setDefaultPlaceholder:forMarker:withBinding:';
    class function defaultPlaceholderForMarker_withBinding(marker: id; binding: NSString): id; message 'defaultPlaceholderForMarker:withBinding:';
  end;

{ NSEditorRegistrationCategory }
  NSEditorRegistrationCategory = objccategory external (NSObject)
    procedure objectDidBeginEditing(editor: id); message 'objectDidBeginEditing:';
    procedure objectDidEndEditing(editor: id); message 'objectDidEndEditing:';
  end;

{ NSEditorCategory }
  NSEditorCategory = objccategory external (NSObject)
    procedure discardEditing; message 'discardEditing';
    function commitEditing: Boolean; message 'commitEditing';
    procedure commitEditingWithDelegate_didCommitSelector_contextInfo(delegate: id; didCommitSelector: SEL; contextInfo: Pointer); message 'commitEditingWithDelegate:didCommitSelector:contextInfo:';
  end;

{$endif}
{$endif}
