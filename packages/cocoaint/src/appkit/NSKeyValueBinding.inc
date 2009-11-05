{ Parsed from Appkit.framework NSKeyValueBinding.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


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
{$ifndef NSKEYVALUEBINDING_PAS_S}
{$define NSKEYVALUEBINDING_PAS_S}

{ External symbols }
var
  NSMultipleValuesMarker: id; external name '_NSMultipleValuesMarker';
  NSNoSelectionMarker: id; external name '_NSNoSelectionMarker';
  NSNotApplicableMarker: id; external name '_NSNotApplicableMarker';

{ External string constants }
var
  NSObservedObjectKey: NSString; external name '_NSObservedObjectKey';
  NSObservedKeyPathKey: NSString; external name '_NSObservedKeyPathKey';
  NSOptionsKey: NSString; external name '_NSOptionsKey';
  NSAlignmentBinding: NSString; external name '_NSAlignmentBinding';
  NSAlternateImageBinding: NSString; external name '_NSAlternateImageBinding';
  NSAlternateTitleBinding: NSString; external name '_NSAlternateTitleBinding';
  NSAnimateBinding: NSString; external name '_NSAnimateBinding';
  NSAnimationDelayBinding: NSString; external name '_NSAnimationDelayBinding';
  NSArgumentBinding: NSString; external name '_NSArgumentBinding';
  NSAttributedStringBinding: NSString; external name '_NSAttributedStringBinding';
  NSContentArrayBinding: NSString; external name '_NSContentArrayBinding';
  NSContentArrayForMultipleSelectionBinding: NSString; external name '_NSContentArrayForMultipleSelectionBinding';
  NSContentBinding: NSString; external name '_NSContentBinding';
  NSContentDictionaryBinding: NSString; external name '_NSContentDictionaryBinding';
  NSContentHeightBinding: NSString; external name '_NSContentHeightBinding';
  NSContentObjectBinding: NSString; external name '_NSContentObjectBinding';
  NSContentObjectsBinding: NSString; external name '_NSContentObjectsBinding';
  NSContentSetBinding: NSString; external name '_NSContentSetBinding';
  NSContentValuesBinding: NSString; external name '_NSContentValuesBinding';
  NSContentWidthBinding: NSString; external name '_NSContentWidthBinding';
  NSCriticalValueBinding: NSString; external name '_NSCriticalValueBinding';
  NSDataBinding: NSString; external name '_NSDataBinding';
  NSDisplayPatternTitleBinding: NSString; external name '_NSDisplayPatternTitleBinding';
  NSDisplayPatternValueBinding: NSString; external name '_NSDisplayPatternValueBinding';
  NSDocumentEditedBinding: NSString; external name '_NSDocumentEditedBinding';
  NSDoubleClickArgumentBinding: NSString; external name '_NSDoubleClickArgumentBinding';
  NSDoubleClickTargetBinding: NSString; external name '_NSDoubleClickTargetBinding';
  NSEditableBinding: NSString; external name '_NSEditableBinding';
  NSEnabledBinding: NSString; external name '_NSEnabledBinding';
  NSExcludedKeysBinding: NSString; external name '_NSExcludedKeysBinding';
  NSFilterPredicateBinding: NSString; external name '_NSFilterPredicateBinding';
  NSFontBinding: NSString; external name '_NSFontBinding';
  NSFontBoldBinding: NSString; external name '_NSFontBoldBinding';
  NSFontFamilyNameBinding: NSString; external name '_NSFontFamilyNameBinding';
  NSFontItalicBinding: NSString; external name '_NSFontItalicBinding';
  NSFontNameBinding: NSString; external name '_NSFontNameBinding';
  NSFontSizeBinding: NSString; external name '_NSFontSizeBinding';
  NSHeaderTitleBinding: NSString; external name '_NSHeaderTitleBinding';
  NSHiddenBinding: NSString; external name '_NSHiddenBinding';
  NSImageBinding: NSString; external name '_NSImageBinding';
  NSIncludedKeysBinding: NSString; external name '_NSIncludedKeysBinding';
  NSInitialKeyBinding: NSString; external name '_NSInitialKeyBinding';
  NSInitialValueBinding: NSString; external name '_NSInitialValueBinding';
  NSIsIndeterminateBinding: NSString; external name '_NSIsIndeterminateBinding';
  NSLabelBinding: NSString; external name '_NSLabelBinding';
  NSLocalizedKeyDictionaryBinding: NSString; external name '_NSLocalizedKeyDictionaryBinding';
  NSManagedObjectContextBinding: NSString; external name '_NSManagedObjectContextBinding';
  NSMaximumRecentsBinding: NSString; external name '_NSMaximumRecentsBinding';
  NSMaxValueBinding: NSString; external name '_NSMaxValueBinding';
  NSMaxWidthBinding: NSString; external name '_NSMaxWidthBinding';
  NSMinValueBinding: NSString; external name '_NSMinValueBinding';
  NSMinWidthBinding: NSString; external name '_NSMinWidthBinding';
  NSMixedStateImageBinding: NSString; external name '_NSMixedStateImageBinding';
  NSOffStateImageBinding: NSString; external name '_NSOffStateImageBinding';
  NSOnStateImageBinding: NSString; external name '_NSOnStateImageBinding';
  NSPredicateBinding: NSString; external name '_NSPredicateBinding';
  NSRecentSearchesBinding: NSString; external name '_NSRecentSearchesBinding';
  NSRepresentedFilenameBinding: NSString; external name '_NSRepresentedFilenameBinding';
  NSRowHeightBinding: NSString; external name '_NSRowHeightBinding';
  NSSelectedIdentifierBinding: NSString; external name '_NSSelectedIdentifierBinding';
  NSSelectedIndexBinding: NSString; external name '_NSSelectedIndexBinding';
  NSSelectedLabelBinding: NSString; external name '_NSSelectedLabelBinding';
  NSSelectedObjectBinding: NSString; external name '_NSSelectedObjectBinding';
  NSSelectedObjectsBinding: NSString; external name '_NSSelectedObjectsBinding';
  NSSelectedTagBinding: NSString; external name '_NSSelectedTagBinding';
  NSSelectedValueBinding: NSString; external name '_NSSelectedValueBinding';
  NSSelectedValuesBinding: NSString; external name '_NSSelectedValuesBinding';
  NSSelectionIndexesBinding: NSString; external name '_NSSelectionIndexesBinding';
  NSSelectionIndexPathsBinding: NSString; external name '_NSSelectionIndexPathsBinding';
  NSSortDescriptorsBinding: NSString; external name '_NSSortDescriptorsBinding';
  NSTargetBinding: NSString; external name '_NSTargetBinding';
  NSTextColorBinding: NSString; external name '_NSTextColorBinding';
  NSTitleBinding: NSString; external name '_NSTitleBinding';
  NSToolTipBinding: NSString; external name '_NSToolTipBinding';
  NSTransparentBinding: NSString; external name '_NSTransparentBinding';
  NSValueBinding: NSString; external name '_NSValueBinding';
  NSValuePathBinding: NSString; external name '_NSValuePathBinding';
  NSValueURLBinding: NSString; external name '_NSValueURLBinding';
  NSVisibleBinding: NSString; external name '_NSVisibleBinding';
  NSWarningValueBinding: NSString; external name '_NSWarningValueBinding';
  NSWidthBinding: NSString; external name '_NSWidthBinding';
  NSAllowsEditingMultipleValuesSelectionBindingOption: NSString; external name '_NSAllowsEditingMultipleValuesSelectionBindingOption';
  NSAllowsNullArgumentBindingOption: NSString; external name '_NSAllowsNullArgumentBindingOption';
  NSAlwaysPresentsApplicationModalAlertsBindingOption: NSString; external name '_NSAlwaysPresentsApplicationModalAlertsBindingOption';
  NSConditionallySetsEditableBindingOption: NSString; external name '_NSConditionallySetsEditableBindingOption';
  NSConditionallySetsEnabledBindingOption: NSString; external name '_NSConditionallySetsEnabledBindingOption';
  NSConditionallySetsHiddenBindingOption: NSString; external name '_NSConditionallySetsHiddenBindingOption';
  NSContinuouslyUpdatesValueBindingOption: NSString; external name '_NSContinuouslyUpdatesValueBindingOption';
  NSCreatesSortDescriptorBindingOption: NSString; external name '_NSCreatesSortDescriptorBindingOption';
  NSDeletesObjectsOnRemoveBindingsOption: NSString; external name '_NSDeletesObjectsOnRemoveBindingsOption';
  NSDisplayNameBindingOption: NSString; external name '_NSDisplayNameBindingOption';
  NSDisplayPatternBindingOption: NSString; external name '_NSDisplayPatternBindingOption';
  NSContentPlacementTagBindingOption: NSString; external name '_NSContentPlacementTagBindingOption';
  NSHandlesContentAsCompoundValueBindingOption: NSString; external name '_NSHandlesContentAsCompoundValueBindingOption';
  NSInsertsNullPlaceholderBindingOption: NSString; external name '_NSInsertsNullPlaceholderBindingOption';
  NSInvokesSeparatelyWithArrayObjectsBindingOption: NSString; external name '_NSInvokesSeparatelyWithArrayObjectsBindingOption';
  NSMultipleValuesPlaceholderBindingOption: NSString; external name '_NSMultipleValuesPlaceholderBindingOption';
  NSNoSelectionPlaceholderBindingOption: NSString; external name '_NSNoSelectionPlaceholderBindingOption';
  NSNotApplicablePlaceholderBindingOption: NSString; external name '_NSNotApplicablePlaceholderBindingOption';
  NSNullPlaceholderBindingOption: NSString; external name '_NSNullPlaceholderBindingOption';
  NSRaisesForNotApplicableKeysBindingOption: NSString; external name '_NSRaisesForNotApplicableKeysBindingOption';
  NSPredicateFormatBindingOption: NSString; external name '_NSPredicateFormatBindingOption';
  NSSelectorNameBindingOption: NSString; external name '_NSSelectorNameBindingOption';
  NSSelectsAllWhenSettingContentBindingOption: NSString; external name '_NSSelectsAllWhenSettingContentBindingOption';
  NSValidatesImmediatelyBindingOption: NSString; external name '_NSValidatesImmediatelyBindingOption';
  NSValueTransformerNameBindingOption: NSString; external name '_NSValueTransformerNameBindingOption';
  NSValueTransformerBindingOption: NSString; external name '_NSValueTransformerBindingOption';

{$endif}
{$endif}
