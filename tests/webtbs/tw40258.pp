{$mode objfpc} {$longstrings on}
{$define _1000th}

type
	INamedClient = interface procedure Listen; end;
	IStoredDispatcher = interface procedure Dispatch; end;
	IComplexThunk = interface procedure Perform; end;
	IMultithreadedStrategy = interface procedure Apply; end;
	IDummyBuilder = interface procedure Build; end;
	INamedAutoContext = interface procedure Switch; end;
	IMultithreadedFeedbackGlobalBuilder = interface procedure Build; end;
	IUniversalRecurringBuilder = interface procedure Build; end;
	IDummyDispatcher = interface procedure Dispatch; end;
	ISimpleAutoEncoder = interface procedure Encode; end;
	IPersistentVisitor = interface procedure Visit; end;
	IGlobalFeedbackRequest = interface procedure Send; end;
	IHeuristicVisitor = interface procedure Visit; end;
	IFastDecoder = interface procedure Decode; end;
	IRecurringToken = interface procedure Pass; end;
	IProxyPiecewiseSupervisor = interface procedure Cancel; end;
	IPiecewiseSingleThreadedSupervisor = interface procedure Cancel; end;
	INamedStrategy = interface procedure Apply; end;
	ILazyExecutor = interface procedure Execute; end;
	IExhaustiveEncoder = interface procedure Encode; end;
	IWraparoundLocalRequest = interface procedure Send; end;
	IComplexStoredBuilder = interface procedure Build; end;
	IProxyDecoder = interface procedure Decode; end;
	IPiecewiseComplexWrapper = interface procedure Wrap; end;
	ILocalStub = interface procedure Perform; end;
	IGlobalAutoComplexToken = interface procedure Pass; end;
	IPiecewiseExecutor = interface procedure Execute; end;
	ITransientEncoder = interface procedure Encode; end;
	IHeuristicBuilder = interface procedure Build; end;
	INativeToken = interface procedure Pass; end;
	IGlobalUniversalFactory = interface procedure Make; end;
	ISimplePiecewiseManager = interface procedure Get; end;
	ILocalFactory = interface procedure Make; end;
	ISimpleFactory = interface procedure Make; end;
	IGlobalManager = interface procedure Get; end;
	IPersistentCompoundBuilder = interface procedure Build; end;
	IGlobalPiecewiseChecker = interface procedure Check; end;
	INamedNativeTransientComparer = interface procedure Compare; end;
	INetworkMultithreadedDispatcher = interface procedure Dispatch; end;
	IDummyAdapter = interface procedure Call; end;
	ICompatibilityCallback = interface procedure Perform; end;
	IProxyClient = interface procedure Listen; end;
	ITransientToken = interface procedure Pass; end;
	IFeedbackExhaustiveVisitor = interface procedure Visit; end;
	IFastSupervisor = interface procedure Cancel; end;
	IMultithreadedVisitor = interface procedure Visit; end;
	IRecurringThunk = interface procedure Perform; end;
	IRecurringNativeMultithreadedStub = interface procedure Perform; end;
	IDelegatingComparer = interface procedure Compare; end;
	IExhaustiveLocalEncoder = interface procedure Encode; end;
	INativeStub = interface procedure Perform; end;
	IExhaustiveLocalDispatcher = interface procedure Dispatch; end;
	IFeedbackBuilder = interface procedure Build; end;
	IPiecewiseState = interface procedure Save; end;
	IHeuristicProxyServer = interface procedure Broadcast; end;
	IProxyNetworkHeuristicChecker = interface procedure Check; end;
	ITransientServer = interface procedure Broadcast; end;
	ISingleThreadedContext = interface procedure Switch; end;
	IPiecewisePersistentLazyDecoder = interface procedure Decode; end;
	IUniversalSimpleRequest = interface procedure Send; end;
	IWraparoundFactory = interface procedure Make; end;
	IStoredRequest = interface procedure Send; end;
	ILocalStoredBuilder = interface procedure Build; end;
	IComplexVisitor = interface procedure Visit; end;
	IUniversalState = interface procedure Save; end;
	IDelegatingFastFactory = interface procedure Make; end;
	IWraparoundRequest = interface procedure Send; end;
	ISimpleNativeWraparoundManager = interface procedure Get; end;
	IGlobalStub = interface procedure Perform; end;
	ISingleThreadedClient = interface procedure Listen; end;
	ILazyManager = interface procedure Get; end;
	IDummyState = interface procedure Save; end;
	ISimpleFastCompatibilityVisitor = interface procedure Visit; end;
	ICompoundDecoder = interface procedure Decode; end;
	IGlobalDummyRequest = interface procedure Send; end;
	IRecurringClient = interface procedure Listen; end;
	INamedServer = interface procedure Broadcast; end;
	ICompoundPiecewiseStrategy = interface procedure Apply; end;
	ITransientExecutor = interface procedure Execute; end;
	ICompatibilityPersistentAdapter = interface procedure Call; end;
	ISingleThreadedVisitor = interface procedure Visit; end;
	IWraparoundBuilder = interface procedure Build; end;
	ICompatibilityComparer = interface procedure Compare; end;
	INamedContext = interface procedure Switch; end;
	ICompatibilityDispatcher = interface procedure Dispatch; end;
	IAutoChecker = interface procedure Check; end;
	IComplexNamedManager = interface procedure Get; end;
	ITransientExhaustiveSupervisor = interface procedure Cancel; end;
	IUniversalWrapper = interface procedure Wrap; end;
	IMultithreadedBuilder = interface procedure Build; end;
	IHeuristicCompoundFastDispatcher = interface procedure Dispatch; end;
	IComplexClient = interface procedure Listen; end;
	IFastCallback = interface procedure Perform; end;
	IUniversalNamedCallback = interface procedure Perform; end;
	IExhaustiveStoredWraparoundExecutor = interface procedure Execute; end;
	IGlobalThunk = interface procedure Perform; end;
	IAutoExecutor = interface procedure Execute; end;
	ITransientClient = interface procedure Listen; end;
	IFeedbackPiecewiseWrapper = interface procedure Wrap; end;
	IAutoStrategy = interface procedure Apply; end;
	ILazyFastContext = interface procedure Switch; end;
	IFastCompatibilityManager = interface procedure Get; end;
	INetworkAutoMultithreadedStub = interface procedure Perform; end;
	ITransientManager = interface procedure Get; end;
	IMultithreadedFeedbackProxyExecutor = interface procedure Execute; end;
	IDelegatingRequest = interface procedure Send; end;
	INamedState = interface procedure Save; end;
	IWraparoundChecker = interface procedure Check; end;
	IDelegatingServer = interface procedure Broadcast; end;
	ICompatibilityExhaustiveToken = interface procedure Pass; end;
	IDelegatingUniversalServer = interface procedure Broadcast; end;
	IHeuristicComparer = interface procedure Compare; end;
	ICompoundNamedWraparoundManager = interface procedure Get; end;
	IExhaustiveUniversalLazyVisitor = interface procedure Visit; end;
	ILazyComparer = interface procedure Compare; end;
	ITransientDummyClient = interface procedure Listen; end;
	IRecurringGlobalAdapter = interface procedure Call; end;
	IGlobalDispatcher = interface procedure Dispatch; end;
	INativeState = interface procedure Save; end;
	IStoredBuilder = interface procedure Build; end;
	IPiecewiseMultithreadedExhaustiveExecutor = interface procedure Execute; end;
	IPiecewiseServer = interface procedure Broadcast; end;
	INativeAdapter = interface procedure Call; end;
	IComplexChecker = interface procedure Check; end;
	IMultithreadedLazyToken = interface procedure Pass; end;
	IFeedbackDispatcher = interface procedure Dispatch; end;
	IAutoFastSupervisor = interface procedure Cancel; end;
	IExhaustiveRecurringFastManager = interface procedure Get; end;
	IFeedbackTransientAutoComparer = interface procedure Compare; end;
	IFeedbackSimpleLazyToken = interface procedure Pass; end;
	IProxyExhaustiveGlobalExecutor = interface procedure Execute; end;
	IExhaustiveDispatcher = interface procedure Dispatch; end;
	ILazyDelegatingAdapter = interface procedure Call; end;
	IPiecewiseComparer = interface procedure Compare; end;
	IComplexDecoder = interface procedure Decode; end;
	ILazyGlobalComparer = interface procedure Compare; end;
	INetworkAdapter = interface procedure Call; end;
	INetworkDelegatingCallback = interface procedure Perform; end;
	IExhaustiveNetworkDispatcher = interface procedure Dispatch; end;
	IComplexNamedFactory = interface procedure Make; end;
	IFastDispatcher = interface procedure Dispatch; end;
	IStoredThunk = interface procedure Perform; end;
	ICompoundManager = interface procedure Get; end;
	ICompatibilityEncoder = interface procedure Encode; end;
	IUniversalLocalDispatcher = interface procedure Dispatch; end;
	IMultithreadedWrapper = interface procedure Wrap; end;
	ITransientUniversalChecker = interface procedure Check; end;
	IPiecewiseAdapter = interface procedure Call; end;
	INamedLazyManager = interface procedure Get; end;
	IRecurringRequest = interface procedure Send; end;
	IComplexUniversalVisitor = interface procedure Visit; end;
	IMultithreadedCompatibilityPiecewiseBuilder = interface procedure Build; end;
	IWraparoundWrapper = interface procedure Wrap; end;
	ILocalExecutor = interface procedure Execute; end;
	IProxyFactory = interface procedure Make; end;
	ISimpleToken = interface procedure Pass; end;
	IGlobalComplexState = interface procedure Save; end;
	IStoredCompatibilityWrapper = interface procedure Wrap; end;
	IGlobalMultithreadedAdapter = interface procedure Call; end;
	IHeuristicNativeLocalThunk = interface procedure Perform; end;
	INativeLocalFactory = interface procedure Make; end;
	IProxyExhaustiveLazyDispatcher = interface procedure Dispatch; end;
	ISimpleLocalBuilder = interface procedure Build; end;
	IComplexFastCallback = interface procedure Perform; end;
	IHeuristicDecoder = interface procedure Decode; end;
	IStoredHeuristicAutoWrapper = interface procedure Wrap; end;
	IMultithreadedRecurringWrapper = interface procedure Wrap; end;
	IDummyStrategy = interface procedure Apply; end;
	IDummyNetworkNativeCallback = interface procedure Perform; end;
	ITransientDecoder = interface procedure Decode; end;
	IWraparoundManager = interface procedure Get; end;
	IPersistentAdapter = interface procedure Call; end;
	IPersistentToken = interface procedure Pass; end;
	ILocalRequest = interface procedure Send; end;
	ISimpleNetworkEncoder = interface procedure Encode; end;
	ILazyToken = interface procedure Pass; end;
	INetworkComplexWrapper = interface procedure Wrap; end;
	IUniversalLocalThunk = interface procedure Perform; end;
	IFastAdapter = interface procedure Call; end;
	IExhaustiveToken = interface procedure Pass; end;
	IGlobalHeuristicComparer = interface procedure Compare; end;
	IFeedbackDecoder = interface procedure Decode; end;
	ISimpleCompoundStub = interface procedure Perform; end;
	ITransientCallback = interface procedure Perform; end;
	IAutoAdapter = interface procedure Call; end;
	IWraparoundMultithreadedCompoundStrategy = interface procedure Apply; end;
	IRecurringStub = interface procedure Perform; end;
	IFeedbackHeuristicChecker = interface procedure Check; end;
	IPiecewiseHeuristicServer = interface procedure Broadcast; end;
	ILocalLazyServer = interface procedure Broadcast; end;
	IPersistentSimpleProxyStrategy = interface procedure Apply; end;
	IExhaustiveThunk = interface procedure Perform; end;
	ICompatibilityAutoStrategy = interface procedure Apply; end;
	ICompatibilityChecker = interface procedure Check; end;
	INamedCallback = interface procedure Perform; end;
	IDummyThunk = interface procedure Perform; end;
	ILocalNativeCallback = interface procedure Perform; end;
	IWraparoundAdapter = interface procedure Call; end;
	IExhaustiveChecker = interface procedure Check; end;
	IComplexStub = interface procedure Perform; end;
	IProxyPersistentManager = interface procedure Get; end;
	IUniversalDispatcher = interface procedure Dispatch; end;
	IPiecewiseCompatibilityThunk = interface procedure Perform; end;
	INamedMultithreadedSupervisor = interface procedure Cancel; end;
	IComplexContext = interface procedure Switch; end;
	IUniversalStrategy = interface procedure Apply; end;
	INativeWrapper = interface procedure Wrap; end;
	IComplexAdapter = interface procedure Call; end;
	IPersistentWrapper = interface procedure Wrap; end;
	ISingleThreadedGlobalNativeServer = interface procedure Broadcast; end;
	ITransientRequest = interface procedure Send; end;
	IUniversalBuilder = interface procedure Build; end;
	IDelegatingAdapter = interface procedure Call; end;
	IWraparoundPersistentComplexState = interface procedure Save; end;
	IFeedbackAutoSingleThreadedContext = interface procedure Switch; end;
	ISimpleRecurringCallback = interface procedure Perform; end;
	ISingleThreadedSimpleManager = interface procedure Get; end;
	IUniversalDecoder = interface procedure Decode; end;
	IWraparoundNetworkDecoder = interface procedure Decode; end;
	ICompoundClient = interface procedure Listen; end;
	ICompatibilityServer = interface procedure Broadcast; end;
	IUniversalFactory = interface procedure Make; end;
	INetworkNativeToken = interface procedure Pass; end;
	ISimpleRequest = interface procedure Send; end;
	IGlobalSingleThreadedDispatcher = interface procedure Dispatch; end;
	IDummyExhaustivePersistentBuilder = interface procedure Build; end;
	ITransientNamedState = interface procedure Save; end;
	ISingleThreadedStrategy = interface procedure Apply; end;
	ITransientState = interface procedure Save; end;
	IComplexNamedRequest = interface procedure Send; end;
	IRecurringNativeStub = interface procedure Perform; end;
	IComplexPiecewiseSupervisor = interface procedure Cancel; end;
	IGlobalContext = interface procedure Switch; end;
	INativeProxyChecker = interface procedure Check; end;
	IComplexToken = interface procedure Pass; end;
	IRecurringExecutor = interface procedure Execute; end;
	INetworkComparer = interface procedure Compare; end;
	INamedWrapper = interface procedure Wrap; end;
	IRecurringVisitor = interface procedure Visit; end;
	IMultithreadedLocalRequest = interface procedure Send; end;
	IHeuristicState = interface procedure Save; end;
	IPersistentSimpleStrategy = interface procedure Apply; end;
	IPiecewiseNamedSingleThreadedServer = interface procedure Broadcast; end;
	IStoredContext = interface procedure Switch; end;
	IAutoExhaustiveContext = interface procedure Switch; end;
	INamedFeedbackExecutor = interface procedure Execute; end;
	IPersistentCallback = interface procedure Perform; end;
	ILazyMultithreadedServer = interface procedure Broadcast; end;
	IAutoContext = interface procedure Switch; end;
	ICompoundSingleThreadedNamedRequest = interface procedure Send; end;
	IExhaustiveCompatibilityThunk = interface procedure Perform; end;
	IFeedbackContext = interface procedure Switch; end;
	INetworkClient = interface procedure Listen; end;
	INativeLazyToken = interface procedure Pass; end;
	ISimpleStoredDecoder = interface procedure Decode; end;
	IUniversalCompoundSupervisor = interface procedure Cancel; end;
	IRecurringPersistentWraparoundExecutor = interface procedure Execute; end;
	IUniversalSupervisor = interface procedure Cancel; end;
	ITransientRecurringUniversalBuilder = interface procedure Build; end;
	ILocalStoredNetworkStrategy = interface procedure Apply; end;
	IPiecewiseRecurringComparer = interface procedure Compare; end;
	IPersistentBuilder = interface procedure Build; end;
	ICompatibilityNativeVisitor = interface procedure Visit; end;
	IPiecewiseGlobalVisitor = interface procedure Visit; end;
	IGlobalToken = interface procedure Pass; end;
	IProxyServer = interface procedure Broadcast; end;
	ILazyAdapter = interface procedure Call; end;
	IHeuristicWraparoundPiecewiseComparer = interface procedure Compare; end;
	IWraparoundMultithreadedNetworkClient = interface procedure Listen; end;
	IPersistentNamedExecutor = interface procedure Execute; end;
	IDummyFastComplexDispatcher = interface procedure Dispatch; end;
	IDelegatingRecurringServer = interface procedure Broadcast; end;
	IDelegatingContext = interface procedure Switch; end;
	IRecurringTransientLocalVisitor = interface procedure Visit; end;
	IRecurringFastSupervisor = interface procedure Cancel; end;
	INativeSupervisor = interface procedure Cancel; end;
	IRecurringNetworkClient = interface procedure Listen; end;
	IDelegatingStub = interface procedure Perform; end;
	IDelegatingStoredRequest = interface procedure Send; end;
	IHeuristicChecker = interface procedure Check; end;
	ICompoundSingleThreadedManager = interface procedure Get; end;
	IFeedbackRequest = interface procedure Send; end;
	INativeWraparoundRequest = interface procedure Send; end;
	ILazyStoredDecoder = interface procedure Decode; end;
	IWraparoundFeedbackNetworkStrategy = interface procedure Apply; end;
	IComplexCompatibilityCallback = interface procedure Perform; end;
	IComplexCompoundDecoder = interface procedure Decode; end;
	ILocalAdapter = interface procedure Call; end;
	IGlobalVisitor = interface procedure Visit; end;
	INamedGlobalThunk = interface procedure Perform; end;
	IStoredCompatibilityBuilder = interface procedure Build; end;
	ISimpleExhaustiveTransientComparer = interface procedure Compare; end;
	IAutoLocalTransientAdapter = interface procedure Call; end;
	IDelegatingTransientThunk = interface procedure Perform; end;
	IFeedbackCallback = interface procedure Perform; end;
	INativeCompoundToken = interface procedure Pass; end;
	IRecurringAdapter = interface procedure Call; end;
	IExhaustiveVisitor = interface procedure Visit; end;
	IStoredStub = interface procedure Perform; end;
	ICompoundChecker = interface procedure Check; end;
	IPersistentGlobalServer = interface procedure Broadcast; end;
	IFastToken = interface procedure Pass; end;
	INamedUniversalState = interface procedure Save; end;
	ICompatibilityDelegatingServer = interface procedure Broadcast; end;
	IFeedbackGlobalDecoder = interface procedure Decode; end;
	ILazyEncoder = interface procedure Encode; end;
	IPiecewiseThunk = interface procedure Perform; end;
	IWraparoundVisitor = interface procedure Visit; end;
	IAutoNetworkHeuristicStub = interface procedure Perform; end;
	IFeedbackState = interface procedure Save; end;
	IHeuristicToken = interface procedure Pass; end;
	ICompatibilityState = interface procedure Save; end;
	ICompatibilityAdapter = interface procedure Call; end;
	IPiecewiseSingleThreadedExecutor = interface procedure Execute; end;
	IHeuristicRequest = interface procedure Send; end;
	ISimpleBuilder = interface procedure Build; end;
	IComplexExecutor = interface procedure Execute; end;
	IAutoCompatibilityEncoder = interface procedure Encode; end;
	IMultithreadedToken = interface procedure Pass; end;
	ICompatibilitySimpleNamedWrapper = interface procedure Wrap; end;
	ITransientComplexClient = interface procedure Listen; end;
	ILocalStoredContext = interface procedure Switch; end;
	IGlobalDelegatingComparer = interface procedure Compare; end;
	IAutoUniversalFactory = interface procedure Make; end;
	IMultithreadedAdapter = interface procedure Call; end;
	ISimpleClient = interface procedure Listen; end;
	IGlobalNamedToken = interface procedure Pass; end;
	IProxyPersistentEncoder = interface procedure Encode; end;
	INetworkContext = interface procedure Switch; end;
	INativeThunk = interface procedure Perform; end;
	IProxyChecker = interface procedure Check; end;
	ICompoundDelegatingAdapter = interface procedure Call; end;
	IMultithreadedNativeWrapper = interface procedure Wrap; end;
	IHeuristicManager = interface procedure Get; end;
	IMultithreadedDummyFactory = interface procedure Make; end;
	ICompoundPersistentComparer = interface procedure Compare; end;
	IDummyComparer = interface procedure Compare; end;
	INamedSingleThreadedRequest = interface procedure Send; end;
	ILocalToken = interface procedure Pass; end;
	INamedPiecewiseWrapper = interface procedure Wrap; end;
	IStoredDecoder = interface procedure Decode; end;
	IGlobalExecutor = interface procedure Execute; end;
	ISingleThreadedExhaustiveState = interface procedure Save; end;
	IHeuristicExecutor = interface procedure Execute; end;
	IFeedbackComparer = interface procedure Compare; end;
	ISimpleProxyState = interface procedure Save; end;
	IExhaustiveStub = interface procedure Perform; end;
	ISingleThreadedFastEncoder = interface procedure Encode; end;
	ILocalChecker = interface procedure Check; end;
	IFeedbackSupervisor = interface procedure Cancel; end;
	ICompatibilityPiecewiseMultithreadedSupervisor = interface procedure Cancel; end;
	INativeLazyStrategy = interface procedure Apply; end;
	IFastWraparoundStub = interface procedure Perform; end;
	IStoredSimpleSupervisor = interface procedure Cancel; end;
	IUniversalRecurringDispatcher = interface procedure Dispatch; end;
	ICompatibilityContext = interface procedure Switch; end;
	IComplexFactory = interface procedure Make; end;
	ILazyDispatcher = interface procedure Dispatch; end;
	INamedNativeVisitor = interface procedure Visit; end;
	ILocalNativeRequest = interface procedure Send; end;
	IComplexNamedWrapper = interface procedure Wrap; end;
	IAutoGlobalExhaustiveStrategy = interface procedure Apply; end;
	IAutoRecurringChecker = interface procedure Check; end;
	IAutoNetworkManager = interface procedure Get; end;
	ILazyComplexPiecewiseBuilder = interface procedure Build; end;
	IHeuristicUniversalLocalDecoder = interface procedure Decode; end;
	INetworkAutoComplexDispatcher = interface procedure Dispatch; end;
	IGlobalRequest = interface procedure Send; end;
	ILocalAutoBuilder = interface procedure Build; end;
	IProxyDispatcher = interface procedure Dispatch; end;
	IRecurringChecker = interface procedure Check; end;
	IGlobalDecoder = interface procedure Decode; end;
	IFeedbackPersistentLocalFactory = interface procedure Make; end;
	IExhaustiveStrategy = interface procedure Apply; end;
	IStoredNativeSimpleServer = interface procedure Broadcast; end;
	ITransientChecker = interface procedure Check; end;
	IHeuristicSimpleState = interface procedure Save; end;
	ITransientSimpleStrategy = interface procedure Apply; end;
	IUniversalManager = interface procedure Get; end;
	IPersistentStrategy = interface procedure Apply; end;
	INetworkDelegatingEncoder = interface procedure Encode; end;
	IHeuristicStrategy = interface procedure Apply; end;
	IWraparoundServer = interface procedure Broadcast; end;
	IDelegatingEncoder = interface procedure Encode; end;
	INetworkStub = interface procedure Perform; end;
	IUniversalWraparoundStrategy = interface procedure Apply; end;
	INativeChecker = interface procedure Check; end;
	IExhaustiveServer = interface procedure Broadcast; end;
	INativeVisitor = interface procedure Visit; end;
	ICompoundExecutor = interface procedure Execute; end;
	ISingleThreadedNativeState = interface procedure Save; end;
	IDelegatingWrapper = interface procedure Wrap; end;
	IRecurringFeedbackContext = interface procedure Switch; end;
	IUniversalToken = interface procedure Pass; end;
	ICompatibilityDecoder = interface procedure Decode; end;
	IComplexEncoder = interface procedure Encode; end;
	IExhaustivePiecewiseAdapter = interface procedure Call; end;
	INamedUniversalDummyContext = interface procedure Switch; end;
	IStoredManager = interface procedure Get; end;
	IFeedbackLazyEncoder = interface procedure Encode; end;
	IAutoRecurringAdapter = interface procedure Call; end;
	ITransientCompatibilityEncoder = interface procedure Encode; end;
	IGlobalMultithreadedDispatcher = interface procedure Dispatch; end;
	IComplexNativeManager = interface procedure Get; end;
	IAutoCompatibilityClient = interface procedure Listen; end;
	IRecurringNativeDispatcher = interface procedure Dispatch; end;
	ILazyCallback = interface procedure Perform; end;
	ILocalStrategy = interface procedure Apply; end;
	IUniversalStub = interface procedure Perform; end;
	IPiecewiseClient = interface procedure Listen; end;
	ISingleThreadedDispatcher = interface procedure Dispatch; end;
	IProxyStoredSingleThreadedContext = interface procedure Switch; end;
	INativeContext = interface procedure Switch; end;
	IComplexTransientExecutor = interface procedure Execute; end;
	ISingleThreadedManager = interface procedure Get; end;
	IMultithreadedRecurringProxyContext = interface procedure Switch; end;
	ICompoundPiecewiseGlobalAdapter = interface procedure Call; end;
	ILocalBuilder = interface procedure Build; end;
	IMultithreadedRequest = interface procedure Send; end;
	ISimpleDummyManager = interface procedure Get; end;
	ISimpleContext = interface procedure Switch; end;
	IFeedbackClient = interface procedure Listen; end;
	ILocalNamedManager = interface procedure Get; end;
	ILazyState = interface procedure Save; end;
	ICompatibilityStrategy = interface procedure Apply; end;
	ITransientCompoundSimpleComparer = interface procedure Compare; end;
	IAutoNativeNamedAdapter = interface procedure Call; end;
	IProxyPiecewiseCallback = interface procedure Perform; end;
	IRecurringNetworkDelegatingVisitor = interface procedure Visit; end;
	IFeedbackAdapter = interface procedure Call; end;
	ILocalProxyThunk = interface procedure Perform; end;
	IFastProxyToken = interface procedure Pass; end;
	IPersistentExecutor = interface procedure Execute; end;
	IFastRecurringChecker = interface procedure Check; end;
	IPiecewiseExhaustiveEncoder = interface procedure Encode; end;
	ILazyUniversalStub = interface procedure Perform; end;
	IHeuristicAdapter = interface procedure Call; end;
	INamedComparer = interface procedure Compare; end;
	IPersistentState = interface procedure Save; end;
	ISimpleAutoWrapper = interface procedure Wrap; end;
	IStoredComparer = interface procedure Compare; end;
	IGlobalComparer = interface procedure Compare; end;
	IDummyClient = interface procedure Listen; end;
	IPersistentProxyStub = interface procedure Perform; end;
	ISimpleState = interface procedure Save; end;
	ILazyThunk = interface procedure Perform; end;
	IPiecewiseRecurringCallback = interface procedure Perform; end;
	IMultithreadedSupervisor = interface procedure Cancel; end;
	IRecurringNativeDecoder = interface procedure Decode; end;
	IFastClient = interface procedure Listen; end;
	ILocalNamedSupervisor = interface procedure Cancel; end;
	IPiecewiseWraparoundGlobalToken = interface procedure Pass; end;
	IHeuristicPiecewiseDispatcher = interface procedure Dispatch; end;
	ITransientWraparoundToken = interface procedure Pass; end;
	IProxyMultithreadedCallback = interface procedure Perform; end;
	IPersistentSimpleAutoToken = interface procedure Pass; end;
	IExhaustiveSupervisor = interface procedure Cancel; end;
	IFeedbackDelegatingStrategy = interface procedure Apply; end;
	ICompatibilitySimpleBuilder = interface procedure Build; end;
	IMultithreadedState = interface procedure Save; end;
	INetworkState = interface procedure Save; end;
	IDummyDecoder = interface procedure Decode; end;
	IRecurringCallback = interface procedure Perform; end;
	IDummyLocalContext = interface procedure Switch; end;
	IComplexRecurringToken = interface procedure Pass; end;
	INetworkUniversalRecurringStub = interface procedure Perform; end;
	IFeedbackAutoStub = interface procedure Perform; end;
	ICompoundDelegatingVisitor = interface procedure Visit; end;
	IMultithreadedExhaustiveWraparoundVisitor = interface procedure Visit; end;
	INamedToken = interface procedure Pass; end;
	ICompoundRequest = interface procedure Send; end;
	IProxyManager = interface procedure Get; end;
	INativeProxyClient = interface procedure Listen; end;
	IAutoCompoundWrapper = interface procedure Wrap; end;
	ITransientRecurringSimpleStub = interface procedure Perform; end;
	ICompoundToken = interface procedure Pass; end;
	ILazyServer = interface procedure Broadcast; end;
	IWraparoundComparer = interface procedure Compare; end;
	IExhaustiveBuilder = interface procedure Build; end;
	IMultithreadedComparer = interface procedure Compare; end;
	IFastBuilder = interface procedure Build; end;
	IStoredUniversalAdapter = interface procedure Call; end;
	ISingleThreadedStub = interface procedure Perform; end;
	ISingleThreadedFastRecurringRequest = interface procedure Send; end;
	IUniversalAutoLazyVisitor = interface procedure Visit; end;
	INamedFastServer = interface procedure Broadcast; end;
	IPersistentServer = interface procedure Broadcast; end;
	INetworkPersistentThunk = interface procedure Perform; end;
	ICompatibilitySingleThreadedThunk = interface procedure Perform; end;
	INamedStub = interface procedure Perform; end;
	ILocalRecurringAutoComparer = interface procedure Compare; end;
	IFastSimplePersistentEncoder = interface procedure Encode; end;
	IWraparoundComplexClient = interface procedure Listen; end;
	IAutoSupervisor = interface procedure Cancel; end;
	ICompoundFeedbackCompatibilityStub = interface procedure Perform; end;
	INamedFactory = interface procedure Make; end;
	IFastDummyExecutor = interface procedure Execute; end;
	ISimpleExecutor = interface procedure Execute; end;
	IStoredToken = interface procedure Pass; end;
	IDelegatingVisitor = interface procedure Visit; end;
	IDelegatingLazyPersistentWrapper = interface procedure Wrap; end;
	INetworkNativeCallback = interface procedure Perform; end;
	ISimpleWrapper = interface procedure Wrap; end;
	ITransientSupervisor = interface procedure Cancel; end;
	IRecurringServer = interface procedure Broadcast; end;
	IPersistentRequest = interface procedure Send; end;
	INetworkWraparoundChecker = interface procedure Check; end;
	ISingleThreadedComparer = interface procedure Compare; end;
	ISimpleHeuristicExecutor = interface procedure Execute; end;
	IFastFeedbackRequest = interface procedure Send; end;
	IComplexComparer = interface procedure Compare; end;
	IExhaustiveNamedUniversalComparer = interface procedure Compare; end;
	IProxyState = interface procedure Save; end;
	IProxyExhaustiveContext = interface procedure Switch; end;
	IAutoClient = interface procedure Listen; end;
	IGlobalNamedFactory = interface procedure Make; end;
	ILocalStoredChecker = interface procedure Check; end;
	ISimpleThunk = interface procedure Perform; end;
	IDummyServer = interface procedure Broadcast; end;
	ISimpleChecker = interface procedure Check; end;
	IUniversalNetworkDispatcher = interface procedure Dispatch; end;
	IExhaustiveMultithreadedUniversalState = interface procedure Save; end;
	ISingleThreadedSimpleSupervisor = interface procedure Cancel; end;
	IDelegatingSupervisor = interface procedure Cancel; end;
	IFastEncoder = interface procedure Encode; end;
	IProxyCompoundState = interface procedure Save; end;
	IStoredFastFactory = interface procedure Make; end;
	IWraparoundFeedbackState = interface procedure Save; end;
	ITransientAdapter = interface procedure Call; end;
	IRecurringMultithreadedProxyChecker = interface procedure Check; end;
	IRecurringCompoundThunk = interface procedure Perform; end;
	IDummyWrapper = interface procedure Wrap; end;
	IProxyCompoundAdapter = interface procedure Call; end;
	ISimpleCompatibilityStrategy = interface procedure Apply; end;
	IHeuristicContext = interface procedure Switch; end;
	IPersistentDelegatingSupervisor = interface procedure Cancel; end;
	IHeuristicFeedbackStub = interface procedure Perform; end;
	IPiecewiseSimpleComparer = interface procedure Compare; end;
	ISimplePersistentStub = interface procedure Perform; end;
	IProxyNetworkDispatcher = interface procedure Dispatch; end;
	ILazyDelegatingDispatcher = interface procedure Dispatch; end;
	ISimplePiecewiseState = interface procedure Save; end;
	IStoredMultithreadedContext = interface procedure Switch; end;
	ICompoundComparer = interface procedure Compare; end;
	IComplexDispatcher = interface procedure Dispatch; end;
	ILazyNamedCallback = interface procedure Perform; end;
	IAutoManager = interface procedure Get; end;
	ILocalProxyChecker = interface procedure Check; end;
	ILocalVisitor = interface procedure Visit; end;
	INamedThunk = interface procedure Perform; end;
	IFastRequest = interface procedure Send; end;
	IExhaustiveWrapper = interface procedure Wrap; end;
	IGlobalPersistentExecutor = interface procedure Execute; end;
	IHeuristicCompatibilityRequest = interface procedure Send; end;
	IDelegatingDispatcher = interface procedure Dispatch; end;
	IPiecewiseDispatcher = interface procedure Dispatch; end;
	IProxyExecutor = interface procedure Execute; end;
	ICompoundTransientChecker = interface procedure Check; end;
	ISingleThreadedNamedEncoder = interface procedure Encode; end;
	IHeuristicDispatcher = interface procedure Dispatch; end;
	IRecurringDispatcher = interface procedure Dispatch; end;
	IDelegatingFastServer = interface procedure Broadcast; end;
	IDelegatingCallback = interface procedure Perform; end;
	IDelegatingCompatibilityDecoder = interface procedure Decode; end;
	ISimpleManager = interface procedure Get; end;
	ITransientStrategy = interface procedure Apply; end;
	IPiecewiseStrategy = interface procedure Apply; end;
	IDummyVisitor = interface procedure Visit; end;
	IStoredChecker = interface procedure Check; end;
	IGlobalState = interface procedure Save; end;
	IFeedbackFastContext = interface procedure Switch; end;
	ILazyAutoGlobalThunk = interface procedure Perform; end;
	ISingleThreadedDummyFastCallback = interface procedure Perform; end;
	IMultithreadedSimpleExecutor = interface procedure Execute; end;
	ICompoundEncoder = interface procedure Encode; end;
	IProxyAutoState = interface procedure Save; end;
	IComplexFeedbackExecutor = interface procedure Execute; end;
	INativeAutoNetworkCallback = interface procedure Perform; end;
	IMultithreadedNamedExecutor = interface procedure Execute; end;
	INamedChecker = interface procedure Check; end;
	IRecurringFastManager = interface procedure Get; end;
	ILocalTransientUniversalThunk = interface procedure Perform; end;
	IGlobalChecker = interface procedure Check; end;
	IPersistentPiecewiseToken = interface procedure Pass; end;
	IDelegatingState = interface procedure Save; end;
	IWraparoundFastCompoundServer = interface procedure Broadcast; end;
	IDummyChecker = interface procedure Check; end;
	ISingleThreadedWrapper = interface procedure Wrap; end;
	IGlobalSupervisor = interface procedure Cancel; end;
	IDelegatingThunk = interface procedure Perform; end;
	INetworkComplexSupervisor = interface procedure Cancel; end;
	IPersistentThunk = interface procedure Perform; end;
	INamedMultithreadedDecoder = interface procedure Decode; end;
	INamedRequest = interface procedure Send; end;
	ICompatibilityComplexFastRequest = interface procedure Send; end;
	IPiecewiseToken = interface procedure Pass; end;
	ITransientDelegatingFactory = interface procedure Make; end;
	ISimpleStrategy = interface procedure Apply; end;
	IDelegatingAutoStub = interface procedure Perform; end;
	IWraparoundStrategy = interface procedure Apply; end;
	ICompoundDispatcher = interface procedure Dispatch; end;
	ISingleThreadedChecker = interface procedure Check; end;
	IMultithreadedRecurringNamedAdapter = interface procedure Call; end;
	IAutoDecoder = interface procedure Decode; end;
	IRecurringDummyState = interface procedure Save; end;
	ISingleThreadedToken = interface procedure Pass; end;
	ISingleThreadedExecutor = interface procedure Execute; end;
	IAutoWrapper = interface procedure Wrap; end;
	IDelegatingNamedBuilder = interface procedure Build; end;
	IDummyContext = interface procedure Switch; end;
	IGlobalWraparoundDecoder = interface procedure Decode; end;
	IGlobalRecurringStub = interface procedure Perform; end;
	IStoredNativeState = interface procedure Save; end;
	INetworkThunk = interface procedure Perform; end;
	IExhaustiveExecutor = interface procedure Execute; end;
	IExhaustiveFeedbackContext = interface procedure Switch; end;
	IProxyStoredCallback = interface procedure Perform; end;
	IRecurringManager = interface procedure Get; end;
	ICompoundStoredDispatcher = interface procedure Dispatch; end;
	ISingleThreadedAdapter = interface procedure Call; end;
	IProxyContext = interface procedure Switch; end;
	IWraparoundExhaustiveFactory = interface procedure Make; end;
	IComplexStoredWrapper = interface procedure Wrap; end;
	IMultithreadedClient = interface procedure Listen; end;
	INativeCompatibilityExecutor = interface procedure Execute; end;
	IProxyDelegatingRequest = interface procedure Send; end;
	IHeuristicPiecewiseState = interface procedure Save; end;
	IDelegatingDecoder = interface procedure Decode; end;
	IDummyTransientCallback = interface procedure Perform; end;
	IProxyMultithreadedFeedbackAdapter = interface procedure Call; end;
	IMultithreadedPiecewiseStrategy = interface procedure Apply; end;
	IWraparoundState = interface procedure Save; end;
	IExhaustivePiecewiseDispatcher = interface procedure Dispatch; end;
	ISingleThreadedCallback = interface procedure Perform; end;
	ISingleThreadedRequest = interface procedure Send; end;
	IProxyThunk = interface procedure Perform; end;
	ISimpleExhaustiveContext = interface procedure Switch; end;
	ICompoundServer = interface procedure Broadcast; end;
	ICompoundAdapter = interface procedure Call; end;
	IMultithreadedFactory = interface procedure Make; end;
	IStoredRecurringFactory = interface procedure Make; end;
	IUniversalDummyEncoder = interface procedure Encode; end;
	IFeedbackFactory = interface procedure Make; end;
	ISimpleLocalServer = interface procedure Broadcast; end;
	IAutoNamedSingleThreadedExecutor = interface procedure Execute; end;
	IDelegatingExhaustiveDecoder = interface procedure Decode; end;
	ICompatibilitySupervisor = interface procedure Cancel; end;
	INamedCompoundMultithreadedSupervisor = interface procedure Cancel; end;
	IUniversalServer = interface procedure Broadcast; end;
	ISingleThreadedNamedStrategy = interface procedure Apply; end;
	IMultithreadedServer = interface procedure Broadcast; end;
	IStoredWrapper = interface procedure Wrap; end;
	IFastCompatibilityStub = interface procedure Perform; end;
	IPiecewiseRequest = interface procedure Send; end;
	ISimpleDecoder = interface procedure Decode; end;
	ICompoundRecurringStrategy = interface procedure Apply; end;
	IRecurringBuilder = interface procedure Build; end;
	IDelegatingRecurringSingleThreadedVisitor = interface procedure Visit; end;
	IRecurringWrapper = interface procedure Wrap; end;
	IDummyRequest = interface procedure Send; end;
	IDelegatingManager = interface procedure Get; end;
	IWraparoundMultithreadedGlobalEncoder = interface procedure Encode; end;
	ISimplePersistentAdapter = interface procedure Call; end;
	INamedNetworkFactory = interface procedure Make; end;
	IStoredPiecewiseServer = interface procedure Broadcast; end;
	IComplexStoredStub = interface procedure Perform; end;
	INetworkProxyCompoundComparer = interface procedure Compare; end;
	ICompoundStub = interface procedure Perform; end;
	IExhaustiveUniversalVisitor = interface procedure Visit; end;
	IGlobalFastDecoder = interface procedure Decode; end;
	IMultithreadedEncoder = interface procedure Encode; end;
	IWraparoundStub = interface procedure Perform; end;
	INativeManager = interface procedure Get; end;
	INativeCompoundNetworkState = interface procedure Save; end;
	IHeuristicFeedbackProxyToken = interface procedure Pass; end;
	IFastNetworkClient = interface procedure Listen; end;
	IAutoRequest = interface procedure Send; end;
	IExhaustiveManager = interface procedure Get; end;
	IAutoVisitor = interface procedure Visit; end;
	ICompoundVisitor = interface procedure Visit; end;
	ISimpleTransientVisitor = interface procedure Visit; end;
	ISingleThreadedUniversalNetworkState = interface procedure Save; end;
	IProxyRequest = interface procedure Send; end;
	ICompatibilityThunk = interface procedure Perform; end;
	IFeedbackServer = interface procedure Broadcast; end;
	IWraparoundMultithreadedDispatcher = interface procedure Dispatch; end;
	IAutoNativeSupervisor = interface procedure Cancel; end;
	IGlobalBuilder = interface procedure Build; end;
	INetworkBuilder = interface procedure Build; end;
	IPiecewiseNetworkToken = interface procedure Pass; end;
	IGlobalStrategy = interface procedure Apply; end;
	IGlobalPersistentAdapter = interface procedure Call; end;
	IDummySupervisor = interface procedure Cancel; end;
	IFastServer = interface procedure Broadcast; end;
	IPiecewiseStoredNativeToken = interface procedure Pass; end;
	IExhaustiveSingleThreadedServer = interface procedure Broadcast; end;
	IProxyToken = interface procedure Pass; end;
	ILocalManager = interface procedure Get; end;
	IUniversalStoredChecker = interface procedure Check; end;
	INativeStrategy = interface procedure Apply; end;
	INamedCompatibilityComparer = interface procedure Compare; end;
	ILazyNativeThunk = interface procedure Perform; end;
	INamedEncoder = interface procedure Encode; end;
	IWraparoundComplexAutoClient = interface procedure Listen; end;
	ILocalSupervisor = interface procedure Cancel; end;
	IWraparoundExecutor = interface procedure Execute; end;
	IPiecewiseStub = interface procedure Perform; end;
	IComplexNamedContext = interface procedure Switch; end;
	ICompoundWrapper = interface procedure Wrap; end;
	IRecurringPiecewiseSupervisor = interface procedure Cancel; end;
	IDummyAutoDispatcher = interface procedure Dispatch; end;
	IPiecewiseLocalCallback = interface procedure Perform; end;
	ILazyRecurringEncoder = interface procedure Encode; end;
	IPersistentStub = interface procedure Perform; end;
	IComplexBuilder = interface procedure Build; end;
	IProxyTransientBuilder = interface procedure Build; end;
	IFastWrapper = interface procedure Wrap; end;
	ILazyStub = interface procedure Perform; end;
	INetworkChecker = interface procedure Check; end;
	IComplexWrapper = interface procedure Wrap; end;
	INamedWraparoundStoredEncoder = interface procedure Encode; end;
	IPiecewiseStoredNetworkEncoder = interface procedure Encode; end;
	IFastAutoStub = interface procedure Perform; end;
	IExhaustiveComparer = interface procedure Compare; end;
	ILocalThunk = interface procedure Perform; end;
	IPersistentManager = interface procedure Get; end;
	ISingleThreadedNativeFactory = interface procedure Make; end;
	IExhaustiveComplexFactory = interface procedure Make; end;
	IPiecewiseBuilder = interface procedure Build; end;
	INativeFactory = interface procedure Make; end;
	ILocalHeuristicToken = interface procedure Pass; end;
	IRecurringHeuristicSupervisor = interface procedure Cancel; end;
	IComplexManager = interface procedure Get; end;
	INamedDecoder = interface procedure Decode; end;
	IMultithreadedWraparoundClient = interface procedure Listen; end;
	IMultithreadedChecker = interface procedure Check; end;
	IDelegatingCompatibilityStrategy = interface procedure Apply; end;
	INetworkRequest = interface procedure Send; end;
	IHeuristicWrapper = interface procedure Wrap; end;
	ISimpleCompoundDispatcher = interface procedure Dispatch; end;
	IFastStrategy = interface procedure Apply; end;
	INetworkAutoDispatcher = interface procedure Dispatch; end;
	IRecurringFactory = interface procedure Make; end;
	IRecurringPiecewiseTransientChecker = interface procedure Check; end;
	ISingleThreadedState = interface procedure Save; end;
	INetworkFeedbackPiecewiseState = interface procedure Save; end;
	IFeedbackCompatibilityGlobalThunk = interface procedure Perform; end;
	ILocalWrapper = interface procedure Wrap; end;
	IProxyLazyHeuristicServer = interface procedure Broadcast; end;
	ISimpleExhaustiveComparer = interface procedure Compare; end;
	IMultithreadedExecutor = interface procedure Execute; end;
	ICompoundSingleThreadedServer = interface procedure Broadcast; end;
	IStoredLazyClient = interface procedure Listen; end;
	IWraparoundCompoundBuilder = interface procedure Build; end;
	ISingleThreadedProxyExecutor = interface procedure Execute; end;
	IMultithreadedExhaustiveCompoundDispatcher = interface procedure Dispatch; end;
	IMultithreadedManager = interface procedure Get; end;
	ISingleThreadedPiecewiseNativeEncoder = interface procedure Encode; end;
	IWraparoundDecoder = interface procedure Decode; end;
	IHeuristicCompatibilityContext = interface procedure Switch; end;
	IDummyGlobalCallback = interface procedure Perform; end;
	ISingleThreadedServer = interface procedure Broadcast; end;
	ILocalExhaustiveCompoundFactory = interface procedure Make; end;
	IPiecewiseStoredVisitor = interface procedure Visit; end;
	IComplexGlobalContext = interface procedure Switch; end;
	IWraparoundDelegatingLocalWrapper = interface procedure Wrap; end;
	IPiecewiseCallback = interface procedure Perform; end;
	IComplexSupervisor = interface procedure Cancel; end;
	IHeuristicProxyLazyWrapper = interface procedure Wrap; end;
	ICompatibilityTransientBuilder = interface procedure Build; end;
	INamedNativeCompatibilityDispatcher = interface procedure Dispatch; end;
	INamedAutoComparer = interface procedure Compare; end;
	ICompatibilityRequest = interface procedure Send; end;
	ICompatibilitySingleThreadedAdapter = interface procedure Call; end;
	INativeExecutor = interface procedure Execute; end;
	INativeAutoChecker = interface procedure Check; end;
	ICompoundStrategy = interface procedure Apply; end;
	IPiecewiseWraparoundLazyState = interface procedure Save; end;
	INetworkWrapper = interface procedure Wrap; end;
	IMultithreadedNamedContext = interface procedure Switch; end;
	IPersistentClient = interface procedure Listen; end;
	IPiecewiseCompoundAdapter = interface procedure Call; end;
	ICompatibilityWrapper = interface procedure Wrap; end;
	IFeedbackEncoder = interface procedure Encode; end;
	IHeuristicMultithreadedExecutor = interface procedure Execute; end;
	IRecurringNamedEncoder = interface procedure Encode; end;
	ICompatibilityExecutor = interface procedure Execute; end;
	ICompoundBuilder = interface procedure Build; end;
	IDelegatingFactory = interface procedure Make; end;
	ICompoundPersistentRequest = interface procedure Send; end;
	IUniversalAutoStub = interface procedure Perform; end;
	ITransientBuilder = interface procedure Build; end;
	IDelegatingRecurringDummySupervisor = interface procedure Cancel; end;
	INamedAdapter = interface procedure Call; end;
	ICompatibilityFactory = interface procedure Make; end;
	ILocalEncoder = interface procedure Encode; end;
	IUniversalTransientExecutor = interface procedure Execute; end;
	IHeuristicTransientContext = interface procedure Switch; end;
	ILazyExhaustiveUniversalAdapter = interface procedure Call; end;
	IRecurringExhaustiveExecutor = interface procedure Execute; end;
	INativeDummyThunk = interface procedure Perform; end;
	IPiecewiseNamedDecoder = interface procedure Decode; end;
	IComplexCallback = interface procedure Perform; end;
	ISimpleCallback = interface procedure Perform; end;
	IStoredExecutor = interface procedure Execute; end;
	ICompatibilityCompoundLocalComparer = interface procedure Compare; end;
	IDummyStub = interface procedure Perform; end;
	IProxyRecurringStrategy = interface procedure Apply; end;
	IDelegatingHeuristicContext = interface procedure Switch; end;
	IStoredEncoder = interface procedure Encode; end;
	IRecurringNamedWrapper = interface procedure Wrap; end;
	IWraparoundStoredUniversalWrapper = interface procedure Wrap; end;
	IDelegatingTransientWrapper = interface procedure Wrap; end;
	IStoredServer = interface procedure Broadcast; end;
	ILocalState = interface procedure Save; end;
	IPiecewiseVisitor = interface procedure Visit; end;
	IWraparoundPersistentEncoder = interface procedure Encode; end;
	ILazyComplexComparer = interface procedure Compare; end;
	IFastDelegatingBuilder = interface procedure Build; end;
	IUniversalNamedFactory = interface procedure Make; end;
	IComplexProxyFactory = interface procedure Make; end;
	IAutoFastFactory = interface procedure Make; end;
	IStoredCallback = interface procedure Perform; end;
	IFeedbackChecker = interface procedure Check; end;
	INativeStoredClient = interface procedure Listen; end;
	IComplexUniversalAdapter = interface procedure Call; end;
	ITransientDispatcher = interface procedure Dispatch; end;
	IProxyNamedComparer = interface procedure Compare; end;
	IPiecewiseLocalVisitor = interface procedure Visit; end;
	IComplexState = interface procedure Save; end;
	IDummyExhaustiveEncoder = interface procedure Encode; end;
	IMultithreadedStub = interface procedure Perform; end;
	IWraparoundMultithreadedToken = interface procedure Pass; end;
	IAutoNetworkCallback = interface procedure Perform; end;
	IUniversalLocalCallback = interface procedure Perform; end;
	IHeuristicTransientToken = interface procedure Pass; end;
	IUniversalGlobalWraparoundBuilder = interface procedure Build; end;
	IFeedbackSingleThreadedCallback = interface procedure Perform; end;
	IProxySimpleDummyExecutor = interface procedure Execute; end;
	ITransientNamedAdapter = interface procedure Call; end;
	INetworkSupervisor = interface procedure Cancel; end;
	IProxyComparer = interface procedure Compare; end;
	INativeBuilder = interface procedure Build; end;
	IStoredNamedFactory = interface procedure Make; end;
	IRecurringDummySimpleCallback = interface procedure Perform; end;
	IHeuristicRecurringComplexVisitor = interface procedure Visit; end;
	IHeuristicLazyDecoder = interface procedure Decode; end;
	IComplexRequest = interface procedure Send; end;
	IPersistentNamedChecker = interface procedure Check; end;
	IUniversalWraparoundStub = interface procedure Perform; end;
	IGlobalMultithreadedBuilder = interface procedure Build; end;
	IProxyEncoder = interface procedure Encode; end;
	IExhaustiveLazyContext = interface procedure Switch; end;
	IUniversalChecker = interface procedure Check; end;
	IComplexGlobalSupervisor = interface procedure Cancel; end;
	INetworkEncoder = interface procedure Encode; end;
	IUniversalCallback = interface procedure Perform; end;
	IGlobalTransientNetworkContext = interface procedure Switch; end;
	ISimpleLocalTransientChecker = interface procedure Check; end;
	INamedGlobalContext = interface procedure Switch; end;
	IDelegatingGlobalContext = interface procedure Switch; end;
	IPersistentEncoder = interface procedure Encode; end;
	ISingleThreadedDummySimpleCallback = interface procedure Perform; end;
	IWraparoundDispatcher = interface procedure Dispatch; end;
	ISimpleServer = interface procedure Broadcast; end;
	ICompoundThunk = interface procedure Perform; end;
	IWraparoundClient = interface procedure Listen; end;
	ILocalUniversalWraparoundVisitor = interface procedure Visit; end;
	IProxyCallback = interface procedure Perform; end;
	IMultithreadedDispatcher = interface procedure Dispatch; end;
	ILocalComparer = interface procedure Compare; end;
	INativeMultithreadedLocalSupervisor = interface procedure Cancel; end;
	IRecurringStrategy = interface procedure Apply; end;
	IMultithreadedGlobalWrapper = interface procedure Wrap; end;
	IFastSingleThreadedServer = interface procedure Broadcast; end;
	INetworkLocalRequest = interface procedure Send; end;
	IExhaustiveProxySupervisor = interface procedure Cancel; end;
	ICompoundPiecewiseNamedToken = interface procedure Pass; end;
	IUniversalVisitor = interface procedure Visit; end;
	ICompoundFastNetworkToken = interface procedure Pass; end;
	INetworkRecurringServer = interface procedure Broadcast; end;
	IExhaustiveDelegatingState = interface procedure Save; end;
	ISimplePersistentHeuristicThunk = interface procedure Perform; end;
	INamedNetworkStrategy = interface procedure Apply; end;
	ILazyVisitor = interface procedure Visit; end;
	IAutoHeuristicFastState = interface procedure Save; end;
	ICompoundPersistentFastDecoder = interface procedure Decode; end;
	IMultithreadedDelegatingFactory = interface procedure Make; end;
	INamedSupervisor = interface procedure Cancel; end;
	ICompatibilityVisitor = interface procedure Visit; end;
	IAutoLocalPiecewiseFactory = interface procedure Make; end;
	IStoredExhaustiveDummyServer = interface procedure Broadcast; end;
	ISimpleNetworkCompoundStub = interface procedure Perform; end;
	IGlobalServer = interface procedure Broadcast; end;
	ICompoundExhaustiveState = interface procedure Save; end;
	IDelegatingDummySingleThreadedCallback = interface procedure Perform; end;
	IPersistentExhaustiveFactory = interface procedure Make; end;
	IDelegatingComplexFactory = interface procedure Make; end;
	ISimpleProxyStoredChecker = interface procedure Check; end;
	IAutoExhaustiveComparer = interface procedure Compare; end;
	INamedBuilder = interface procedure Build; end;
	IDelegatingExhaustiveStrategy = interface procedure Apply; end;
	IComplexAutoPiecewiseCallback = interface procedure Perform; end;
	IHeuristicThunk = interface procedure Perform; end;
	IFastExecutor = interface procedure Execute; end;
	INamedManager = interface procedure Get; end;
	IFastManager = interface procedure Get; end;
	ICompoundCompatibilityExecutor = interface procedure Execute; end;
	IStoredCompoundClient = interface procedure Listen; end;
	INetworkWraparoundWrapper = interface procedure Wrap; end;
	IUniversalRecurringExecutor = interface procedure Execute; end;
	IMultithreadedAutoVisitor = interface procedure Visit; end;
	IPiecewiseProxyComparer = interface procedure Compare; end;
	IFeedbackPiecewiseRequest = interface procedure Send; end;
	IGlobalPersistentPiecewiseStub = interface procedure Perform; end;
	IDelegatingStoredExecutor = interface procedure Execute; end;
	IDelegatingWraparoundDecoder = interface procedure Decode; end;
	IRecurringSupervisor = interface procedure Cancel; end;
	ISimpleAutoStub = interface procedure Perform; end;
	IRecurringGlobalFactory = interface procedure Make; end;
	IAutoBuilder = interface procedure Build; end;
	IGlobalNetworkBuilder = interface procedure Build; end;
	ITransientStub = interface procedure Perform; end;
	IHeuristicServer = interface procedure Broadcast; end;
	IProxyNamedComplexSupervisor = interface procedure Cancel; end;
	IExhaustiveCompatibilityFactory = interface procedure Make; end;
	IStoredClient = interface procedure Listen; end;
	IUniversalProxyPersistentDispatcher = interface procedure Dispatch; end;
	ILocalUniversalDecoder = interface procedure Decode; end;
	ILazyFeedbackCallback = interface procedure Perform; end;
	IHeuristicPersistentStub = interface procedure Perform; end;
	IStoredSupervisor = interface procedure Cancel; end;
	IDummyCompatibilityPiecewiseDecoder = interface procedure Decode; end;
	IExhaustivePersistentStrategy = interface procedure Apply; end;
	IComplexRecurringLocalServer = interface procedure Broadcast; end;
	IExhaustiveLazySingleThreadedContext = interface procedure Switch; end;
	IGlobalHeuristicPersistentWrapper = interface procedure Wrap; end;
	IDelegatingMultithreadedDecoder = interface procedure Decode; end;
	IWraparoundEncoder = interface procedure Encode; end;
	IRecurringDecoder = interface procedure Decode; end;
	IUniversalAdapter = interface procedure Call; end;
	IPersistentContext = interface procedure Switch; end;
	IWraparoundCompoundRecurringVisitor = interface procedure Visit; end;
	IProxyLocalStub = interface procedure Perform; end;
	IFastStub = interface procedure Perform; end;
	INativeMultithreadedEncoder = interface procedure Encode; end;
	IAutoServer = interface procedure Broadcast; end;
	INamedAutoVisitor = interface procedure Visit; end;
	IWraparoundProxyDecoder = interface procedure Decode; end;
	ISingleThreadedEncoder = interface procedure Encode; end;
	IDummyLazyToken = interface procedure Pass; end;
	IFastProxyComparer = interface procedure Compare; end;
	IPersistentSingleThreadedExecutor = interface procedure Execute; end;
	IFeedbackThunk = interface procedure Perform; end;
	INativeClient = interface procedure Listen; end;
	IDelegatingComplexState = interface procedure Save; end;
	IMultithreadedNamedCompoundToken = interface procedure Pass; end;
	ICompatibilityPiecewiseContext = interface procedure Switch; end;
	IUniversalExecutor = interface procedure Execute; end;
	ITransientFactory = interface procedure Make; end;
	IPersistentGlobalToken = interface procedure Pass; end;
	ISimpleNamedThunk = interface procedure Perform; end;
	INamedProxyManager = interface procedure Get; end;
	IStoredNamedChecker = interface procedure Check; end;
	IDelegatingMultithreadedLazyVisitor = interface procedure Visit; end;
	INamedDelegatingNetworkSupervisor = interface procedure Cancel; end;
	IComplexStrategy = interface procedure Apply; end;
	IGlobalFeedbackWraparoundEncoder = interface procedure Encode; end;
	IWraparoundThunk = interface procedure Perform; end;
	IDelegatingComplexDispatcher = interface procedure Dispatch; end;
	ISimpleExhaustiveStub = interface procedure Perform; end;
	IDummyWraparoundContext = interface procedure Switch; end;
	IFastDelegatingCallback = interface procedure Perform; end;
	INetworkWraparoundNativeCallback = interface procedure Perform; end;
	INetworkServer = interface procedure Broadcast; end;
	ISingleThreadedThunk = interface procedure Perform; end;
	IWraparoundTransientEncoder = interface procedure Encode; end;
	IWraparoundLazyFactory = interface procedure Make; end;
	IWraparoundSingleThreadedDelegatingFactory = interface procedure Make; end;
	IUniversalNetworkEncoder = interface procedure Encode; end;
	ILocalMultithreadedState = interface procedure Save; end;
	IProxyWraparoundNamedServer = interface procedure Broadcast; end;
	IMultithreadedCallback = interface procedure Perform; end;
	ITransientWrapper = interface procedure Wrap; end;
	IStoredUniversalNetworkServer = interface procedure Broadcast; end;
	IWraparoundLazyStub = interface procedure Perform; end;
	IFeedbackWrapper = interface procedure Wrap; end;
	INativeRequest = interface procedure Send; end;
	IDelegatingProxyServer = interface procedure Broadcast; end;
	IStoredSimpleChecker = interface procedure Check; end;
	IExhaustiveUniversalDecoder = interface procedure Decode; end;
	IAutoComparer = interface procedure Compare; end;
	IHeuristicFactory = interface procedure Make; end;
	INativeDispatcher = interface procedure Dispatch; end;
	ICompoundFeedbackWrapper = interface procedure Wrap; end;
	IDelegatingNativeToken = interface procedure Pass; end;
	IDummyRecurringThunk = interface procedure Perform; end;
	IUniversalExhaustiveStrategy = interface procedure Apply; end;
	IAutoMultithreadedEncoder = interface procedure Encode; end;
{$ifdef _1000th}
	ITransientProxyChecker = interface procedure Check; end;
{$endif}

	God = class(TInterfacedObject,
		INamedClient, IStoredDispatcher, IComplexThunk, IMultithreadedStrategy, IDummyBuilder,
		INamedAutoContext, IMultithreadedFeedbackGlobalBuilder, IUniversalRecurringBuilder, IDummyDispatcher, ISimpleAutoEncoder,
		IPersistentVisitor, IGlobalFeedbackRequest, IHeuristicVisitor, IFastDecoder, IRecurringToken,
		IProxyPiecewiseSupervisor, IPiecewiseSingleThreadedSupervisor, INamedStrategy, ILazyExecutor, IExhaustiveEncoder,
		IWraparoundLocalRequest, IComplexStoredBuilder, IProxyDecoder, IPiecewiseComplexWrapper, ILocalStub,
		IGlobalAutoComplexToken, IPiecewiseExecutor, ITransientEncoder, IHeuristicBuilder, INativeToken,
		IGlobalUniversalFactory, ISimplePiecewiseManager, ILocalFactory, ISimpleFactory, IGlobalManager,
		IPersistentCompoundBuilder, IGlobalPiecewiseChecker, INamedNativeTransientComparer, INetworkMultithreadedDispatcher, IDummyAdapter,
		ICompatibilityCallback, IProxyClient, ITransientToken, IFeedbackExhaustiveVisitor, IFastSupervisor,
		IMultithreadedVisitor, IRecurringThunk, IRecurringNativeMultithreadedStub, IDelegatingComparer, IExhaustiveLocalEncoder,
		INativeStub, IExhaustiveLocalDispatcher, IFeedbackBuilder, IPiecewiseState, IHeuristicProxyServer,
		IProxyNetworkHeuristicChecker, ITransientServer, ISingleThreadedContext, IPiecewisePersistentLazyDecoder, IUniversalSimpleRequest,
		IWraparoundFactory, IStoredRequest, ILocalStoredBuilder, IComplexVisitor, IUniversalState,
		IDelegatingFastFactory, IWraparoundRequest, ISimpleNativeWraparoundManager, IGlobalStub, ISingleThreadedClient,
		ILazyManager, IDummyState, ISimpleFastCompatibilityVisitor, ICompoundDecoder, IGlobalDummyRequest,
		IRecurringClient, INamedServer, ICompoundPiecewiseStrategy, ITransientExecutor, ICompatibilityPersistentAdapter,
		ISingleThreadedVisitor, IWraparoundBuilder, ICompatibilityComparer, INamedContext, ICompatibilityDispatcher,
		IAutoChecker, IComplexNamedManager, ITransientExhaustiveSupervisor, IUniversalWrapper, IMultithreadedBuilder,
		IHeuristicCompoundFastDispatcher, IComplexClient, IFastCallback, IUniversalNamedCallback, IExhaustiveStoredWraparoundExecutor,
		IGlobalThunk, IAutoExecutor, ITransientClient, IFeedbackPiecewiseWrapper, IAutoStrategy,
		ILazyFastContext, IFastCompatibilityManager, INetworkAutoMultithreadedStub, ITransientManager, IMultithreadedFeedbackProxyExecutor,
		IDelegatingRequest, INamedState, IWraparoundChecker, IDelegatingServer, ICompatibilityExhaustiveToken,
		IDelegatingUniversalServer, IHeuristicComparer, ICompoundNamedWraparoundManager, IExhaustiveUniversalLazyVisitor, ILazyComparer,
		ITransientDummyClient, IRecurringGlobalAdapter, IGlobalDispatcher, INativeState, IStoredBuilder,
		IPiecewiseMultithreadedExhaustiveExecutor, IPiecewiseServer, INativeAdapter, IComplexChecker, IMultithreadedLazyToken,
		IFeedbackDispatcher, IAutoFastSupervisor, IExhaustiveRecurringFastManager, IFeedbackTransientAutoComparer, IFeedbackSimpleLazyToken,
		IProxyExhaustiveGlobalExecutor, IExhaustiveDispatcher, ILazyDelegatingAdapter, IPiecewiseComparer, IComplexDecoder,
		ILazyGlobalComparer, INetworkAdapter, INetworkDelegatingCallback, IExhaustiveNetworkDispatcher, IComplexNamedFactory,
		IFastDispatcher, IStoredThunk, ICompoundManager, ICompatibilityEncoder, IUniversalLocalDispatcher,
		IMultithreadedWrapper, ITransientUniversalChecker, IPiecewiseAdapter, INamedLazyManager, IRecurringRequest,
		IComplexUniversalVisitor, IMultithreadedCompatibilityPiecewiseBuilder, IWraparoundWrapper, ILocalExecutor, IProxyFactory,
		ISimpleToken, IGlobalComplexState, IStoredCompatibilityWrapper, IGlobalMultithreadedAdapter, IHeuristicNativeLocalThunk,
		INativeLocalFactory, IProxyExhaustiveLazyDispatcher, ISimpleLocalBuilder, IComplexFastCallback, IHeuristicDecoder,
		IStoredHeuristicAutoWrapper, IMultithreadedRecurringWrapper, IDummyStrategy, IDummyNetworkNativeCallback, ITransientDecoder,
		IWraparoundManager, IPersistentAdapter, IPersistentToken, ILocalRequest, ISimpleNetworkEncoder,
		ILazyToken, INetworkComplexWrapper, IUniversalLocalThunk, IFastAdapter, IExhaustiveToken,
		IGlobalHeuristicComparer, IFeedbackDecoder, ISimpleCompoundStub, ITransientCallback, IAutoAdapter,
		IWraparoundMultithreadedCompoundStrategy, IRecurringStub, IFeedbackHeuristicChecker, IPiecewiseHeuristicServer, ILocalLazyServer,
		IPersistentSimpleProxyStrategy, IExhaustiveThunk, ICompatibilityAutoStrategy, ICompatibilityChecker, INamedCallback,
		IDummyThunk, ILocalNativeCallback, IWraparoundAdapter, IExhaustiveChecker, IComplexStub,
		IProxyPersistentManager, IUniversalDispatcher, IPiecewiseCompatibilityThunk, INamedMultithreadedSupervisor, IComplexContext,
		IUniversalStrategy, INativeWrapper, IComplexAdapter, IPersistentWrapper, ISingleThreadedGlobalNativeServer,
		ITransientRequest, IUniversalBuilder, IDelegatingAdapter, IWraparoundPersistentComplexState, IFeedbackAutoSingleThreadedContext,
		ISimpleRecurringCallback, ISingleThreadedSimpleManager, IUniversalDecoder, IWraparoundNetworkDecoder, ICompoundClient,
		ICompatibilityServer, IUniversalFactory, INetworkNativeToken, ISimpleRequest, IGlobalSingleThreadedDispatcher,
		IDummyExhaustivePersistentBuilder, ITransientNamedState, ISingleThreadedStrategy, ITransientState, IComplexNamedRequest,
		IRecurringNativeStub, IComplexPiecewiseSupervisor, IGlobalContext, INativeProxyChecker, IComplexToken,
		IRecurringExecutor, INetworkComparer, INamedWrapper, IRecurringVisitor, IMultithreadedLocalRequest,
		IHeuristicState, IPersistentSimpleStrategy, IPiecewiseNamedSingleThreadedServer, IStoredContext, IAutoExhaustiveContext,
		INamedFeedbackExecutor, IPersistentCallback, ILazyMultithreadedServer, IAutoContext, ICompoundSingleThreadedNamedRequest,
		IExhaustiveCompatibilityThunk, IFeedbackContext, INetworkClient, INativeLazyToken, ISimpleStoredDecoder,
		IUniversalCompoundSupervisor, IRecurringPersistentWraparoundExecutor, IUniversalSupervisor, ITransientRecurringUniversalBuilder, ILocalStoredNetworkStrategy,
		IPiecewiseRecurringComparer, IPersistentBuilder, ICompatibilityNativeVisitor, IPiecewiseGlobalVisitor, IGlobalToken,
		IProxyServer, ILazyAdapter, IHeuristicWraparoundPiecewiseComparer, IWraparoundMultithreadedNetworkClient, IPersistentNamedExecutor,
		IDummyFastComplexDispatcher, IDelegatingRecurringServer, IDelegatingContext, IRecurringTransientLocalVisitor, IRecurringFastSupervisor,
		INativeSupervisor, IRecurringNetworkClient, IDelegatingStub, IDelegatingStoredRequest, IHeuristicChecker,
		ICompoundSingleThreadedManager, IFeedbackRequest, INativeWraparoundRequest, ILazyStoredDecoder, IWraparoundFeedbackNetworkStrategy,
		IComplexCompatibilityCallback, IComplexCompoundDecoder, ILocalAdapter, IGlobalVisitor, INamedGlobalThunk,
		IStoredCompatibilityBuilder, ISimpleExhaustiveTransientComparer, IAutoLocalTransientAdapter, IDelegatingTransientThunk, IFeedbackCallback,
		INativeCompoundToken, IRecurringAdapter, IExhaustiveVisitor, IStoredStub, ICompoundChecker,
		IPersistentGlobalServer, IFastToken, INamedUniversalState, ICompatibilityDelegatingServer, IFeedbackGlobalDecoder,
		ILazyEncoder, IPiecewiseThunk, IWraparoundVisitor, IAutoNetworkHeuristicStub, IFeedbackState,
		IHeuristicToken, ICompatibilityState, ICompatibilityAdapter, IPiecewiseSingleThreadedExecutor, IHeuristicRequest,
		ISimpleBuilder, IComplexExecutor, IAutoCompatibilityEncoder, IMultithreadedToken, ICompatibilitySimpleNamedWrapper,
		ITransientComplexClient, ILocalStoredContext, IGlobalDelegatingComparer, IAutoUniversalFactory, IMultithreadedAdapter,
		ISimpleClient, IGlobalNamedToken, IProxyPersistentEncoder, INetworkContext, INativeThunk,
		IProxyChecker, ICompoundDelegatingAdapter, IMultithreadedNativeWrapper, IHeuristicManager, IMultithreadedDummyFactory,
		ICompoundPersistentComparer, IDummyComparer, INamedSingleThreadedRequest, ILocalToken, INamedPiecewiseWrapper,
		IStoredDecoder, IGlobalExecutor, ISingleThreadedExhaustiveState, IHeuristicExecutor, IFeedbackComparer,
		ISimpleProxyState, IExhaustiveStub, ISingleThreadedFastEncoder, ILocalChecker, IFeedbackSupervisor,
		ICompatibilityPiecewiseMultithreadedSupervisor, INativeLazyStrategy, IFastWraparoundStub, IStoredSimpleSupervisor, IUniversalRecurringDispatcher,
		ICompatibilityContext, IComplexFactory, ILazyDispatcher, INamedNativeVisitor, ILocalNativeRequest,
		IComplexNamedWrapper, IAutoGlobalExhaustiveStrategy, IAutoRecurringChecker, IAutoNetworkManager, ILazyComplexPiecewiseBuilder,
		IHeuristicUniversalLocalDecoder, INetworkAutoComplexDispatcher, IGlobalRequest, ILocalAutoBuilder, IProxyDispatcher,
		IRecurringChecker, IGlobalDecoder, IFeedbackPersistentLocalFactory, IExhaustiveStrategy, IStoredNativeSimpleServer,
		ITransientChecker, IHeuristicSimpleState, ITransientSimpleStrategy, IUniversalManager, IPersistentStrategy,
		INetworkDelegatingEncoder, IHeuristicStrategy, IWraparoundServer, IDelegatingEncoder, INetworkStub,
		IUniversalWraparoundStrategy, INativeChecker, IExhaustiveServer, INativeVisitor, ICompoundExecutor,
		ISingleThreadedNativeState, IDelegatingWrapper, IRecurringFeedbackContext, IUniversalToken, ICompatibilityDecoder,
		IComplexEncoder, IExhaustivePiecewiseAdapter, INamedUniversalDummyContext, IStoredManager, IFeedbackLazyEncoder,
		IAutoRecurringAdapter, ITransientCompatibilityEncoder, IGlobalMultithreadedDispatcher, IComplexNativeManager, IAutoCompatibilityClient,
		IRecurringNativeDispatcher, ILazyCallback, ILocalStrategy, IUniversalStub, IPiecewiseClient,
		ISingleThreadedDispatcher, IProxyStoredSingleThreadedContext, INativeContext, IComplexTransientExecutor, ISingleThreadedManager,
		IMultithreadedRecurringProxyContext, ICompoundPiecewiseGlobalAdapter, ILocalBuilder, IMultithreadedRequest, ISimpleDummyManager,
		ISimpleContext, IFeedbackClient, ILocalNamedManager, ILazyState, ICompatibilityStrategy,
		ITransientCompoundSimpleComparer, IAutoNativeNamedAdapter, IProxyPiecewiseCallback, IRecurringNetworkDelegatingVisitor, IFeedbackAdapter,
		ILocalProxyThunk, IFastProxyToken, IPersistentExecutor, IFastRecurringChecker, IPiecewiseExhaustiveEncoder,
		ILazyUniversalStub, IHeuristicAdapter, INamedComparer, IPersistentState, ISimpleAutoWrapper,
		IStoredComparer, IGlobalComparer, IDummyClient, IPersistentProxyStub, ISimpleState,
		ILazyThunk, IPiecewiseRecurringCallback, IMultithreadedSupervisor, IRecurringNativeDecoder, IFastClient,
		ILocalNamedSupervisor, IPiecewiseWraparoundGlobalToken, IHeuristicPiecewiseDispatcher, ITransientWraparoundToken, IProxyMultithreadedCallback,
		IPersistentSimpleAutoToken, IExhaustiveSupervisor, IFeedbackDelegatingStrategy, ICompatibilitySimpleBuilder, IMultithreadedState,
		INetworkState, IDummyDecoder, IRecurringCallback, IDummyLocalContext, IComplexRecurringToken,
		INetworkUniversalRecurringStub, IFeedbackAutoStub, ICompoundDelegatingVisitor, IMultithreadedExhaustiveWraparoundVisitor, INamedToken,
		ICompoundRequest, IProxyManager, INativeProxyClient, IAutoCompoundWrapper, ITransientRecurringSimpleStub,
		ICompoundToken, ILazyServer, IWraparoundComparer, IExhaustiveBuilder, IMultithreadedComparer,
		IFastBuilder, IStoredUniversalAdapter, ISingleThreadedStub, ISingleThreadedFastRecurringRequest, IUniversalAutoLazyVisitor,
		INamedFastServer, IPersistentServer, INetworkPersistentThunk, ICompatibilitySingleThreadedThunk, INamedStub,
		ILocalRecurringAutoComparer, IFastSimplePersistentEncoder, IWraparoundComplexClient, IAutoSupervisor, ICompoundFeedbackCompatibilityStub,
		INamedFactory, IFastDummyExecutor, ISimpleExecutor, IStoredToken, IDelegatingVisitor,
		IDelegatingLazyPersistentWrapper, INetworkNativeCallback, ISimpleWrapper, ITransientSupervisor, IRecurringServer,
		IPersistentRequest, INetworkWraparoundChecker, ISingleThreadedComparer, ISimpleHeuristicExecutor, IFastFeedbackRequest,
		IComplexComparer, IExhaustiveNamedUniversalComparer, IProxyState, IProxyExhaustiveContext, IAutoClient,
		IGlobalNamedFactory, ILocalStoredChecker, ISimpleThunk, IDummyServer, ISimpleChecker,
		IUniversalNetworkDispatcher, IExhaustiveMultithreadedUniversalState, ISingleThreadedSimpleSupervisor, IDelegatingSupervisor, IFastEncoder,
		IProxyCompoundState, IStoredFastFactory, IWraparoundFeedbackState, ITransientAdapter, IRecurringMultithreadedProxyChecker,
		IRecurringCompoundThunk, IDummyWrapper, IProxyCompoundAdapter, ISimpleCompatibilityStrategy, IHeuristicContext,
		IPersistentDelegatingSupervisor, IHeuristicFeedbackStub, IPiecewiseSimpleComparer, ISimplePersistentStub, IProxyNetworkDispatcher,
		ILazyDelegatingDispatcher, ISimplePiecewiseState, IStoredMultithreadedContext, ICompoundComparer, IComplexDispatcher,
		ILazyNamedCallback, IAutoManager, ILocalProxyChecker, ILocalVisitor, INamedThunk,
		IFastRequest, IExhaustiveWrapper, IGlobalPersistentExecutor, IHeuristicCompatibilityRequest, IDelegatingDispatcher,
		IPiecewiseDispatcher, IProxyExecutor, ICompoundTransientChecker, ISingleThreadedNamedEncoder, IHeuristicDispatcher,
		IRecurringDispatcher, IDelegatingFastServer, IDelegatingCallback, IDelegatingCompatibilityDecoder, ISimpleManager,
		ITransientStrategy, IPiecewiseStrategy, IDummyVisitor, IStoredChecker, IGlobalState,
		IFeedbackFastContext, ILazyAutoGlobalThunk, ISingleThreadedDummyFastCallback, IMultithreadedSimpleExecutor, ICompoundEncoder,
		IProxyAutoState, IComplexFeedbackExecutor, INativeAutoNetworkCallback, IMultithreadedNamedExecutor, INamedChecker,
		IRecurringFastManager, ILocalTransientUniversalThunk, IGlobalChecker, IPersistentPiecewiseToken, IDelegatingState,
		IWraparoundFastCompoundServer, IDummyChecker, ISingleThreadedWrapper, IGlobalSupervisor, IDelegatingThunk,
		INetworkComplexSupervisor, IPersistentThunk, INamedMultithreadedDecoder, INamedRequest, ICompatibilityComplexFastRequest,
		IPiecewiseToken, ITransientDelegatingFactory, ISimpleStrategy, IDelegatingAutoStub, IWraparoundStrategy,
		ICompoundDispatcher, ISingleThreadedChecker, IMultithreadedRecurringNamedAdapter, IAutoDecoder, IRecurringDummyState,
		ISingleThreadedToken, ISingleThreadedExecutor, IAutoWrapper, IDelegatingNamedBuilder, IDummyContext,
		IGlobalWraparoundDecoder, IGlobalRecurringStub, IStoredNativeState, INetworkThunk, IExhaustiveExecutor,
		IExhaustiveFeedbackContext, IProxyStoredCallback, IRecurringManager, ICompoundStoredDispatcher, ISingleThreadedAdapter,
		IProxyContext, IWraparoundExhaustiveFactory, IComplexStoredWrapper, IMultithreadedClient, INativeCompatibilityExecutor,
		IProxyDelegatingRequest, IHeuristicPiecewiseState, IDelegatingDecoder, IDummyTransientCallback, IProxyMultithreadedFeedbackAdapter,
		IMultithreadedPiecewiseStrategy, IWraparoundState, IExhaustivePiecewiseDispatcher, ISingleThreadedCallback, ISingleThreadedRequest,
		IProxyThunk, ISimpleExhaustiveContext, ICompoundServer, ICompoundAdapter, IMultithreadedFactory,
		IStoredRecurringFactory, IUniversalDummyEncoder, IFeedbackFactory, ISimpleLocalServer, IAutoNamedSingleThreadedExecutor,
		IDelegatingExhaustiveDecoder, ICompatibilitySupervisor, INamedCompoundMultithreadedSupervisor, IUniversalServer, ISingleThreadedNamedStrategy,
		IMultithreadedServer, IStoredWrapper, IFastCompatibilityStub, IPiecewiseRequest, ISimpleDecoder,
		ICompoundRecurringStrategy, IRecurringBuilder, IDelegatingRecurringSingleThreadedVisitor, IRecurringWrapper, IDummyRequest,
		IDelegatingManager, IWraparoundMultithreadedGlobalEncoder, ISimplePersistentAdapter, INamedNetworkFactory, IStoredPiecewiseServer,
		IComplexStoredStub, INetworkProxyCompoundComparer, ICompoundStub, IExhaustiveUniversalVisitor, IGlobalFastDecoder,
		IMultithreadedEncoder, IWraparoundStub, INativeManager, INativeCompoundNetworkState, IHeuristicFeedbackProxyToken,
		IFastNetworkClient, IAutoRequest, IExhaustiveManager, IAutoVisitor, ICompoundVisitor,
		ISimpleTransientVisitor, ISingleThreadedUniversalNetworkState, IProxyRequest, ICompatibilityThunk, IFeedbackServer,
		IWraparoundMultithreadedDispatcher, IAutoNativeSupervisor, IGlobalBuilder, INetworkBuilder, IPiecewiseNetworkToken,
		IGlobalStrategy, IGlobalPersistentAdapter, IDummySupervisor, IFastServer, IPiecewiseStoredNativeToken,
		IExhaustiveSingleThreadedServer, IProxyToken, ILocalManager, IUniversalStoredChecker, INativeStrategy,
		INamedCompatibilityComparer, ILazyNativeThunk, INamedEncoder, IWraparoundComplexAutoClient, ILocalSupervisor,
		IWraparoundExecutor, IPiecewiseStub, IComplexNamedContext, ICompoundWrapper, IRecurringPiecewiseSupervisor,
		IDummyAutoDispatcher, IPiecewiseLocalCallback, ILazyRecurringEncoder, IPersistentStub, IComplexBuilder,
		IProxyTransientBuilder, IFastWrapper, ILazyStub, INetworkChecker, IComplexWrapper,
		INamedWraparoundStoredEncoder, IPiecewiseStoredNetworkEncoder, IFastAutoStub, IExhaustiveComparer, ILocalThunk,
		IPersistentManager, ISingleThreadedNativeFactory, IExhaustiveComplexFactory, IPiecewiseBuilder, INativeFactory,
		ILocalHeuristicToken, IRecurringHeuristicSupervisor, IComplexManager, INamedDecoder, IMultithreadedWraparoundClient,
		IMultithreadedChecker, IDelegatingCompatibilityStrategy, INetworkRequest, IHeuristicWrapper, ISimpleCompoundDispatcher,
		IFastStrategy, INetworkAutoDispatcher, IRecurringFactory, IRecurringPiecewiseTransientChecker, ISingleThreadedState,
		INetworkFeedbackPiecewiseState, IFeedbackCompatibilityGlobalThunk, ILocalWrapper, IProxyLazyHeuristicServer, ISimpleExhaustiveComparer,
		IMultithreadedExecutor, ICompoundSingleThreadedServer, IStoredLazyClient, IWraparoundCompoundBuilder, ISingleThreadedProxyExecutor,
		IMultithreadedExhaustiveCompoundDispatcher, IMultithreadedManager, ISingleThreadedPiecewiseNativeEncoder, IWraparoundDecoder, IHeuristicCompatibilityContext,
		IDummyGlobalCallback, ISingleThreadedServer, ILocalExhaustiveCompoundFactory, IPiecewiseStoredVisitor, IComplexGlobalContext,
		IWraparoundDelegatingLocalWrapper, IPiecewiseCallback, IComplexSupervisor, IHeuristicProxyLazyWrapper, ICompatibilityTransientBuilder,
		INamedNativeCompatibilityDispatcher, INamedAutoComparer, ICompatibilityRequest, ICompatibilitySingleThreadedAdapter, INativeExecutor,
		INativeAutoChecker, ICompoundStrategy, IPiecewiseWraparoundLazyState, INetworkWrapper, IMultithreadedNamedContext,
		IPersistentClient, IPiecewiseCompoundAdapter, ICompatibilityWrapper, IFeedbackEncoder, IHeuristicMultithreadedExecutor,
		IRecurringNamedEncoder, ICompatibilityExecutor, ICompoundBuilder, IDelegatingFactory, ICompoundPersistentRequest,
		IUniversalAutoStub, ITransientBuilder, IDelegatingRecurringDummySupervisor, INamedAdapter, ICompatibilityFactory,
		ILocalEncoder, IUniversalTransientExecutor, IHeuristicTransientContext, ILazyExhaustiveUniversalAdapter, IRecurringExhaustiveExecutor,
		INativeDummyThunk, IPiecewiseNamedDecoder, IComplexCallback, ISimpleCallback, IStoredExecutor,
		ICompatibilityCompoundLocalComparer, IDummyStub, IProxyRecurringStrategy, IDelegatingHeuristicContext, IStoredEncoder,
		IRecurringNamedWrapper, IWraparoundStoredUniversalWrapper, IDelegatingTransientWrapper, IStoredServer, ILocalState,
		IPiecewiseVisitor, IWraparoundPersistentEncoder, ILazyComplexComparer, IFastDelegatingBuilder, IUniversalNamedFactory,
		IComplexProxyFactory, IAutoFastFactory, IStoredCallback, IFeedbackChecker, INativeStoredClient,
		IComplexUniversalAdapter, ITransientDispatcher, IProxyNamedComparer, IPiecewiseLocalVisitor, IComplexState,
		IDummyExhaustiveEncoder, IMultithreadedStub, IWraparoundMultithreadedToken, IAutoNetworkCallback, IUniversalLocalCallback,
		IHeuristicTransientToken, IUniversalGlobalWraparoundBuilder, IFeedbackSingleThreadedCallback, IProxySimpleDummyExecutor, ITransientNamedAdapter,
		INetworkSupervisor, IProxyComparer, INativeBuilder, IStoredNamedFactory, IRecurringDummySimpleCallback,
		IHeuristicRecurringComplexVisitor, IHeuristicLazyDecoder, IComplexRequest, IPersistentNamedChecker, IUniversalWraparoundStub,
		IGlobalMultithreadedBuilder, IProxyEncoder, IExhaustiveLazyContext, IUniversalChecker, IComplexGlobalSupervisor,
		INetworkEncoder, IUniversalCallback, IGlobalTransientNetworkContext, ISimpleLocalTransientChecker, INamedGlobalContext,
		IDelegatingGlobalContext, IPersistentEncoder, ISingleThreadedDummySimpleCallback, IWraparoundDispatcher, ISimpleServer,
		ICompoundThunk, IWraparoundClient, ILocalUniversalWraparoundVisitor, IProxyCallback, IMultithreadedDispatcher,
		ILocalComparer, INativeMultithreadedLocalSupervisor, IRecurringStrategy, IMultithreadedGlobalWrapper, IFastSingleThreadedServer,
		INetworkLocalRequest, IExhaustiveProxySupervisor, ICompoundPiecewiseNamedToken, IUniversalVisitor, ICompoundFastNetworkToken,
		INetworkRecurringServer, IExhaustiveDelegatingState, ISimplePersistentHeuristicThunk, INamedNetworkStrategy, ILazyVisitor,
		IAutoHeuristicFastState, ICompoundPersistentFastDecoder, IMultithreadedDelegatingFactory, INamedSupervisor, ICompatibilityVisitor,
		IAutoLocalPiecewiseFactory, IStoredExhaustiveDummyServer, ISimpleNetworkCompoundStub, IGlobalServer, ICompoundExhaustiveState,
		IDelegatingDummySingleThreadedCallback, IPersistentExhaustiveFactory, IDelegatingComplexFactory, ISimpleProxyStoredChecker, IAutoExhaustiveComparer,
		INamedBuilder, IDelegatingExhaustiveStrategy, IComplexAutoPiecewiseCallback, IHeuristicThunk, IFastExecutor,
		INamedManager, IFastManager, ICompoundCompatibilityExecutor, IStoredCompoundClient, INetworkWraparoundWrapper,
		IUniversalRecurringExecutor, IMultithreadedAutoVisitor, IPiecewiseProxyComparer, IFeedbackPiecewiseRequest, IGlobalPersistentPiecewiseStub,
		IDelegatingStoredExecutor, IDelegatingWraparoundDecoder, IRecurringSupervisor, ISimpleAutoStub, IRecurringGlobalFactory,
		IAutoBuilder, IGlobalNetworkBuilder, ITransientStub, IHeuristicServer, IProxyNamedComplexSupervisor,
		IExhaustiveCompatibilityFactory, IStoredClient, IUniversalProxyPersistentDispatcher, ILocalUniversalDecoder, ILazyFeedbackCallback,
		IHeuristicPersistentStub, IStoredSupervisor, IDummyCompatibilityPiecewiseDecoder, IExhaustivePersistentStrategy, IComplexRecurringLocalServer,
		IExhaustiveLazySingleThreadedContext, IGlobalHeuristicPersistentWrapper, IDelegatingMultithreadedDecoder, IWraparoundEncoder, IRecurringDecoder,
		IUniversalAdapter, IPersistentContext, IWraparoundCompoundRecurringVisitor, IProxyLocalStub, IFastStub,
		INativeMultithreadedEncoder, IAutoServer, INamedAutoVisitor, IWraparoundProxyDecoder, ISingleThreadedEncoder,
		IDummyLazyToken, IFastProxyComparer, IPersistentSingleThreadedExecutor, IFeedbackThunk, INativeClient,
		IDelegatingComplexState, IMultithreadedNamedCompoundToken, ICompatibilityPiecewiseContext, IUniversalExecutor, ITransientFactory,
		IPersistentGlobalToken, ISimpleNamedThunk, INamedProxyManager, IStoredNamedChecker, IDelegatingMultithreadedLazyVisitor,
		INamedDelegatingNetworkSupervisor, IComplexStrategy, IGlobalFeedbackWraparoundEncoder, IWraparoundThunk, IDelegatingComplexDispatcher,
		ISimpleExhaustiveStub, IDummyWraparoundContext, IFastDelegatingCallback, INetworkWraparoundNativeCallback, INetworkServer,
		ISingleThreadedThunk, IWraparoundTransientEncoder, IWraparoundLazyFactory, IWraparoundSingleThreadedDelegatingFactory, IUniversalNetworkEncoder,
		ILocalMultithreadedState, IProxyWraparoundNamedServer, IMultithreadedCallback, ITransientWrapper, IStoredUniversalNetworkServer,
		IWraparoundLazyStub, IFeedbackWrapper, INativeRequest, IDelegatingProxyServer, IStoredSimpleChecker,
		IExhaustiveUniversalDecoder, IAutoComparer, IHeuristicFactory, INativeDispatcher, ICompoundFeedbackWrapper,
		IDelegatingNativeToken, IDummyRecurringThunk, IUniversalExhaustiveStrategy, IAutoMultithreadedEncoder {$ifdef _1000th}, ITransientProxyChecker {$endif})

		procedure Build;
		procedure Compare;
		procedure Listen;
		procedure Send;
		procedure Broadcast;
		procedure Check;
		procedure Perform;
		procedure Call;
		procedure Visit;
		procedure Save;
		procedure Execute;
		procedure Apply;
		procedure Make;
		procedure Dispatch;
		procedure Cancel;
		procedure Pass;
		procedure Get;
		procedure Switch;
		procedure Wrap;
		procedure Encode;
		procedure Decode;
	end;

	procedure God.Build; begin writeln('Build'); end;
	procedure God.Compare; begin writeln('Compare'); end;
	procedure God.Listen; begin writeln('Listen'); end;
	procedure God.Broadcast; begin writeln('Broadcast'); end;
	procedure God.Send; begin writeln('Send'); end;
	procedure God.Check; begin writeln('Check'); end;
	procedure God.Perform; begin writeln('Perform'); end;
	procedure God.Call; begin writeln('Call'); end;
	procedure God.Visit; begin writeln('Visit'); end;
	procedure God.Save; begin writeln('Save'); end;
	procedure God.Execute; begin writeln('Execute'); end;
	procedure God.Apply; begin writeln('Apply'); end;
	procedure God.Make; begin writeln('Make'); end;
	procedure God.Dispatch; begin writeln('Dispatch'); end;
	procedure God.Cancel; begin writeln('Cancel'); end;
	procedure God.Pass; begin writeln('Pass'); end;
	procedure God.Get; begin writeln('Get'); end;
	procedure God.Switch; begin writeln('Switch'); end;
	procedure God.Wrap; begin writeln('Wrap'); end;
	procedure God.Encode; begin writeln('Encode'); end;
	procedure God.Decode; begin writeln('Decode'); end;

begin
	writeln('God.InstanceSize = TInterfacedObject.InstanceSize + ', (God.InstanceSize - TInterfacedObject.InstanceSize) div sizeof(pointer), ' pointers');
end.
