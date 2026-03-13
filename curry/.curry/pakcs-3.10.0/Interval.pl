%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').
:-importModule('Rational').

:-curryModule('Interval').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23','_inst#Prelude.Eq#Interval.Interval#',1,'Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23','_impl#==#Prelude.Eq#Interval.Interval#',2,'Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.Interval\'23','_impl#/=#Prelude.Eq#Interval.Interval#',0,'Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23','_inst#Prelude.Ord#Interval.Interval#',1,'Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23','_impl#compare#Prelude.Ord#Interval.Interval#',2,'Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23\'3C\'23Prelude.Ord\'23Interval.Interval\'23','_impl#<#Prelude.Ord#Interval.Interval#',0,'Interval._impl\'23\'3C\'23Prelude.Ord\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23\'3E\'23Prelude.Ord\'23Interval.Interval\'23','_impl#>#Prelude.Ord#Interval.Interval#',0,'Interval._impl\'23\'3E\'23Prelude.Ord\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23\'3C\'3D\'23Prelude.Ord\'23Interval.Interval\'23','_impl#<=#Prelude.Ord#Interval.Interval#',0,'Interval._impl\'23\'3C\'3D\'23Prelude.Ord\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23\'3E\'3D\'23Prelude.Ord\'23Interval.Interval\'23','_impl#>=#Prelude.Ord#Interval.Interval#',0,'Interval._impl\'23\'3E\'3D\'23Prelude.Ord\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23min\'23Prelude.Ord\'23Interval.Interval\'23','_impl#min#Prelude.Ord#Interval.Interval#',0,'Interval._impl\'23min\'23Prelude.Ord\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23max\'23Prelude.Ord\'23Interval.Interval\'23','_impl#max#Prelude.Ord#Interval.Interval#',0,'Interval._impl\'23max\'23Prelude.Ord\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._inst\'23Prelude.Show\'23Interval.Interval\'23','_inst#Prelude.Show#Interval.Interval#',1,'Interval._inst\'23Prelude.Show\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23','_impl#show#Prelude.Show#Interval.Interval#',0,'Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.Interval\'23','_impl#showsPrec#Prelude.Show#Interval.Interval#',0,'Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23showList\'23Prelude.Show\'23Interval.Interval\'23','_impl#showList#Prelude.Show#Interval.Interval#',0,'Interval._impl\'23showList\'23Prelude.Show\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23','_inst#Prelude.Eq#Interval.ComplexInterval#',1,'Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23','_impl#==#Prelude.Eq#Interval.ComplexInterval#',2,'Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23','_impl#/=#Prelude.Eq#Interval.ComplexInterval#',0,'Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23','_inst#Prelude.Show#Interval.ComplexInterval#',1,'Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23','_impl#show#Prelude.Show#Interval.ComplexInterval#',1,'Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.ComplexInterval\'23','_impl#showsPrec#Prelude.Show#Interval.ComplexInterval#',0,'Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._impl\'23showList\'23Prelude.Show\'23Interval.ComplexInterval\'23','_impl#showList#Prelude.Show#Interval.ComplexInterval#',0,'Interval._impl\'23showList\'23Prelude.Show\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._inst\'23Prelude.Data\'23Interval.Interval\'23','_inst#Prelude.Data#Interval.Interval#',1,'Interval._inst\'23Prelude.Data\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23','_impl#===#Prelude.Data#Interval.Interval#',2,'Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._impl\'23aValue\'23Prelude.Data\'23Interval.Interval\'23','_impl#aValue#Prelude.Data#Interval.Interval#',0,'Interval._impl\'23aValue\'23Prelude.Data\'23Interval.Interval\'23',nofix,notype).
functiontype('Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23','_inst#Prelude.Data#Interval.ComplexInterval#',1,'Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23','_impl#===#Prelude.Data#Interval.ComplexInterval#',2,'Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval._impl\'23aValue\'23Prelude.Data\'23Interval.ComplexInterval\'23','_impl#aValue#Prelude.Data#Interval.ComplexInterval#',0,'Interval._impl\'23aValue\'23Prelude.Data\'23Interval.ComplexInterval\'23',nofix,notype).
functiontype('Interval.ri','Interval.ri',0,'Interval.ri',nofix,notype).
functiontype('Interval.midpoint',midpoint,1,'Interval.midpoint',nofix,notype).
functiontype('Interval.width',width,1,'Interval.width',nofix,notype).
functiontype('Interval.ivContains',ivContains,2,'Interval.ivContains',nofix,notype).
functiontype('Interval.overlaps',overlaps,2,'Interval.overlaps',nofix,notype).
functiontype('Interval.bisect',bisect,1,'Interval.bisect',nofix,notype).
functiontype('Interval.refine',refine,2,'Interval.refine',nofix,notype).
functiontype('Interval.refine._\'23selFP2\'23left','Interval.refine._#selFP2#left',1,'Interval.refine._\'23selFP2\'23left',nofix,notype).
functiontype('Interval.fromRat',fromRat,1,'Interval.fromRat',nofix,notype).
functiontype('Interval.iadd',iadd,2,'Interval.iadd',nofix,notype).
functiontype('Interval.isub',isub,2,'Interval.isub',nofix,notype).
functiontype('Interval.imul',imul,2,'Interval.imul',nofix,notype).
functiontype('Interval.iinv',iinv,1,'Interval.iinv',nofix,notype).
functiontype('Interval.idiv',idiv,2,'Interval.idiv',nofix,notype).
functiontype('Interval.ipow',ipow,2,'Interval.ipow',nofix,notype).
functiontype('Interval.ipow._\'23selFP4\'23l','Interval.ipow._#selFP4#l',1,'Interval.ipow._\'23selFP4\'23l',nofix,notype).
functiontype('Interval.ipow._\'23selFP5\'23h','Interval.ipow._#selFP5#h',1,'Interval.ipow._\'23selFP5\'23h',nofix,notype).
functiontype('Interval.isqrt',isqrt,1,'Interval.isqrt',nofix,notype).
functiontype('Interval.inth',inth,2,'Interval.inth',nofix,notype).
functiontype('Interval.inth._\'23selFP7\'23l\'27','Interval.inth._#selFP7#l\'',1,'Interval.inth._\'23selFP7\'23l\'27',nofix,notype).
functiontype('Interval.inth._\'23selFP8\'23h\'27','Interval.inth._#selFP8#h\'',1,'Interval.inth._\'23selFP8\'23h\'27',nofix,notype).
functiontype('Interval.nthRootLower','Interval.nthRootLower',2,'Interval.nthRootLower',nofix,notype).
functiontype('Interval.nthRootUpper','Interval.nthRootUpper',2,'Interval.nthRootUpper',nofix,notype).
functiontype('Interval.bisectDown','Interval.bisectDown',5,'Interval.bisectDown',nofix,notype).
functiontype('Interval.bisectUp','Interval.bisectUp',5,'Interval.bisectUp',nofix,notype).
functiontype('Interval.iabs',iabs,1,'Interval.iabs',nofix,notype).
functiontype('Interval.strictlyPositive',strictlyPositive,1,'Interval.strictlyPositive',nofix,notype).
functiontype('Interval.strictlyNegative',strictlyNegative,1,'Interval.strictlyNegative',nofix,notype).
functiontype('Interval.containsZero',containsZero,1,'Interval.containsZero',nofix,notype).
functiontype('Interval.ciFromRat',ciFromRat,1,'Interval.ciFromRat',nofix,notype).
functiontype('Interval.ciFromReal',ciFromReal,1,'Interval.ciFromReal',nofix,notype).
functiontype('Interval.ciadd',ciadd,2,'Interval.ciadd',nofix,notype).
functiontype('Interval.cisub',cisub,2,'Interval.cisub',nofix,notype).
functiontype('Interval.cineg',cineg,1,'Interval.cineg',nofix,notype).
functiontype('Interval.cimul',cimul,2,'Interval.cimul',nofix,notype).
functiontype('Interval.ciinv',ciinv,1,'Interval.ciinv',nofix,notype).
functiontype('Interval.ciinv._\'23selFP10\'23il','Interval.ciinv._#selFP10#il',1,'Interval.ciinv._\'23selFP10\'23il',nofix,notype).
functiontype('Interval.ciinv._\'23selFP11\'23ih','Interval.ciinv._#selFP11#ih',1,'Interval.ciinv._\'23selFP11\'23ih',nofix,notype).
functiontype('Interval.cipow',cipow,2,'Interval.cipow',nofix,notype).
functiontype('Interval.ciMagnitudeSq',ciMagnitudeSq,1,'Interval.ciMagnitudeSq',nofix,notype).
functiontype('Interval.ciRealPart',ciRealPart,1,'Interval.ciRealPart',nofix,notype).
functiontype('Interval.ciImagPart',ciImagPart,1,'Interval.ciImagPart',nofix,notype).
functiontype('Interval.piInterval',piInterval,0,'Interval.piInterval',nofix,notype).
functiontype('Interval.iatanSmall','Interval.iatanSmall',1,'Interval.iatanSmall',nofix,notype).
functiontype('Interval.iatanSmall._\'23lambda2','Interval.iatanSmall._#lambda2',2,'Interval.iatanSmall._\'23lambda2',nofix,notype).
functiontype('Interval.iatanPoint','Interval.iatanPoint',1,'Interval.iatanPoint',nofix,notype).
functiontype('Interval.iatanPoint._\'23selFP13\'23pl','Interval.iatanPoint._#selFP13#pl',1,'Interval.iatanPoint._\'23selFP13\'23pl',nofix,notype).
functiontype('Interval.iatanPoint._\'23selFP14\'23ph','Interval.iatanPoint._#selFP14#ph',1,'Interval.iatanPoint._\'23selFP14\'23ph',nofix,notype).
functiontype('Interval.iatan',iatan,1,'Interval.iatan',nofix,notype).
functiontype('Interval.iatan._\'23selFP18\'23al','Interval.iatan._#selFP18#al',1,'Interval.iatan._\'23selFP18\'23al',nofix,notype).
functiontype('Interval.iatan._\'23selFP17\'23bh','Interval.iatan._#selFP17#bh',1,'Interval.iatan._\'23selFP17\'23bh',nofix,notype).
functiontype('Interval.icosPoint','Interval.icosPoint',1,'Interval.icosPoint',nofix,notype).
functiontype('Interval.icosPoint._\'23lambda3','Interval.icosPoint._#lambda3',2,'Interval.icosPoint._\'23lambda3',nofix,notype).
functiontype('Interval.factorial','Interval.factorial',1,'Interval.factorial',nofix,notype).
functiontype('Interval.factorial.go.143','Interval.factorial.go.143',4,'Interval.factorial.go.143',nofix,notype).
functiontype('Interval.icos',icos,1,'Interval.icos',nofix,notype).
functiontype('Interval.ivLo','Interval.ivLo',1,'Interval.ivLo',nofix,notype).
functiontype('Interval.ivHi','Interval.ivHi',1,'Interval.ivHi',nofix,notype).
functiontype('Interval.isin',isin,1,'Interval.isin',nofix,notype).
functiontype('Interval.iatan2',iatan2,2,'Interval.iatan2',nofix,notype).
functiontype('Interval.iatan2._\'23selFP19\'23pl','Interval.iatan2._#selFP19#pl',1,'Interval.iatan2._\'23selFP19\'23pl',nofix,notype).
functiontype('Interval.iatan2._\'23selFP20\'23ph','Interval.iatan2._#selFP20#ph',1,'Interval.iatan2._\'23selFP20\'23ph',nofix,notype).
functiontype('Interval.iatan2._\'23selFP21\'23pl','Interval.iatan2._#selFP21#pl',1,'Interval.iatan2._\'23selFP21\'23pl',nofix,notype).
functiontype('Interval.iatan2._\'23selFP22\'23ph','Interval.iatan2._#selFP22#ph',1,'Interval.iatan2._\'23selFP22\'23ph',nofix,notype).
functiontype('Interval.iatan2._\'23selFP24\'23ph','Interval.iatan2._#selFP24#ph',1,'Interval.iatan2._\'23selFP24\'23ph',nofix,notype).
functiontype('Interval.cinthroot',cinthroot,2,'Interval.cinthroot',nofix,notype).
functiontype('Interval.showInterval',showInterval,1,'Interval.showInterval',nofix,notype).
functiontype('Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0','Interval._impl#compare#Prelude.Ord#Interval.Interval#._#caseor0',4,'Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0',nofix,notype).
functiontype('Interval.refine._\'23caseor0','Interval.refine._#caseor0',3,'Interval.refine._\'23caseor0',nofix,notype).
functiontype('Interval.ipow._\'23caseor0','Interval.ipow._#caseor0',2,'Interval.ipow._\'23caseor0',nofix,notype).
functiontype('Interval.bisectDown._\'23caseor0','Interval.bisectDown._#caseor0',7,'Interval.bisectDown._\'23caseor0',nofix,notype).
functiontype('Interval.bisectUp._\'23caseor0','Interval.bisectUp._#caseor0',7,'Interval.bisectUp._\'23caseor0',nofix,notype).
functiontype('Interval.iatanSmall._\'23lambda2._\'23caseor0','Interval.iatanSmall._#lambda2._#caseor0',1,'Interval.iatanSmall._\'23lambda2._\'23caseor0',nofix,notype).
functiontype('Interval.icosPoint._\'23lambda3._\'23caseor0','Interval.icosPoint._#lambda3._#caseor0',1,'Interval.icosPoint._\'23lambda3._\'23caseor0',nofix,notype).
functiontype('Interval.icos._\'23caseor0','Interval.icos._#caseor0',2,'Interval.icos._\'23caseor0',nofix,notype).
functiontype('Interval.icos._\'23caseor0._\'23caseor0','Interval.icos._#caseor0._#caseor0',2,'Interval.icos._\'23caseor0._\'23caseor0',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.
constructortype('Interval.IV','IV',2,'IV',0,notype,[]).
constructortype('Interval.CI','CI',2,'CI',0,notype,[]).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23'(_5272862,_5272864,_5272866,_5272868):-freeze(_5272866,'blocked_Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23'(_5272862,_5272864,_5272866,_5272868)).
'blocked_Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23'(_5272948,_5273628,_5273634,_5273640):-hnf(_5272948,_5275806,_5273634,_5275818),'blocked_Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23_1'(_5275806,_5273628,_5275818,_5273640).

'blocked_Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23_1'(_5276360,_5276362,_5276364,_5276366):-freeze(_5276364,'blocked_blocked_Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23_1'(_5276360,_5276362,_5276364,_5276366)).
'blocked_blocked_Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23_1'('Prelude.()','Prelude._Dict\'23Eq'(partcall(2,'Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23',[]),'Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.Interval\'23'),_5276712,_5276712):-!.
'blocked_blocked_Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23_1'('FAIL'(_5278296),'FAIL'(_5278296),_5278310,_5278310):-nonvar(_5278296).

'Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23'(_5280480,_5280482,_5280484,_5280486,_5280488):-freeze(_5280486,'blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23'(_5280480,_5280482,_5280484,_5280486,_5280488)).
'blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23'(_5280576,_5280594,_5282144,_5282150,_5282156):-hnf(_5280576,_5284654,_5282150,_5284672),'blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1'(_5284654,_5280594,_5282144,_5284672,_5282156).

'blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1'(_5285276,_5285278,_5285280,_5285282,_5285284):-freeze(_5285282,'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1'(_5285276,_5285278,_5285280,_5285282,_5285284)).
'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1'('Interval.IV'(_5280710,_5280728),_5280594,_5286178,_5286184,_5286190):-!,hnf(_5280594,_5289408,_5286184,_5289432),'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1_Interval.IV_3'(_5289408,_5280710,_5280728,_5286178,_5289432,_5286190).

'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1_Interval.IV_3'(_5290194,_5290196,_5290198,_5290200,_5290202,_5290204):-freeze(_5290202,'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1_Interval.IV_3'(_5290194,_5290196,_5290198,_5290200,_5290202,_5290204)).
'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1_Interval.IV_3'('Interval.IV'(_5280856,_5280874),_5280710,_5280728,_5290598,_5290604,_5290610):-!,hnf('Prelude.&&'('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_5280710),_5280856),'Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_5280728),_5280874)),_5290598,_5290604,_5290610).
'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1_Interval.IV_3'('FAIL'(_5293878),_5280710,_5280728,'FAIL'(_5293878),_5293892,_5293892):-nonvar(_5293878).
'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23_1'('FAIL'(_5293964),_5280594,'FAIL'(_5293964),_5293978,_5293978):-nonvar(_5293964).

'Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.Interval\'23'(_5296156,_5296158,_5296160):-freeze(_5296158,'blocked_Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.Interval\'23'(_5296156,_5296158,_5296160)).
'blocked_Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.Interval\'23'(_5296328,_5296334,_5296340):-hnf(partcall(2,'Prelude._def\'23\'2F\'3D\'23Prelude.Eq',[partcall(1,'Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23',[])]),_5296328,_5296334,_5296340).

'Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23'(_5299446,_5299448,_5299450,_5299452):-freeze(_5299450,'blocked_Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23'(_5299446,_5299448,_5299450,_5299452)).
'blocked_Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23'(_5299532,_5300722,_5300728,_5300734):-hnf(_5299532,_5302936,_5300728,_5302948),'blocked_Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23_1'(_5302936,_5300722,_5302948,_5300734).

'blocked_Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23_1'(_5303496,_5303498,_5303500,_5303502):-freeze(_5303500,'blocked_blocked_Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23_1'(_5303496,_5303498,_5303500,_5303502)).
'blocked_blocked_Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23_1'('Prelude.()','Prelude._Dict\'23Ord'(partcall(1,'Interval._inst\'23Prelude.Eq\'23Interval.Interval\'23',[]),partcall(2,'Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23',[]),'Interval._impl\'23\'3C\'23Prelude.Ord\'23Interval.Interval\'23','Interval._impl\'23\'3E\'23Prelude.Ord\'23Interval.Interval\'23','Interval._impl\'23\'3C\'3D\'23Prelude.Ord\'23Interval.Interval\'23','Interval._impl\'23\'3E\'3D\'23Prelude.Ord\'23Interval.Interval\'23','Interval._impl\'23min\'23Prelude.Ord\'23Interval.Interval\'23','Interval._impl\'23max\'23Prelude.Ord\'23Interval.Interval\'23'),_5303848,_5303848):-!.
'blocked_blocked_Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23_1'('FAIL'(_5308166),'FAIL'(_5308166),_5308180,_5308180):-nonvar(_5308166).

'Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23'(_5310522,_5310524,_5310526,_5310528,_5310530):-freeze(_5310528,'blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23'(_5310522,_5310524,_5310526,_5310528,_5310530)).
'blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23'(_5310618,_5310636,_5312358,_5312364,_5312370):-hnf(_5310618,_5314940,_5312364,_5314958),'blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1'(_5314940,_5310636,_5312358,_5314958,_5312370).

'blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1'(_5315574,_5315576,_5315578,_5315580,_5315582):-freeze(_5315580,'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1'(_5315574,_5315576,_5315578,_5315580,_5315582)).
'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1'('Interval.IV'(_5310752,_5310770),_5310636,_5316488,_5316494,_5316500):-!,hnf(_5310636,_5319790,_5316494,_5319814),'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1_Interval.IV_3'(_5319790,_5310752,_5310770,_5316488,_5319814,_5316500).

'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1_Interval.IV_3'(_5320588,_5320590,_5320592,_5320594,_5320596,_5320598):-freeze(_5320596,'blocked_blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1_Interval.IV_3'(_5320588,_5320590,_5320592,_5320594,_5320596,_5320598)).
'blocked_blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1_Interval.IV_3'('Interval.IV'(_5310898,_5310916),_5310752,_5310770,_5320992,_5320998,_5321004):-!,makeShare(_5310956,_5321294),hnf('Prelude.cond'(letrec4PAKCS(_5321294,'Prelude.apply'('Prelude.apply'('Rational._impl\'23compare\'23Prelude.Ord\'23Rational.Rational\'23',_5310752),_5310898)),'Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0'(_5321294,_5310770,_5310916,_5321294)),_5320992,_5320998,_5321004).
'blocked_blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1_Interval.IV_3'('FAIL'(_5324804),_5310752,_5310770,'FAIL'(_5324804),_5324818,_5324818):-nonvar(_5324804).
'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23_1'('FAIL'(_5324890),_5310636,'FAIL'(_5324890),_5324904,_5324904):-nonvar(_5324890).

'Interval._impl\'23\'3C\'23Prelude.Ord\'23Interval.Interval\'23'(_5327054,_5327056,_5327058):-freeze(_5327056,'blocked_Interval._impl\'23\'3C\'23Prelude.Ord\'23Interval.Interval\'23'(_5327054,_5327056,_5327058)).
'blocked_Interval._impl\'23\'3C\'23Prelude.Ord\'23Interval.Interval\'23'(_5327226,_5327232,_5327238):-hnf(partcall(2,'Prelude._def\'23\'3C\'23Prelude.Ord',[partcall(1,'Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23',[])]),_5327226,_5327232,_5327238).

'Interval._impl\'23\'3E\'23Prelude.Ord\'23Interval.Interval\'23'(_5330458,_5330460,_5330462):-freeze(_5330460,'blocked_Interval._impl\'23\'3E\'23Prelude.Ord\'23Interval.Interval\'23'(_5330458,_5330460,_5330462)).
'blocked_Interval._impl\'23\'3E\'23Prelude.Ord\'23Interval.Interval\'23'(_5330630,_5330636,_5330642):-hnf(partcall(2,'Prelude._def\'23\'3E\'23Prelude.Ord',[partcall(1,'Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23',[])]),_5330630,_5330636,_5330642).

'Interval._impl\'23\'3C\'3D\'23Prelude.Ord\'23Interval.Interval\'23'(_5333928,_5333930,_5333932):-freeze(_5333930,'blocked_Interval._impl\'23\'3C\'3D\'23Prelude.Ord\'23Interval.Interval\'23'(_5333928,_5333930,_5333932)).
'blocked_Interval._impl\'23\'3C\'3D\'23Prelude.Ord\'23Interval.Interval\'23'(_5334100,_5334106,_5334112):-hnf(partcall(2,'Prelude._def\'23\'3C\'3D\'23Prelude.Ord',[partcall(1,'Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23',[])]),_5334100,_5334106,_5334112).

'Interval._impl\'23\'3E\'3D\'23Prelude.Ord\'23Interval.Interval\'23'(_5337434,_5337436,_5337438):-freeze(_5337436,'blocked_Interval._impl\'23\'3E\'3D\'23Prelude.Ord\'23Interval.Interval\'23'(_5337434,_5337436,_5337438)).
'blocked_Interval._impl\'23\'3E\'3D\'23Prelude.Ord\'23Interval.Interval\'23'(_5337606,_5337612,_5337618):-hnf(partcall(2,'Prelude._def\'23\'3E\'3D\'23Prelude.Ord',[partcall(1,'Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23',[])]),_5337606,_5337612,_5337618).

'Interval._impl\'23min\'23Prelude.Ord\'23Interval.Interval\'23'(_5340922,_5340924,_5340926):-freeze(_5340924,'blocked_Interval._impl\'23min\'23Prelude.Ord\'23Interval.Interval\'23'(_5340922,_5340924,_5340926)).
'blocked_Interval._impl\'23min\'23Prelude.Ord\'23Interval.Interval\'23'(_5341094,_5341100,_5341106):-hnf(partcall(2,'Prelude._def\'23min\'23Prelude.Ord',[partcall(1,'Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23',[])]),_5341094,_5341100,_5341106).

'Interval._impl\'23max\'23Prelude.Ord\'23Interval.Interval\'23'(_5344374,_5344376,_5344378):-freeze(_5344376,'blocked_Interval._impl\'23max\'23Prelude.Ord\'23Interval.Interval\'23'(_5344374,_5344376,_5344378)).
'blocked_Interval._impl\'23max\'23Prelude.Ord\'23Interval.Interval\'23'(_5344546,_5344552,_5344558):-hnf(partcall(2,'Prelude._def\'23max\'23Prelude.Ord',[partcall(1,'Interval._inst\'23Prelude.Ord\'23Interval.Interval\'23',[])]),_5344546,_5344552,_5344558).

'Interval._inst\'23Prelude.Show\'23Interval.Interval\'23'(_5347684,_5347686,_5347688,_5347690):-freeze(_5347688,'blocked_Interval._inst\'23Prelude.Show\'23Interval.Interval\'23'(_5347684,_5347686,_5347688,_5347690)).
'blocked_Interval._inst\'23Prelude.Show\'23Interval.Interval\'23'(_5347770,_5348546,_5348552,_5348558):-hnf(_5347770,_5350796,_5348552,_5350808),'blocked_Interval._inst\'23Prelude.Show\'23Interval.Interval\'23_1'(_5350796,_5348546,_5350808,_5348558).

'blocked_Interval._inst\'23Prelude.Show\'23Interval.Interval\'23_1'(_5351362,_5351364,_5351366,_5351368):-freeze(_5351366,'blocked_blocked_Interval._inst\'23Prelude.Show\'23Interval.Interval\'23_1'(_5351362,_5351364,_5351366,_5351368)).
'blocked_blocked_Interval._inst\'23Prelude.Show\'23Interval.Interval\'23_1'('Prelude.()','Prelude._Dict\'23Show'('Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23','Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.Interval\'23','Interval._impl\'23showList\'23Prelude.Show\'23Interval.Interval\'23'),_5351714,_5351714):-!.
'blocked_blocked_Interval._inst\'23Prelude.Show\'23Interval.Interval\'23_1'('FAIL'(_5353838),'FAIL'(_5353838),_5353852,_5353852):-nonvar(_5353838).

'Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23'(_5356118,_5356120,_5356122):-freeze(_5356120,'blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23'(_5356118,_5356120,_5356122)).
'blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23'(_5356206,_5356212,_5356218):-hnf(partcall(1,'Interval.showInterval',[]),_5356206,_5356212,_5356218).

'Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.Interval\'23'(_5359268,_5359270,_5359272):-freeze(_5359270,'blocked_Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.Interval\'23'(_5359268,_5359270,_5359272)).
'blocked_Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.Interval\'23'(_5359440,_5359446,_5359452):-hnf(partcall(3,'Prelude._def\'23showsPrec\'23Prelude.Show',[partcall(1,'Interval._inst\'23Prelude.Show\'23Interval.Interval\'23',[])]),_5359440,_5359446,_5359452).

'Interval._impl\'23showList\'23Prelude.Show\'23Interval.Interval\'23'(_5363038,_5363040,_5363042):-freeze(_5363040,'blocked_Interval._impl\'23showList\'23Prelude.Show\'23Interval.Interval\'23'(_5363038,_5363040,_5363042)).
'blocked_Interval._impl\'23showList\'23Prelude.Show\'23Interval.Interval\'23'(_5363210,_5363216,_5363222):-hnf('Prelude._def\'23showList\'23Prelude.Show'(partcall(1,'Interval._inst\'23Prelude.Show\'23Interval.Interval\'23',[])),_5363210,_5363216,_5363222).

'Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5366610,_5366612,_5366614,_5366616):-freeze(_5366614,'blocked_Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5366610,_5366612,_5366614,_5366616)).
'blocked_Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5366696,_5367418,_5367424,_5367430):-hnf(_5366696,_5369848,_5367424,_5369860),'blocked_Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'(_5369848,_5367418,_5369860,_5367430).

'blocked_Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'(_5370444,_5370446,_5370448,_5370450):-freeze(_5370448,'blocked_blocked_Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'(_5370444,_5370446,_5370448,_5370450)).
'blocked_blocked_Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'('Prelude.()','Prelude._Dict\'23Eq'(partcall(2,'Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23',[]),'Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23'),_5370796,_5370796):-!.
'blocked_blocked_Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'('FAIL'(_5372506),'FAIL'(_5372506),_5372520,_5372520):-nonvar(_5372506).

'Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5374956,_5374958,_5374960,_5374962,_5374964):-freeze(_5374962,'blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5374956,_5374958,_5374960,_5374962,_5374964)).
'blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5375052,_5375070,_5376326,_5376332,_5376338):-hnf(_5375052,_5379088,_5376332,_5379106),'blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'(_5379088,_5375070,_5376326,_5379106,_5376338).

'blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'(_5379752,_5379754,_5379756,_5379758,_5379760):-freeze(_5379758,'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'(_5379752,_5379754,_5379756,_5379758,_5379760)).
'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'('Interval.CI'(_5375186,_5375204),_5375070,_5380696,_5380702,_5380708):-!,hnf(_5375070,_5384178,_5380702,_5384202),'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1_Interval.CI_3'(_5384178,_5375186,_5375204,_5380696,_5384202,_5380708).

'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1_Interval.CI_3'(_5385006,_5385008,_5385010,_5385012,_5385014,_5385016):-freeze(_5385014,'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1_Interval.CI_3'(_5385006,_5385008,_5385010,_5385012,_5385014,_5385016)).
'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1_Interval.CI_3'('Interval.CI'(_5375332,_5375350),_5375186,_5375204,_5385410,_5385416,_5385422):-!,hnf('Prelude.&&'('Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23'(_5375186,_5375332),'Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.Interval\'23'(_5375204,_5375350)),_5385410,_5385416,_5385422).
'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1_Interval.CI_3'('FAIL'(_5387984),_5375186,_5375204,'FAIL'(_5387984),_5387998,_5387998):-nonvar(_5387984).
'blocked_blocked_Interval._impl\'23\'3D\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23_1'('FAIL'(_5388070),_5375070,'FAIL'(_5388070),_5388084,_5388084):-nonvar(_5388070).

'Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5390528,_5390530,_5390532):-freeze(_5390530,'blocked_Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5390528,_5390530,_5390532)).
'blocked_Interval._impl\'23\'2F\'3D\'23Prelude.Eq\'23Interval.ComplexInterval\'23'(_5390700,_5390706,_5390712):-hnf(partcall(2,'Prelude._def\'23\'2F\'3D\'23Prelude.Eq',[partcall(1,'Interval._inst\'23Prelude.Eq\'23Interval.ComplexInterval\'23',[])]),_5390700,_5390706,_5390712).

'Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5394206,_5394208,_5394210,_5394212):-freeze(_5394210,'blocked_Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5394206,_5394208,_5394210,_5394212)).
'blocked_Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5394292,_5395110,_5395116,_5395122):-hnf(_5394292,_5397612,_5395116,_5397624),'blocked_Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'(_5397612,_5395110,_5397624,_5395122).

'blocked_Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'(_5398220,_5398222,_5398224,_5398226):-freeze(_5398224,'blocked_blocked_Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'(_5398220,_5398222,_5398224,_5398226)).
'blocked_blocked_Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'('Prelude.()','Prelude._Dict\'23Show'(partcall(1,'Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23',[]),'Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.ComplexInterval\'23','Interval._impl\'23showList\'23Prelude.Show\'23Interval.ComplexInterval\'23'),_5398572,_5398572):-!.
'blocked_blocked_Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'('FAIL'(_5400868),'FAIL'(_5400868),_5400882,_5400882):-nonvar(_5400868).

'Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5403414,_5403416,_5403418,_5403420):-freeze(_5403418,'blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5403414,_5403416,_5403418,_5403420)).
'blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5403500,_5405570,_5405576,_5405582):-hnf(_5403500,_5408324,_5405576,_5408336),'blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'(_5408324,_5405570,_5408336,_5405582).

'blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'(_5408974,_5408976,_5408978,_5408980):-freeze(_5408978,'blocked_blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'(_5408974,_5408976,_5408978,_5408980)).
'blocked_blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'('Interval.CI'(_5403616,_5403634),_5409358,_5409364,_5409370):-!,hnf('Prelude.++'('Prelude.apply'('Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23',_5403616),'Prelude.++'(['^ ',^+,'^ ','^i',^*],'Prelude.apply'('Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23',_5403634))),_5409358,_5409364,_5409370).
'blocked_blocked_Interval._impl\'23show\'23Prelude.Show\'23Interval.ComplexInterval\'23_1'('FAIL'(_5413128),'FAIL'(_5413128),_5413142,_5413142):-nonvar(_5413128).

'Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5415864,_5415866,_5415868):-freeze(_5415866,'blocked_Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5415864,_5415866,_5415868)).
'blocked_Interval._impl\'23showsPrec\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5416036,_5416042,_5416048):-hnf(partcall(3,'Prelude._def\'23showsPrec\'23Prelude.Show',[partcall(1,'Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23',[])]),_5416036,_5416042,_5416048).

'Interval._impl\'23showList\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5419984,_5419986,_5419988):-freeze(_5419986,'blocked_Interval._impl\'23showList\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5419984,_5419986,_5419988)).
'blocked_Interval._impl\'23showList\'23Prelude.Show\'23Interval.ComplexInterval\'23'(_5420156,_5420162,_5420168):-hnf('Prelude._def\'23showList\'23Prelude.Show'(partcall(1,'Interval._inst\'23Prelude.Show\'23Interval.ComplexInterval\'23',[])),_5420156,_5420162,_5420168).

'Interval._inst\'23Prelude.Data\'23Interval.Interval\'23'(_5423450,_5423452,_5423454,_5423456):-freeze(_5423454,'blocked_Interval._inst\'23Prelude.Data\'23Interval.Interval\'23'(_5423450,_5423452,_5423454,_5423456)).
'blocked_Interval._inst\'23Prelude.Data\'23Interval.Interval\'23'(_5423536,_5424228,_5424234,_5424240):-hnf(_5423536,_5426478,_5424234,_5426490),'blocked_Interval._inst\'23Prelude.Data\'23Interval.Interval\'23_1'(_5426478,_5424228,_5426490,_5424240).

'blocked_Interval._inst\'23Prelude.Data\'23Interval.Interval\'23_1'(_5427044,_5427046,_5427048,_5427050):-freeze(_5427048,'blocked_blocked_Interval._inst\'23Prelude.Data\'23Interval.Interval\'23_1'(_5427044,_5427046,_5427048,_5427050)).
'blocked_blocked_Interval._inst\'23Prelude.Data\'23Interval.Interval\'23_1'('Prelude.()','Prelude._Dict\'23Data'(partcall(2,'Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23',[]),'Interval._impl\'23aValue\'23Prelude.Data\'23Interval.Interval\'23'),_5427396,_5427396):-!.
'blocked_blocked_Interval._inst\'23Prelude.Data\'23Interval.Interval\'23_1'('FAIL'(_5429046),'FAIL'(_5429046),_5429060,_5429060):-nonvar(_5429046).

'Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23'(_5431372,_5431374,_5431376,_5431378,_5431380):-freeze(_5431378,'blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23'(_5431372,_5431374,_5431376,_5431378,_5431380)).
'blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23'(_5431468,_5431486,_5432730,_5432736,_5432742):-hnf(_5431468,_5435420,_5432736,_5435438),'blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1'(_5435420,_5431486,_5432730,_5435438,_5432742).

'blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1'(_5436072,_5436074,_5436076,_5436078,_5436080):-freeze(_5436078,'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1'(_5436072,_5436074,_5436076,_5436078,_5436080)).
'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1'('Interval.IV'(_5431602,_5431620),_5431486,_5437004,_5437010,_5437016):-!,hnf(_5431486,_5440414,_5437010,_5440438),'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1_Interval.IV_3'(_5440414,_5431602,_5431620,_5437004,_5440438,_5437016).

'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1_Interval.IV_3'(_5441230,_5441232,_5441234,_5441236,_5441238,_5441240):-freeze(_5441238,'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1_Interval.IV_3'(_5441230,_5441232,_5441234,_5441236,_5441238,_5441240)).
'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1_Interval.IV_3'('Interval.IV'(_5431748,_5431766),_5431602,_5431620,_5441634,_5441640,_5441646):-!,hnf('Prelude.&&'('Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23'(_5431602,_5431748),'Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23'(_5431620,_5431766)),_5441634,_5441640,_5441646).
'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1_Interval.IV_3'('FAIL'(_5444256),_5431602,_5431620,'FAIL'(_5444256),_5444270,_5444270):-nonvar(_5444256).
'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23_1'('FAIL'(_5444342),_5431486,'FAIL'(_5444342),_5444356,_5444356):-nonvar(_5444342).

'Interval._impl\'23aValue\'23Prelude.Data\'23Interval.Interval\'23'(_5446706,_5446708,_5446710):-freeze(_5446708,'blocked_Interval._impl\'23aValue\'23Prelude.Data\'23Interval.Interval\'23'(_5446706,_5446708,_5446710)).
'blocked_Interval._impl\'23aValue\'23Prelude.Data\'23Interval.Interval\'23'('Interval.IV'('Rational._impl\'23aValue\'23Prelude.Data\'23Rational.Rational\'23','Rational._impl\'23aValue\'23Prelude.Data\'23Rational.Rational\'23'),_5446968,_5446968).

'Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23'(_5450770,_5450772,_5450774,_5450776):-freeze(_5450774,'blocked_Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23'(_5450770,_5450772,_5450774,_5450776)).
'blocked_Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23'(_5450856,_5451590,_5451596,_5451602):-hnf(_5450856,_5454092,_5451596,_5454104),'blocked_Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'(_5454092,_5451590,_5454104,_5451602).

'blocked_Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'(_5454700,_5454702,_5454704,_5454706):-freeze(_5454704,'blocked_blocked_Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'(_5454700,_5454702,_5454704,_5454706)).
'blocked_blocked_Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'('Prelude.()','Prelude._Dict\'23Data'(partcall(2,'Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23',[]),'Interval._impl\'23aValue\'23Prelude.Data\'23Interval.ComplexInterval\'23'),_5455052,_5455052):-!.
'blocked_blocked_Interval._inst\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'('FAIL'(_5456828),'FAIL'(_5456828),_5456842,_5456842):-nonvar(_5456828).

'Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23'(_5459420,_5459422,_5459424,_5459426,_5459428):-freeze(_5459426,'blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23'(_5459420,_5459422,_5459424,_5459426,_5459428)).
'blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23'(_5459516,_5459534,_5460820,_5460826,_5460832):-hnf(_5459516,_5463762,_5460826,_5463780),'blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'(_5463762,_5459534,_5460820,_5463780,_5460832).

'blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'(_5464456,_5464458,_5464460,_5464462,_5464464):-freeze(_5464462,'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'(_5464456,_5464458,_5464460,_5464462,_5464464)).
'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'('Interval.CI'(_5459650,_5459668),_5459534,_5465430,_5465436,_5465442):-!,hnf(_5459534,_5469092,_5465436,_5469116),'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1_Interval.CI_3'(_5469092,_5459650,_5459668,_5465430,_5469116,_5465442).

'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1_Interval.CI_3'(_5469950,_5469952,_5469954,_5469956,_5469958,_5469960):-freeze(_5469958,'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1_Interval.CI_3'(_5469950,_5469952,_5469954,_5469956,_5469958,_5469960)).
'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1_Interval.CI_3'('Interval.CI'(_5459796,_5459814),_5459650,_5459668,_5470354,_5470360,_5470366):-!,hnf('Prelude.&&'('Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23'(_5459650,_5459796),'Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.Interval\'23'(_5459668,_5459814)),_5470354,_5470360,_5470366).
'blocked_blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1_Interval.CI_3'('FAIL'(_5473018),_5459650,_5459668,'FAIL'(_5473018),_5473032,_5473032):-nonvar(_5473018).
'blocked_blocked_Interval._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Interval.ComplexInterval\'23_1'('FAIL'(_5473104),_5459534,'FAIL'(_5473104),_5473118,_5473118):-nonvar(_5473104).

'Interval._impl\'23aValue\'23Prelude.Data\'23Interval.ComplexInterval\'23'(_5475734,_5475736,_5475738):-freeze(_5475736,'blocked_Interval._impl\'23aValue\'23Prelude.Data\'23Interval.ComplexInterval\'23'(_5475734,_5475736,_5475738)).
'blocked_Interval._impl\'23aValue\'23Prelude.Data\'23Interval.ComplexInterval\'23'('Interval.CI'('Interval._impl\'23aValue\'23Prelude.Data\'23Interval.Interval\'23','Interval._impl\'23aValue\'23Prelude.Data\'23Interval.Interval\'23'),_5475996,_5475996).

'Interval.ri'(_5478128,_5478130,_5478132):-freeze(_5478130,'blocked_Interval.ri'(_5478128,_5478130,_5478132)).
'blocked_Interval.ri'(_5478216,_5478222,_5478228):-hnf(partcall(1,'Rational.fromInt',[]),_5478216,_5478222,_5478228).

'Interval.midpoint'(_5479334,_5479336,_5479338,_5479340):-freeze(_5479338,'blocked_Interval.midpoint'(_5479334,_5479336,_5479338,_5479340)).
'blocked_Interval.midpoint'(_5479420,_5480244,_5480250,_5480256):-hnf(_5479420,_5481234,_5480250,_5481246),'blocked_Interval.midpoint_1'(_5481234,_5480244,_5481246,_5480256).

'blocked_Interval.midpoint_1'(_5481590,_5481592,_5481594,_5481596):-freeze(_5481594,'blocked_blocked_Interval.midpoint_1'(_5481590,_5481592,_5481594,_5481596)).
'blocked_blocked_Interval.midpoint_1'('Interval.IV'(_5479536,_5479554),_5481974,_5481980,_5481986):-!,hnf('Rational.ratDiv'('Rational.ratAdd'(_5479536,_5479554),'Prelude.apply'('Interval.ri',2)),_5481974,_5481980,_5481986).
'blocked_blocked_Interval.midpoint_1'('FAIL'(_5483402),'FAIL'(_5483402),_5483416,_5483416):-nonvar(_5483402).

'Interval.width'(_5484132,_5484134,_5484136,_5484138):-freeze(_5484136,'blocked_Interval.width'(_5484132,_5484134,_5484136,_5484138)).
'blocked_Interval.width'(_5484218,_5484702,_5484708,_5484714):-hnf(_5484218,_5485584,_5484708,_5485596),'blocked_Interval.width_1'(_5485584,_5484702,_5485596,_5484714).

'blocked_Interval.width_1'(_5485922,_5485924,_5485926,_5485928):-freeze(_5485926,'blocked_blocked_Interval.width_1'(_5485922,_5485924,_5485926,_5485928)).
'blocked_blocked_Interval.width_1'('Interval.IV'(_5484334,_5484352),_5486306,_5486312,_5486318):-!,hnf('Rational.ratSub'(_5484352,_5484334),_5486306,_5486312,_5486318).
'blocked_blocked_Interval.width_1'('FAIL'(_5487074),'FAIL'(_5487074),_5487088,_5487088):-nonvar(_5487074).

'Interval.ivContains'(_5487994,_5487996,_5487998,_5488000,_5488002):-freeze(_5488000,'blocked_Interval.ivContains'(_5487994,_5487996,_5487998,_5488000,_5488002)).
'blocked_Interval.ivContains'(_5488090,_5488108,_5488930,_5488936,_5488942):-hnf(_5488090,_5490000,_5488936,_5490018),'blocked_Interval.ivContains_1'(_5490000,_5488108,_5488930,_5490018,_5488942).

'blocked_Interval.ivContains_1'(_5490382,_5490384,_5490386,_5490388,_5490390):-freeze(_5490388,'blocked_blocked_Interval.ivContains_1'(_5490382,_5490384,_5490386,_5490388,_5490390)).
'blocked_blocked_Interval.ivContains_1'('Interval.IV'(_5488224,_5488242),_5488108,_5490776,_5490782,_5490788):-!,makeShare(_5488108,_5490950),hnf('Prelude.&&'('Rational.ratLe'(_5488224,_5490950),'Rational.ratLe'(_5490950,_5488242)),_5490776,_5490782,_5490788).
'blocked_blocked_Interval.ivContains_1'('FAIL'(_5492528),_5488108,'FAIL'(_5492528),_5492542,_5492542):-nonvar(_5492528).

'Interval.overlaps'(_5493380,_5493382,_5493384,_5493386,_5493388):-freeze(_5493386,'blocked_Interval.overlaps'(_5493380,_5493382,_5493384,_5493386,_5493388)).
'blocked_Interval.overlaps'(_5493476,_5493494,_5494456,_5494462,_5494468):-hnf(_5493476,_5495454,_5494462,_5495472),'blocked_Interval.overlaps_1'(_5495454,_5493494,_5494456,_5495472,_5494468).

'blocked_Interval.overlaps_1'(_5495824,_5495826,_5495828,_5495830,_5495832):-freeze(_5495830,'blocked_blocked_Interval.overlaps_1'(_5495824,_5495826,_5495828,_5495830,_5495832)).
'blocked_blocked_Interval.overlaps_1'('Interval.IV'(_5493610,_5493628),_5493494,_5496474,_5496480,_5496486):-!,hnf(_5493494,_5498192,_5496480,_5498216),'blocked_blocked_Interval.overlaps_1_Interval.IV_3'(_5498192,_5493610,_5493628,_5496474,_5498216,_5496486).

'blocked_blocked_Interval.overlaps_1_Interval.IV_3'(_5498726,_5498728,_5498730,_5498732,_5498734,_5498736):-freeze(_5498734,'blocked_blocked_blocked_Interval.overlaps_1_Interval.IV_3'(_5498726,_5498728,_5498730,_5498732,_5498734,_5498736)).
'blocked_blocked_blocked_Interval.overlaps_1_Interval.IV_3'('Interval.IV'(_5493756,_5493774),_5493610,_5493628,_5499130,_5499136,_5499142):-!,hnf('Prelude.&&'('Rational.ratLe'(_5493610,_5493774),'Rational.ratLe'(_5493756,_5493628)),_5499130,_5499136,_5499142).
'blocked_blocked_blocked_Interval.overlaps_1_Interval.IV_3'('FAIL'(_5500862),_5493610,_5493628,'FAIL'(_5500862),_5500876,_5500876):-nonvar(_5500862).
'blocked_blocked_Interval.overlaps_1'('FAIL'(_5500948),_5493494,'FAIL'(_5500948),_5500962,_5500962):-nonvar(_5500948).

'Interval.bisect'(_5501724,_5501726,_5501728,_5501730):-freeze(_5501728,'blocked_Interval.bisect'(_5501724,_5501726,_5501728,_5501730)).
'blocked_Interval.bisect'(_5501810,_5503636,_5503642,_5503648):-makeShare(_5501810,_5502868),hnf(_5502868,_5504554,_5503642,_5504572),'blocked_Interval.bisect_1'(_5504554,_5504554,_5503636,_5504572,_5503648).

'blocked_Interval.bisect_1'(_5504924,_5504926,_5504928,_5504930,_5504932):-freeze(_5504930,'blocked_blocked_Interval.bisect_1'(_5504924,_5504926,_5504928,_5504930,_5504932)).
'blocked_blocked_Interval.bisect_1'('Interval.IV'(_5501926,_5501944),_5502868,_5505318,_5505324,_5505330):-!,makeShare(_5501984,_5505550),hnf('Prelude.cond'(letrec4PAKCS(_5505550,'Interval.midpoint'(_5502868)),'Prelude.(,)'('Interval.IV'(_5501926,_5505550),'Interval.IV'(_5505550,_5501944))),_5505318,_5505324,_5505330).
'blocked_blocked_Interval.bisect_1'('FAIL'(_5507822),_5502868,'FAIL'(_5507822),_5507836,_5507836):-nonvar(_5507822).

'Interval.refine'(_5508598,_5508600,_5508602,_5508604,_5508606):-freeze(_5508604,'blocked_Interval.refine'(_5508598,_5508600,_5508602,_5508604,_5508606)).
'blocked_Interval.refine'(_5508694,_5508712,_5510364,_5510370,_5510376):-makeShare(_5508740,_5510794),makeShare(_5508712,_5510814),makeShare(_5508758,_5510834),makeShare(_5508776,_5510854),hnf('Prelude.cond'(letrec4PAKCS(_5510794,'Interval.bisect'(_5510814)),'Prelude.cond'(letrec4PAKCS(_5510834,'Interval.refine._\'23selFP2\'23left'(_5510794)),'Prelude.cond'(letrec4PAKCS(_5510854,'Interval.midpoint'(_5510814)),'Interval.refine._\'23caseor0'('Prelude.apply'(_5508694,_5510854),_5510834,_5510814)))),_5510364,_5510370,_5510376).

'Interval.refine._\'23selFP2\'23left'(_5516528,_5516530,_5516532,_5516534):-freeze(_5516532,'blocked_Interval.refine._\'23selFP2\'23left'(_5516528,_5516530,_5516532,_5516534)).
'blocked_Interval.refine._\'23selFP2\'23left'(_5516614,_5517058,_5517064,_5517070):-hnf(_5516614,_5518624,_5517064,_5518636),'blocked_Interval.refine._\'23selFP2\'23left_1'(_5518624,_5517058,_5518636,_5517070).

'blocked_Interval.refine._\'23selFP2\'23left_1'(_5519076,_5519078,_5519080,_5519082):-freeze(_5519080,'blocked_blocked_Interval.refine._\'23selFP2\'23left_1'(_5519076,_5519078,_5519080,_5519082)).
'blocked_blocked_Interval.refine._\'23selFP2\'23left_1'('Prelude.(,)'(_5516730,_5516748),_5519446,_5519452,_5519458):-!,hnf(_5516730,_5519446,_5519452,_5519458).
'blocked_blocked_Interval.refine._\'23selFP2\'23left_1'('FAIL'(_5520024),'FAIL'(_5520024),_5520038,_5520038):-nonvar(_5520024).

'Interval.fromRat'(_5520830,_5520832,_5520834,_5520836):-freeze(_5520834,'blocked_Interval.fromRat'(_5520830,_5520832,_5520834,_5520836)).
'blocked_Interval.fromRat'(_5520916,'Interval.IV'(_5521172,_5521172),_5521092,_5521098):-makeShare(_5520916,_5521172),_5521092=_5521098.

'Interval.iadd'(_5522542,_5522544,_5522546,_5522548,_5522550):-freeze(_5522548,'blocked_Interval.iadd'(_5522542,_5522544,_5522546,_5522548,_5522550)).
'blocked_Interval.iadd'(_5522638,_5522656,_5523594,_5523600,_5523606):-hnf(_5522638,_5524448,_5523600,_5524466),'blocked_Interval.iadd_1'(_5524448,_5522656,_5523594,_5524466,_5523606).

'blocked_Interval.iadd_1'(_5524794,_5524796,_5524798,_5524800,_5524802):-freeze(_5524800,'blocked_blocked_Interval.iadd_1'(_5524794,_5524796,_5524798,_5524800,_5524802)).
'blocked_blocked_Interval.iadd_1'('Interval.IV'(_5522772,_5522790),_5522656,_5525420,_5525426,_5525432):-!,hnf(_5522656,_5526994,_5525426,_5527018),'blocked_blocked_Interval.iadd_1_Interval.IV_3'(_5526994,_5522772,_5522790,_5525420,_5527018,_5525432).

'blocked_blocked_Interval.iadd_1_Interval.IV_3'(_5527504,_5527506,_5527508,_5527510,_5527512,_5527514):-freeze(_5527512,'blocked_blocked_blocked_Interval.iadd_1_Interval.IV_3'(_5527504,_5527506,_5527508,_5527510,_5527512,_5527514)).
'blocked_blocked_blocked_Interval.iadd_1_Interval.IV_3'('Interval.IV'(_5522918,_5522936),_5522772,_5522790,'Interval.IV'('Rational.ratAdd'(_5522772,_5522918),'Rational.ratAdd'(_5522790,_5522936)),_5527914,_5527914):-!.
'blocked_blocked_blocked_Interval.iadd_1_Interval.IV_3'('FAIL'(_5529546),_5522772,_5522790,'FAIL'(_5529546),_5529560,_5529560):-nonvar(_5529546).
'blocked_blocked_Interval.iadd_1'('FAIL'(_5529632),_5522656,'FAIL'(_5529632),_5529646,_5529646):-nonvar(_5529632).

'Interval.isub'(_5530332,_5530334,_5530336,_5530338,_5530340):-freeze(_5530338,'blocked_Interval.isub'(_5530332,_5530334,_5530336,_5530338,_5530340)).
'blocked_Interval.isub'(_5530428,_5530446,_5531384,_5531390,_5531396):-hnf(_5530428,_5532238,_5531390,_5532256),'blocked_Interval.isub_1'(_5532238,_5530446,_5531384,_5532256,_5531396).

'blocked_Interval.isub_1'(_5532584,_5532586,_5532588,_5532590,_5532592):-freeze(_5532590,'blocked_blocked_Interval.isub_1'(_5532584,_5532586,_5532588,_5532590,_5532592)).
'blocked_blocked_Interval.isub_1'('Interval.IV'(_5530562,_5530580),_5530446,_5533210,_5533216,_5533222):-!,hnf(_5530446,_5534784,_5533216,_5534808),'blocked_blocked_Interval.isub_1_Interval.IV_3'(_5534784,_5530562,_5530580,_5533210,_5534808,_5533222).

'blocked_blocked_Interval.isub_1_Interval.IV_3'(_5535294,_5535296,_5535298,_5535300,_5535302,_5535304):-freeze(_5535302,'blocked_blocked_blocked_Interval.isub_1_Interval.IV_3'(_5535294,_5535296,_5535298,_5535300,_5535302,_5535304)).
'blocked_blocked_blocked_Interval.isub_1_Interval.IV_3'('Interval.IV'(_5530708,_5530726),_5530562,_5530580,'Interval.IV'('Rational.ratSub'(_5530562,_5530726),'Rational.ratSub'(_5530580,_5530708)),_5535704,_5535704):-!.
'blocked_blocked_blocked_Interval.isub_1_Interval.IV_3'('FAIL'(_5537336),_5530562,_5530580,'FAIL'(_5537336),_5537350,_5537350):-nonvar(_5537336).
'blocked_blocked_Interval.isub_1'('FAIL'(_5537422),_5530446,'FAIL'(_5537422),_5537436,_5537436):-nonvar(_5537422).

'Interval.imul'(_5538122,_5538124,_5538126,_5538128,_5538130):-freeze(_5538128,'blocked_Interval.imul'(_5538122,_5538124,_5538126,_5538128,_5538130)).
'blocked_Interval.imul'(_5538218,_5538236,_5540790,_5540796,_5540802):-hnf(_5538218,_5541644,_5540796,_5541662),'blocked_Interval.imul_1'(_5541644,_5538236,_5540790,_5541662,_5540802).

'blocked_Interval.imul_1'(_5541990,_5541992,_5541994,_5541996,_5541998):-freeze(_5541996,'blocked_blocked_Interval.imul_1'(_5541990,_5541992,_5541994,_5541996,_5541998)).
'blocked_blocked_Interval.imul_1'('Interval.IV'(_5538352,_5538370),_5538236,_5542616,_5542622,_5542628):-!,hnf(_5538236,_5544190,_5542622,_5544214),'blocked_blocked_Interval.imul_1_Interval.IV_3'(_5544190,_5538352,_5538370,_5542616,_5544214,_5542628).

'blocked_blocked_Interval.imul_1_Interval.IV_3'(_5544700,_5544702,_5544704,_5544706,_5544708,_5544710):-freeze(_5544708,'blocked_blocked_blocked_Interval.imul_1_Interval.IV_3'(_5544700,_5544702,_5544704,_5544706,_5544708,_5544710)).
'blocked_blocked_blocked_Interval.imul_1_Interval.IV_3'('Interval.IV'(_5538498,_5538516),_5538352,_5538370,_5545104,_5545110,_5545116):-!,makeShare(_5538556,_5545602),makeShare(_5538352,_5545622),makeShare(_5538498,_5545642),makeShare(_5538516,_5545662),makeShare(_5538370,_5545682),hnf('Prelude.cond'(letrec4PAKCS(_5545602,['Rational.ratMul'(_5545622,_5545642),'Rational.ratMul'(_5545622,_5545662),'Rational.ratMul'(_5545682,_5545642),'Rational.ratMul'(_5545682,_5545662)]),'Interval.IV'('Prelude.foldl1'(partcall(2,'Rational.ratMin',[]),_5545602),'Prelude.foldl1'(partcall(2,'Rational.ratMax',[]),_5545602))),_5545104,_5545110,_5545116).
'blocked_blocked_blocked_Interval.imul_1_Interval.IV_3'('FAIL'(_5550974),_5538352,_5538370,'FAIL'(_5550974),_5550988,_5550988):-nonvar(_5550974).
'blocked_blocked_Interval.imul_1'('FAIL'(_5551060),_5538236,'FAIL'(_5551060),_5551074,_5551074):-nonvar(_5551060).

'Interval.iinv'(_5551760,_5551762,_5551764,_5551766):-freeze(_5551764,'blocked_Interval.iinv'(_5551760,_5551762,_5551764,_5551766)).
'blocked_Interval.iinv'(_5551846,_5558920,_5558926,_5558932):-hnf(_5551846,_5559766,_5558926,_5559778),'blocked_Interval.iinv_1'(_5559766,_5558920,_5559778,_5558932).

'blocked_Interval.iinv_1'(_5560098,_5560100,_5560102,_5560104):-freeze(_5560102,'blocked_blocked_Interval.iinv_1'(_5560098,_5560100,_5560102,_5560104)).
'blocked_blocked_Interval.iinv_1'('Interval.IV'(_5551962,_5551980),_5564342,_5564348,_5564354):-!,makeShare(_5551962,_5560762),makeShare(_5551980,_5560782),hnf('Prelude.||'('Rational.ratGt'(_5560762,'Prelude.apply'('Interval.ri',0)),'Rational.ratLt'(_5560782,'Prelude.apply'('Interval.ri',0))),_5566010,_5564348,_5565976),'blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase'(_5566010,_5560762,_5560782,_5564342,_5565976,_5564354).

'blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase'(_5566552,_5566554,_5566556,_5566558,_5566560,_5566562):-freeze(_5566560,freeze(_5566552,'blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase'(_5566552,_5566554,_5566556,_5566558,_5566560,_5566562))).
'blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase'('Prelude.True',_5560762,_5560782,'Interval.IV'('Rational.ratDiv'('Prelude.apply'('Interval.ri',1),_5560782),'Rational.ratDiv'('Prelude.apply'('Interval.ri',1),_5560762)),_5566962,_5566962).
'blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase'('Prelude.False',_5560762,_5560782,_5571192,_5571198,_5571204):-!,hnf('Prelude.otherwise',_5574088,_5571198,_5574054),'blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5574088,_5560762,_5560782,_5571192,_5574054,_5571204).

'blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5574810,_5574812,_5574814,_5574816,_5574818,_5574820):-freeze(_5574818,freeze(_5574810,'blocked_blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5574810,_5574812,_5574814,_5574816,_5574818,_5574820))).
'blocked_blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5560762,_5560782,_5575214,_5575220,_5575226):-hnf('Prelude.error'(['^i','^i','^n','^v',^:,'^ ','^i','^n','^t','^e','^r','^v','^a','^l','^ ','^c','^o','^n','^t','^a','^i','^n','^s','^ ','^z','^e','^r','^o']),_5575214,_5575220,_5575226).
'blocked_blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5560762,_5560782,_5582232,_5582238,_5582244):-!,hnf(reportFailure4PAKCS('Interval.iinv',['Prelude.False']),_5582232,_5582238,_5582244).
'blocked_blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5583818),_5560762,_5560782,'FAIL'(_5583818),_5583832,_5583832).
'blocked_blocked_blocked_Interval.iinv_1_Interval.IV_ComplexCase'('FAIL'(_5583900),_5560762,_5560782,'FAIL'(_5583900),_5583914,_5583914).
'blocked_blocked_Interval.iinv_1'('FAIL'(_5583982),'FAIL'(_5583982),_5583996,_5583996):-nonvar(_5583982).

'Interval.idiv'(_5584674,_5584676,_5584678,_5584680,_5584682):-freeze(_5584680,'blocked_Interval.idiv'(_5584674,_5584676,_5584678,_5584680,_5584682)).
'blocked_Interval.idiv'(_5584770,_5584788,_5585042,_5585048,_5585054):-hnf('Interval.imul'(_5584770,'Interval.iinv'(_5584788)),_5585042,_5585048,_5585054).

'Interval.ipow'(_5586532,_5586534,_5586536,_5586538,_5586540):-freeze(_5586538,'blocked_Interval.ipow'(_5586532,_5586534,_5586536,_5586538,_5586540)).
'blocked_Interval.ipow'(_5586628,_5586646,_5597118,_5597124,_5597130):-makeShare(_5586646,_5594208),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5594208,0),_5598074,_5597124,_5598040),'blocked_Interval.ipow_ComplexCase'(_5598074,_5586628,_5594208,_5597118,_5598040,_5597130).

'blocked_Interval.ipow_ComplexCase'(_5598454,_5598456,_5598458,_5598460,_5598462,_5598464):-freeze(_5598462,freeze(_5598454,'blocked_blocked_Interval.ipow_ComplexCase'(_5598454,_5598456,_5598458,_5598460,_5598462,_5598464))).
'blocked_blocked_Interval.ipow_ComplexCase'('Prelude.True',_5586628,_5594208,'Interval.IV'('Prelude.apply'('Interval.ri',1),'Prelude.apply'('Interval.ri',1)),_5598864,_5598864).
'blocked_blocked_Interval.ipow_ComplexCase'('Prelude.False',_5586628,_5594208,_5604742,_5604748,_5604754):-!,makeShare(_5594208,_5601656),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_5601656),0),_5606846,_5604748,_5606812),'blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase'(_5606846,_5586628,_5601656,_5604742,_5606812,_5604754).

'blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase'(_5607448,_5607450,_5607452,_5607454,_5607456,_5607458):-freeze(_5607456,freeze(_5607448,'blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase'(_5607448,_5607450,_5607452,_5607454,_5607456,_5607458))).
'blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5586628,_5601656,_5607852,_5607858,_5607864):-hnf('Interval.iinv'('Interval.ipow'(_5586628,'Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_5601656))),_5607852,_5607858,_5607864).
'blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5586628,_5601656,_5613680,_5613686,_5613692):-!,makeShare(_5601656,_5610746),hnf('Prelude.apply'('Prelude.odd'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[])),_5610746),_5617008,_5613686,_5616974),'blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5617008,_5586628,_5610746,_5613680,_5616974,_5613692).

'blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5617814,_5617816,_5617818,_5617820,_5617822,_5617824):-freeze(_5617822,freeze(_5617814,'blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5617814,_5617816,_5617818,_5617820,_5617822,_5617824))).
'blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5586628,_5610746,_5618218,_5618224,_5618230):-hnf('Prelude.foldl1'(partcall(2,'Interval.imul',[]),'Prelude.replicate'(_5610746,_5586628)),_5618218,_5618224,_5618230).
'blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5586628,_5610746,_5623042,_5623048,_5623054):-!,hnf('Prelude.otherwise',_5627594,_5623048,_5627560),'blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5627594,_5586628,_5610746,_5623042,_5627560,_5623054).

'blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5628592,_5628594,_5628596,_5628598,_5628600,_5628602):-freeze(_5628600,freeze(_5628592,'blocked_blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5628592,_5628594,_5628596,_5628598,_5628600,_5628602))).
'blocked_blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5586628,_5610746,_5628996,_5629002,_5629008):-makeShare(_5589200,_5629876),makeShare(_5586628,_5629896),makeShare(_5589218,_5629916),makeShare(_5589236,_5629936),makeShare(_5610746,_5629956),makeShare(_5589254,_5629976),makeShare(_5589272,_5629996),hnf('Prelude.cond'(letrec4PAKCS(_5629876,'Interval.ipow._\'23selFP4\'23l'(_5629896)),'Prelude.cond'(letrec4PAKCS(_5629916,'Interval.ipow._\'23selFP5\'23h'(_5629896)),'Prelude.cond'(letrec4PAKCS(_5629936,'Rational.ratPow'(_5629876,_5629956)),'Prelude.cond'(letrec4PAKCS(_5629976,'Rational.ratPow'(_5629916,_5629956)),'Prelude.cond'(letrec4PAKCS(_5629996,[_5629936,_5629976]),'Interval.ipow._\'23caseor0'('Prelude.&&'('Rational.ratLe'(_5629876,'Prelude.apply'('Interval.ri',0)),'Rational.ratGe'('Prelude.apply'('Interval.ri',0),'Rational.ratNeg'(_5629916))),_5629996)))))),_5628996,_5629002,_5629008).
'blocked_blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5586628,_5610746,_5639424,_5639430,_5639436):-!,hnf(reportFailure4PAKCS('Interval.ipow',['Prelude.False']),_5639424,_5639430,_5639436).
'blocked_blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5641286),_5586628,_5610746,'FAIL'(_5641286),_5641300,_5641300).
'blocked_blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5641368),_5586628,_5610746,'FAIL'(_5641368),_5641382,_5641382).
'blocked_blocked_blocked_Interval.ipow_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5641450),_5586628,_5601656,'FAIL'(_5641450),_5641464,_5641464).
'blocked_blocked_Interval.ipow_ComplexCase'('FAIL'(_5641532),_5586628,_5594208,'FAIL'(_5641532),_5641546,_5641546).

'Interval.ipow._\'23selFP4\'23l'(_5642724,_5642726,_5642728,_5642730):-freeze(_5642728,'blocked_Interval.ipow._\'23selFP4\'23l'(_5642724,_5642726,_5642728,_5642730)).
'blocked_Interval.ipow._\'23selFP4\'23l'(_5642810,_5643224,_5643230,_5643236):-hnf(_5642810,_5644610,_5643230,_5644622),'blocked_Interval.ipow._\'23selFP4\'23l_1'(_5644610,_5643224,_5644622,_5643236).

'blocked_Interval.ipow._\'23selFP4\'23l_1'(_5645032,_5645034,_5645036,_5645038):-freeze(_5645036,'blocked_blocked_Interval.ipow._\'23selFP4\'23l_1'(_5645032,_5645034,_5645036,_5645038)).
'blocked_blocked_Interval.ipow._\'23selFP4\'23l_1'('Interval.IV'(_5642926,_5642944),_5645416,_5645422,_5645428):-!,hnf(_5642926,_5645416,_5645422,_5645428).
'blocked_blocked_Interval.ipow._\'23selFP4\'23l_1'('FAIL'(_5645964),'FAIL'(_5645964),_5645978,_5645978):-nonvar(_5645964).

'Interval.ipow._\'23selFP5\'23h'(_5647144,_5647146,_5647148,_5647150):-freeze(_5647148,'blocked_Interval.ipow._\'23selFP5\'23h'(_5647144,_5647146,_5647148,_5647150)).
'blocked_Interval.ipow._\'23selFP5\'23h'(_5647230,_5647644,_5647650,_5647656):-hnf(_5647230,_5649030,_5647650,_5649042),'blocked_Interval.ipow._\'23selFP5\'23h_1'(_5649030,_5647644,_5649042,_5647656).

'blocked_Interval.ipow._\'23selFP5\'23h_1'(_5649452,_5649454,_5649456,_5649458):-freeze(_5649456,'blocked_blocked_Interval.ipow._\'23selFP5\'23h_1'(_5649452,_5649454,_5649456,_5649458)).
'blocked_blocked_Interval.ipow._\'23selFP5\'23h_1'('Interval.IV'(_5647346,_5647364),_5649836,_5649842,_5649848):-!,hnf(_5647364,_5649836,_5649842,_5649848).
'blocked_blocked_Interval.ipow._\'23selFP5\'23h_1'('FAIL'(_5650384),'FAIL'(_5650384),_5650398,_5650398):-nonvar(_5650384).

'Interval.isqrt'(_5651114,_5651116,_5651118,_5651120):-freeze(_5651118,'blocked_Interval.isqrt'(_5651114,_5651116,_5651118,_5651120)).
'blocked_Interval.isqrt'(_5651200,_5658022,_5658028,_5658034):-hnf(_5651200,_5658904,_5658028,_5658916),'blocked_Interval.isqrt_1'(_5658904,_5658022,_5658916,_5658034).

'blocked_Interval.isqrt_1'(_5659242,_5659244,_5659246,_5659248):-freeze(_5659246,'blocked_blocked_Interval.isqrt_1'(_5659242,_5659244,_5659246,_5659248)).
'blocked_blocked_Interval.isqrt_1'('Interval.IV'(_5651316,_5651334),_5662494,_5662500,_5662506):-!,makeShare(_5651316,_5659894),hnf('Rational.ratLt'(_5659894,'Prelude.apply'('Interval.ri',0)),_5664198,_5662500,_5664164),'blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase'(_5664198,_5659894,_5651334,_5662494,_5664164,_5662506).

'blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase'(_5664734,_5664736,_5664738,_5664740,_5664742,_5664744):-freeze(_5664742,freeze(_5664734,'blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase'(_5664734,_5664736,_5664738,_5664740,_5664742,_5664744))).
'blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase'('Prelude.True',_5659894,_5651334,_5665138,_5665144,_5665150):-hnf('Prelude.error'(['^i','^s','^q','^r','^t',^:,'^ ','^n','^e','^g','^a','^t','^i','^v','^e','^ ','^i','^n','^t','^e','^r','^v','^a','^l']),_5665138,_5665144,_5665150).
'blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase'('Prelude.False',_5659894,_5651334,_5673874,_5673880,_5673886):-!,makeShare(_5651334,_5671282),hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_5671282),'Prelude.apply'('Interval.ri',0)),_5676806,_5673880,_5676772),'blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5676806,_5659894,_5671282,_5673874,_5676772,_5673886).

'blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5677546,_5677548,_5677550,_5677552,_5677554,_5677556):-freeze(_5677554,freeze(_5677546,'blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5677546,_5677548,_5677550,_5677552,_5677554,_5677556))).
'blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5659894,_5671282,'Interval.IV'('Prelude.apply'('Interval.ri',0),'Prelude.apply'('Interval.ri',0)),_5677956,_5677956).
'blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5659894,_5671282,_5681326,_5681332,_5681338):-!,hnf('Prelude.otherwise',_5685482,_5681332,_5685448),'blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5685482,_5659894,_5671282,_5681326,_5685448,_5681338).

'blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5686414,_5686416,_5686418,_5686420,_5686422,_5686424):-freeze(_5686422,freeze(_5686414,'blocked_blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5686414,_5686416,_5686418,_5686420,_5686422,_5686424))).
'blocked_blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5659894,_5671282,'Interval.IV'('Interval.nthRootLower'(2,_5659894),'Interval.nthRootUpper'(2,_5671282)),_5686824,_5686824).
'blocked_blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5659894,_5671282,_5689084,_5689090,_5689096):-!,hnf(reportFailure4PAKCS('Interval.isqrt',['Prelude.False']),_5689084,_5689090,_5689096).
'blocked_blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5690886),_5659894,_5671282,'FAIL'(_5690886),_5690900,_5690900).
'blocked_blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5690968),_5659894,_5671282,'FAIL'(_5690968),_5690982,_5690982).
'blocked_blocked_blocked_Interval.isqrt_1_Interval.IV_ComplexCase'('FAIL'(_5691050),_5659894,_5651334,'FAIL'(_5691050),_5691064,_5691064).
'blocked_blocked_Interval.isqrt_1'('FAIL'(_5691132),'FAIL'(_5691132),_5691146,_5691146):-nonvar(_5691132).

'Interval.inth'(_5691824,_5691826,_5691828,_5691830,_5691832):-freeze(_5691830,'blocked_Interval.inth'(_5691824,_5691826,_5691828,_5691830,_5691832)).
'blocked_Interval.inth'(_5691920,_5691938,_5712676,_5712682,_5712688):-hnf(_5691938,_5713530,_5712682,_5713548),'blocked_Interval.inth_2'(_5713530,_5691920,_5712676,_5713548,_5712688).

'blocked_Interval.inth_2'(_5713876,_5713878,_5713880,_5713882,_5713884):-freeze(_5713882,'blocked_blocked_Interval.inth_2'(_5713876,_5713878,_5713880,_5713882,_5713884)).
'blocked_blocked_Interval.inth_2'('Interval.IV'(_5692054,_5692072),_5691920,_5721972,_5721978,_5721984):-!,makeShare(_5691920,_5716168),hnf('Prelude._impl\'23\'3C\'3D\'23Prelude.Ord\'23Prelude.Int\'23'(_5716168,0),_5723654,_5721978,_5723614),'blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase'(_5723654,_5692054,_5692072,_5716168,_5721972,_5723614,_5721984).

'blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase'(_5724186,_5724188,_5724190,_5724192,_5724194,_5724196,_5724198):-freeze(_5724196,freeze(_5724186,'blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase'(_5724186,_5724188,_5724190,_5724192,_5724194,_5724196,_5724198))).
'blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase'('Prelude.True',_5692054,_5692072,_5716168,_5724600,_5724606,_5724612):-hnf('Prelude.error'(['^i','^n','^t','^h',^:,'^ ','^n','^o','^n',^-,'^p','^o','^s','^i','^t','^i','^v','^e','^ ','^r','^o','^o','^t','^ ','^i','^n','^d','^e','^x']),_5724600,_5724606,_5724612).
'blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase'('Prelude.False',_5692054,_5692072,_5716168,_5738530,_5738536,_5738542):-!,makeShare(_5716168,_5733448),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5733448,1),_5741440,_5738536,_5741400),'blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5741440,_5692054,_5692072,_5733448,_5738530,_5741400,_5738542).

'blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5742176,_5742178,_5742180,_5742182,_5742184,_5742186,_5742188):-freeze(_5742186,freeze(_5742176,'blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5742176,_5742178,_5742180,_5742182,_5742184,_5742186,_5742188))).
'blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5692054,_5692072,_5733448,'Interval.IV'(_5692054,_5692072),_5742596,_5742596).
'blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5692054,_5692072,_5733448,_5750828,_5750834,_5750840):-!,makeShare(_5733448,_5745760),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5745760,2),_5754962,_5750834,_5754922),'blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5754962,_5692054,_5692072,_5745760,_5750828,_5754922,_5750840).

'blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5755902,_5755904,_5755906,_5755908,_5755910,_5755912,_5755914):-freeze(_5755912,freeze(_5755902,'blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5755902,_5755904,_5755906,_5755908,_5755910,_5755912,_5755914))).
'blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5692054,_5692072,_5745760,_5756316,_5756322,_5756328):-hnf('Interval.isqrt'('Interval.IV'(_5692054,_5692072)),_5756316,_5756322,_5756328).
'blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5692054,_5692072,_5745760,_5766030,_5766036,_5766042):-!,makeShare(_5745760,_5759820),makeShare(_5692054,_5759840),hnf('Prelude.&&'('Prelude.even'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[]),_5759820),'Rational.ratLt'(_5759840,'Prelude.apply'('Interval.ri',0))),_5771388,_5766036,_5771348),'blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5771388,_5759840,_5692072,_5759820,_5766030,_5771348,_5766042).

'blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5772544,_5772546,_5772548,_5772550,_5772552,_5772554,_5772556):-freeze(_5772554,freeze(_5772544,'blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5772544,_5772546,_5772548,_5772550,_5772552,_5772554,_5772556))).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5759840,_5692072,_5759820,_5772958,_5772964,_5772970):-hnf('Prelude.error'(['^i','^n','^t','^h',^:,'^ ','^e','^v','^e','^n','^ ','^r','^o','^o','^t','^ ','^o','^f','^ ','^n','^e','^g','^a','^t','^i','^v','^e','^ ','^i','^n','^t','^e','^r','^v','^a','^l']),_5772958,_5772964,_5772970).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5759840,_5692072,_5759820,_5790014,_5790020,_5790026):-!,makeShare(_5692072,_5783508),makeShare(_5759840,_5783528),hnf('Prelude.&&'('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_5783508),'Prelude.apply'('Interval.ri',0)),'Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_5783528),'Prelude.apply'('Interval.ri',0))),_5796596,_5790020,_5796556),'blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5796596,_5783528,_5783508,_5759820,_5790014,_5796556,_5790026).

'blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5797956,_5797958,_5797960,_5797962,_5797964,_5797966,_5797968):-freeze(_5797966,freeze(_5797956,'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5797956,_5797958,_5797960,_5797962,_5797964,_5797966,_5797968))).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5783528,_5783508,_5759820,'Interval.IV'('Prelude.apply'('Interval.ri',0),'Prelude.apply'('Interval.ri',0)),_5798376,_5798376).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5783528,_5783508,_5759820,_5807654,_5807660,_5807666):-!,makeShare(_5759820,_5802460),makeShare(_5783508,_5802480),hnf('Prelude.&&'('Prelude.apply'('Prelude.odd'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[])),_5802460),'Rational.ratLt'(_5802480,'Prelude.apply'('Interval.ri',0))),_5815460,_5807660,_5815420),'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5815460,_5783528,_5802480,_5802460,_5807654,_5815420,_5807666).

'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5817024,_5817026,_5817028,_5817030,_5817032,_5817034,_5817036):-freeze(_5817034,freeze(_5817024,'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5817024,_5817026,_5817028,_5817030,_5817032,_5817034,_5817036))).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5783528,_5802480,_5802460,_5817438,_5817444,_5817450):-makeShare(_5707186,_5817976),makeShare(_5707204,_5817996),makeShare(_5707222,_5818016),hnf('Prelude.cond'(letrec4PAKCS(_5817976,'Interval.inth'(_5802460,'Interval.IV'('Rational.ratNeg'(_5802480),'Rational.ratNeg'(_5783528)))),'Prelude.cond'(letrec4PAKCS(_5817996,'Interval.inth._\'23selFP7\'23l\'27'(_5817976)),'Prelude.cond'(letrec4PAKCS(_5818016,'Interval.inth._\'23selFP8\'23h\'27'(_5817976)),'Interval.IV'('Rational.ratNeg'(_5818016),'Rational.ratNeg'(_5817996))))),_5817438,_5817444,_5817450).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5783528,_5802480,_5802460,_5829704,_5829710,_5829716):-!,makeShare(_5802460,_5825108),makeShare(_5783528,_5825128),hnf('Prelude.&&'('Prelude.apply'('Prelude.odd'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[])),_5825108),'Rational.ratLt'(_5825128,'Prelude.apply'('Interval.ri',0))),_5838734,_5829710,_5838694),'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5838734,_5825128,_5802480,_5825108,_5829704,_5838694,_5829716).

'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5840502,_5840504,_5840506,_5840508,_5840510,_5840512,_5840514):-freeze(_5840512,freeze(_5840502,'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5840502,_5840504,_5840506,_5840508,_5840510,_5840512,_5840514))).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5825128,_5802480,_5825108,_5840916,_5840922,_5840928):-makeShare(_5710070,_5841294),makeShare(_5825108,_5841314),makeShare(_5710088,_5841334),hnf('Prelude.cond'(letrec4PAKCS(_5841294,'Interval.nthRootUpper'(_5841314,'Rational.ratNeg'(_5825128))),'Prelude.cond'(letrec4PAKCS(_5841334,'Interval.nthRootUpper'(_5841314,_5802480)),'Interval.IV'('Rational.ratNeg'(_5841294),_5841334))),_5840916,_5840922,_5840928).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5825128,_5802480,_5825108,_5849024,_5849030,_5849036):-!,hnf('Prelude.otherwise',_5859278,_5849030,_5859238),'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5859278,_5825128,_5802480,_5825108,_5849024,_5859238,_5849036).

'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5861226,_5861228,_5861230,_5861232,_5861234,_5861236,_5861238):-freeze(_5861236,freeze(_5861226,'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5861226,_5861228,_5861230,_5861232,_5861234,_5861236,_5861238))).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5825128,_5802480,_5825108,'Interval.IV'('Interval.nthRootLower'(_5861818,_5825128),'Interval.nthRootUpper'(_5861818,_5802480)),_5861646,_5861652):-makeShare(_5825108,_5861818),_5861646=_5861652.
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5825128,_5802480,_5825108,_5865294,_5865300,_5865306):-!,hnf(reportFailure4PAKCS('Interval.inth',['Prelude.False']),_5865294,_5865300,_5865306).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5868178),_5825128,_5802480,_5825108,'FAIL'(_5868178),_5868192,_5868192).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5868268),_5825128,_5802480,_5825108,'FAIL'(_5868268),_5868282,_5868282).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5868358),_5783528,_5802480,_5802460,'FAIL'(_5868358),_5868372,_5868372).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5868448),_5783528,_5783508,_5759820,'FAIL'(_5868448),_5868462,_5868462).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5868538),_5759840,_5692072,_5759820,'FAIL'(_5868538),_5868552,_5868552).
'blocked_blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5868628),_5692054,_5692072,_5745760,'FAIL'(_5868628),_5868642,_5868642).
'blocked_blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5868718),_5692054,_5692072,_5733448,'FAIL'(_5868718),_5868732,_5868732).
'blocked_blocked_blocked_Interval.inth_2_Interval.IV_ComplexCase'('FAIL'(_5868808),_5692054,_5692072,_5716168,'FAIL'(_5868808),_5868822,_5868822).
'blocked_blocked_Interval.inth_2'('FAIL'(_5868898),_5691920,'FAIL'(_5868898),_5868912,_5868912):-nonvar(_5868898).

'Interval.inth._\'23selFP7\'23l\'27'(_5870166,_5870168,_5870170,_5870172):-freeze(_5870170,'blocked_Interval.inth._\'23selFP7\'23l\'27'(_5870166,_5870168,_5870170,_5870172)).
'blocked_Interval.inth._\'23selFP7\'23l\'27'(_5870252,_5870684,_5870690,_5870696):-hnf(_5870252,_5872178,_5870690,_5872190),'blocked_Interval.inth._\'23selFP7\'23l\'27_1'(_5872178,_5870684,_5872190,_5870696).

'blocked_Interval.inth._\'23selFP7\'23l\'27_1'(_5872618,_5872620,_5872622,_5872624):-freeze(_5872622,'blocked_blocked_Interval.inth._\'23selFP7\'23l\'27_1'(_5872618,_5872620,_5872622,_5872624)).
'blocked_blocked_Interval.inth._\'23selFP7\'23l\'27_1'('Interval.IV'(_5870368,_5870386),_5873002,_5873008,_5873014):-!,hnf(_5870368,_5873002,_5873008,_5873014).
'blocked_blocked_Interval.inth._\'23selFP7\'23l\'27_1'('FAIL'(_5873568),'FAIL'(_5873568),_5873582,_5873582):-nonvar(_5873568).

'Interval.inth._\'23selFP8\'23h\'27'(_5874828,_5874830,_5874832,_5874834):-freeze(_5874832,'blocked_Interval.inth._\'23selFP8\'23h\'27'(_5874828,_5874830,_5874832,_5874834)).
'blocked_Interval.inth._\'23selFP8\'23h\'27'(_5874914,_5875346,_5875352,_5875358):-hnf(_5874914,_5876840,_5875352,_5876852),'blocked_Interval.inth._\'23selFP8\'23h\'27_1'(_5876840,_5875346,_5876852,_5875358).

'blocked_Interval.inth._\'23selFP8\'23h\'27_1'(_5877280,_5877282,_5877284,_5877286):-freeze(_5877284,'blocked_blocked_Interval.inth._\'23selFP8\'23h\'27_1'(_5877280,_5877282,_5877284,_5877286)).
'blocked_blocked_Interval.inth._\'23selFP8\'23h\'27_1'('Interval.IV'(_5875030,_5875048),_5877664,_5877670,_5877676):-!,hnf(_5875048,_5877664,_5877670,_5877676).
'blocked_blocked_Interval.inth._\'23selFP8\'23h\'27_1'('FAIL'(_5878230),'FAIL'(_5878230),_5878244,_5878244):-nonvar(_5878230).

'Interval.nthRootLower'(_5879226,_5879228,_5879230,_5879232,_5879234):-freeze(_5879232,'blocked_Interval.nthRootLower'(_5879226,_5879228,_5879230,_5879232,_5879234)).
'blocked_Interval.nthRootLower'(_5879322,_5879340,_5884690,_5884696,_5884702):-makeShare(_5879340,_5882168),hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_5882168),'Prelude.apply'('Interval.ri',0)),_5885934,_5884696,_5885900),'blocked_Interval.nthRootLower_ComplexCase'(_5885934,_5879322,_5882168,_5884690,_5885900,_5884702).

'blocked_Interval.nthRootLower_ComplexCase'(_5886362,_5886364,_5886366,_5886368,_5886370,_5886372):-freeze(_5886370,freeze(_5886362,'blocked_blocked_Interval.nthRootLower_ComplexCase'(_5886362,_5886364,_5886366,_5886368,_5886370,_5886372))).
'blocked_blocked_Interval.nthRootLower_ComplexCase'('Prelude.True',_5879322,_5882168,_5886766,_5886772,_5886778):-hnf('Prelude.apply'('Interval.ri',0),_5886766,_5886772,_5886778).
'blocked_blocked_Interval.nthRootLower_ComplexCase'('Prelude.False',_5879322,_5882168,_5889236,_5889242,_5889248):-!,hnf('Prelude.otherwise',_5891628,_5889242,_5891594),'blocked_blocked_Interval.nthRootLower_ComplexCase_Prelude.False_ComplexCase'(_5891628,_5879322,_5882168,_5889236,_5891594,_5889248).

'blocked_blocked_Interval.nthRootLower_ComplexCase_Prelude.False_ComplexCase'(_5892266,_5892268,_5892270,_5892272,_5892274,_5892276):-freeze(_5892274,freeze(_5892266,'blocked_blocked_blocked_Interval.nthRootLower_ComplexCase_Prelude.False_ComplexCase'(_5892266,_5892268,_5892270,_5892272,_5892274,_5892276))).
'blocked_blocked_blocked_Interval.nthRootLower_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5879322,_5882168,_5892670,_5892676,_5892682):-makeShare(_5880322,_5892882),makeShare(_5882168,_5892902),hnf('Prelude.cond'(letrec4PAKCS(_5892882,'Rational.ratMax'(_5892902,'Prelude.apply'('Interval.ri',1))),'Interval.bisectDown'(_5879322,_5892902,'Prelude.apply'('Interval.ri',0),_5892882,60)),_5892670,_5892676,_5892682).
'blocked_blocked_blocked_Interval.nthRootLower_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5879322,_5882168,_5896486,_5896492,_5896498):-!,hnf(reportFailure4PAKCS('Interval.nthRootLower',['Prelude.False']),_5896486,_5896492,_5896498).
'blocked_blocked_blocked_Interval.nthRootLower_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5898036),_5879322,_5882168,'FAIL'(_5898036),_5898050,_5898050).
'blocked_blocked_Interval.nthRootLower_ComplexCase'('FAIL'(_5898118),_5879322,_5882168,'FAIL'(_5898118),_5898132,_5898132).

'Interval.nthRootUpper'(_5899126,_5899128,_5899130,_5899132,_5899134):-freeze(_5899132,'blocked_Interval.nthRootUpper'(_5899126,_5899128,_5899130,_5899132,_5899134)).
'blocked_Interval.nthRootUpper'(_5899222,_5899240,_5904972,_5904978,_5904984):-makeShare(_5899240,_5902398),hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_5902398),'Prelude.apply'('Interval.ri',0)),_5906216,_5904978,_5906182),'blocked_Interval.nthRootUpper_ComplexCase'(_5906216,_5899222,_5902398,_5904972,_5906182,_5904984).

'blocked_Interval.nthRootUpper_ComplexCase'(_5906644,_5906646,_5906648,_5906650,_5906652,_5906654):-freeze(_5906652,freeze(_5906644,'blocked_blocked_Interval.nthRootUpper_ComplexCase'(_5906644,_5906646,_5906648,_5906650,_5906652,_5906654))).
'blocked_blocked_Interval.nthRootUpper_ComplexCase'('Prelude.True',_5899222,_5902398,_5907048,_5907054,_5907060):-hnf('Prelude.apply'('Interval.ri',0),_5907048,_5907054,_5907060).
'blocked_blocked_Interval.nthRootUpper_ComplexCase'('Prelude.False',_5899222,_5902398,_5909578,_5909584,_5909590):-!,hnf('Prelude.otherwise',_5911970,_5909584,_5911936),'blocked_blocked_Interval.nthRootUpper_ComplexCase_Prelude.False_ComplexCase'(_5911970,_5899222,_5902398,_5909578,_5911936,_5909590).

'blocked_blocked_Interval.nthRootUpper_ComplexCase_Prelude.False_ComplexCase'(_5912608,_5912610,_5912612,_5912614,_5912616,_5912618):-freeze(_5912616,freeze(_5912608,'blocked_blocked_blocked_Interval.nthRootUpper_ComplexCase_Prelude.False_ComplexCase'(_5912608,_5912610,_5912612,_5912614,_5912616,_5912618))).
'blocked_blocked_blocked_Interval.nthRootUpper_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5899222,_5902398,_5913012,_5913018,_5913024):-makeShare(_5900222,_5913232),makeShare(_5902398,_5913252),hnf('Prelude.cond'(letrec4PAKCS(_5913232,'Rational.ratAdd'('Rational.ratMax'(_5913252,'Prelude.apply'('Interval.ri',1)),'Prelude.apply'('Interval.ri',1))),'Interval.bisectUp'(_5899222,_5913252,'Prelude.apply'('Interval.ri',0),_5913232,60)),_5913012,_5913018,_5913024).
'blocked_blocked_blocked_Interval.nthRootUpper_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5899222,_5902398,_5917458,_5917464,_5917470):-!,hnf(reportFailure4PAKCS('Interval.nthRootUpper',['Prelude.False']),_5917458,_5917464,_5917470).
'blocked_blocked_blocked_Interval.nthRootUpper_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5919008),_5899222,_5902398,'FAIL'(_5919008),_5919022,_5919022).
'blocked_blocked_Interval.nthRootUpper_ComplexCase'('FAIL'(_5919090),_5899222,_5902398,'FAIL'(_5919090),_5919104,_5919104).

'Interval.bisectDown'(_5920022,_5920024,_5920026,_5920028,_5920030,_5920032,_5920034,_5920036):-freeze(_5920034,'blocked_Interval.bisectDown'(_5920022,_5920024,_5920026,_5920028,_5920030,_5920032,_5920034,_5920036)).
'blocked_Interval.bisectDown'(_5920148,_5920166,_5920184,_5920202,_5920220,_5925396,_5925402,_5925408):-makeShare(_5920220,_5923422),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5923422,0),_5926610,_5925402,_5926558),'blocked_Interval.bisectDown_ComplexCase'(_5926610,_5920148,_5920166,_5920184,_5920202,_5923422,_5925396,_5926558,_5925408).

'blocked_Interval.bisectDown_ComplexCase'(_5927032,_5927034,_5927036,_5927038,_5927040,_5927042,_5927044,_5927046,_5927048):-freeze(_5927046,freeze(_5927032,'blocked_blocked_Interval.bisectDown_ComplexCase'(_5927032,_5927034,_5927036,_5927038,_5927040,_5927042,_5927044,_5927046,_5927048))).
'blocked_blocked_Interval.bisectDown_ComplexCase'('Prelude.True',_5920148,_5920166,_5920184,_5920202,_5923422,_5927466,_5927472,_5927478):-hnf(_5920184,_5927466,_5927472,_5927478).
'blocked_blocked_Interval.bisectDown_ComplexCase'('Prelude.False',_5920148,_5920166,_5920184,_5920202,_5923422,_5930394,_5930400,_5930406):-!,hnf('Prelude.otherwise',_5932756,_5930400,_5932704),'blocked_blocked_Interval.bisectDown_ComplexCase_Prelude.False_ComplexCase'(_5932756,_5920148,_5920166,_5920184,_5920202,_5923422,_5930394,_5932704,_5930406).

'blocked_blocked_Interval.bisectDown_ComplexCase_Prelude.False_ComplexCase'(_5933388,_5933390,_5933392,_5933394,_5933396,_5933398,_5933400,_5933402,_5933404):-freeze(_5933402,freeze(_5933388,'blocked_blocked_blocked_Interval.bisectDown_ComplexCase_Prelude.False_ComplexCase'(_5933388,_5933390,_5933392,_5933394,_5933396,_5933398,_5933400,_5933402,_5933404))).
'blocked_blocked_blocked_Interval.bisectDown_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5920148,_5920166,_5920184,_5920202,_5923422,_5933822,_5933828,_5933834):-makeShare(_5920698,_5934406),makeShare(_5920184,_5934426),makeShare(_5920202,_5934446),makeShare(_5920148,_5934466),makeShare(_5920166,_5934486),hnf('Prelude.cond'(letrec4PAKCS(_5934406,'Rational.ratDiv'('Rational.ratAdd'(_5934426,_5934446),'Prelude.apply'('Interval.ri',2))),'Interval.bisectDown._\'23caseor0'('Rational.ratLe'('Rational.ratPow'(_5934406,_5934466),_5934486),_5934446,_5934466,_5934486,_5934426,_5934406,_5923422)),_5933822,_5933828,_5933834).
'blocked_blocked_blocked_Interval.bisectDown_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5920148,_5920166,_5920184,_5920202,_5923422,_5939620,_5939626,_5939632):-!,hnf(reportFailure4PAKCS('Interval.bisectDown',['Prelude.False']),_5939620,_5939626,_5939632).
'blocked_blocked_blocked_Interval.bisectDown_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5941368),_5920148,_5920166,_5920184,_5920202,_5923422,'FAIL'(_5941368),_5941382,_5941382).
'blocked_blocked_Interval.bisectDown_ComplexCase'('FAIL'(_5941474),_5920148,_5920166,_5920184,_5920202,_5923422,'FAIL'(_5941474),_5941488,_5941488).

'Interval.bisectUp'(_5942354,_5942356,_5942358,_5942360,_5942362,_5942364,_5942366,_5942368):-freeze(_5942366,'blocked_Interval.bisectUp'(_5942354,_5942356,_5942358,_5942360,_5942362,_5942364,_5942366,_5942368)).
'blocked_Interval.bisectUp'(_5942480,_5942498,_5942516,_5942534,_5942552,_5947716,_5947722,_5947728):-makeShare(_5942552,_5945754),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5945754,0),_5948858,_5947722,_5948806),'blocked_Interval.bisectUp_ComplexCase'(_5948858,_5942480,_5942498,_5942516,_5942534,_5945754,_5947716,_5948806,_5947728).

'blocked_Interval.bisectUp_ComplexCase'(_5949268,_5949270,_5949272,_5949274,_5949276,_5949278,_5949280,_5949282,_5949284):-freeze(_5949282,freeze(_5949268,'blocked_blocked_Interval.bisectUp_ComplexCase'(_5949268,_5949270,_5949272,_5949274,_5949276,_5949278,_5949280,_5949282,_5949284))).
'blocked_blocked_Interval.bisectUp_ComplexCase'('Prelude.True',_5942480,_5942498,_5942516,_5942534,_5945754,_5949702,_5949708,_5949714):-hnf(_5942534,_5949702,_5949708,_5949714).
'blocked_blocked_Interval.bisectUp_ComplexCase'('Prelude.False',_5942480,_5942498,_5942516,_5942534,_5945754,_5952606,_5952612,_5952618):-!,hnf('Prelude.otherwise',_5954896,_5952612,_5954844),'blocked_blocked_Interval.bisectUp_ComplexCase_Prelude.False_ComplexCase'(_5954896,_5942480,_5942498,_5942516,_5942534,_5945754,_5952606,_5954844,_5952618).

'blocked_blocked_Interval.bisectUp_ComplexCase_Prelude.False_ComplexCase'(_5955516,_5955518,_5955520,_5955522,_5955524,_5955526,_5955528,_5955530,_5955532):-freeze(_5955530,freeze(_5955516,'blocked_blocked_blocked_Interval.bisectUp_ComplexCase_Prelude.False_ComplexCase'(_5955516,_5955518,_5955520,_5955522,_5955524,_5955526,_5955528,_5955530,_5955532))).
'blocked_blocked_blocked_Interval.bisectUp_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5942480,_5942498,_5942516,_5942534,_5945754,_5955950,_5955956,_5955962):-makeShare(_5943030,_5956534),makeShare(_5942516,_5956554),makeShare(_5942534,_5956574),makeShare(_5942480,_5956594),makeShare(_5942498,_5956614),hnf('Prelude.cond'(letrec4PAKCS(_5956534,'Rational.ratDiv'('Rational.ratAdd'(_5956554,_5956574),'Prelude.apply'('Interval.ri',2))),'Interval.bisectUp._\'23caseor0'('Rational.ratGe'('Rational.ratPow'(_5956534,_5956594),_5956614),_5956554,_5956594,_5956614,_5956534,_5956574,_5945754)),_5955950,_5955956,_5955962).
'blocked_blocked_blocked_Interval.bisectUp_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5942480,_5942498,_5942516,_5942534,_5945754,_5961724,_5961730,_5961736):-!,hnf(reportFailure4PAKCS('Interval.bisectUp',['Prelude.False']),_5961724,_5961730,_5961736).
'blocked_blocked_blocked_Interval.bisectUp_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5963448),_5942480,_5942498,_5942516,_5942534,_5945754,'FAIL'(_5963448),_5963462,_5963462).
'blocked_blocked_Interval.bisectUp_ComplexCase'('FAIL'(_5963554),_5942480,_5942498,_5942516,_5942534,_5945754,'FAIL'(_5963554),_5963568,_5963568).

'Interval.iabs'(_5964282,_5964284,_5964286,_5964288):-freeze(_5964286,'blocked_Interval.iabs'(_5964282,_5964284,_5964286,_5964288)).
'blocked_Interval.iabs'(_5964368,_5967306,_5967312,_5967318):-hnf(_5964368,_5968152,_5967312,_5968164),'blocked_Interval.iabs_1'(_5968152,_5967306,_5968164,_5967318).

'blocked_Interval.iabs_1'(_5968484,_5968486,_5968488,_5968490):-freeze(_5968488,'blocked_blocked_Interval.iabs_1'(_5968484,_5968486,_5968488,_5968490)).
'blocked_blocked_Interval.iabs_1'('Interval.IV'(_5964484,_5964502),_5971094,_5971100,_5971106):-!,makeShare(_5964484,_5969108),hnf('Rational.ratGe'(_5969108,'Prelude.apply'('Interval.ri',0)),_5972762,_5971100,_5972728),'blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase'(_5972762,_5969108,_5964502,_5971094,_5972728,_5971106).

'blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase'(_5973292,_5973294,_5973296,_5973298,_5973300,_5973302):-freeze(_5973300,freeze(_5973292,'blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase'(_5973292,_5973294,_5973296,_5973298,_5973300,_5973302))).
'blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase'('Prelude.True',_5969108,_5964502,'Interval.IV'(_5969108,_5964502),_5973702,_5973702).
'blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase'('Prelude.False',_5969108,_5964502,_5977010,_5977016,_5977022):-!,makeShare(_5964502,_5975078),hnf('Rational.ratLe'(_5975078,'Prelude.apply'('Interval.ri',0)),_5979906,_5977016,_5979872),'blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5979906,_5969108,_5975078,_5977010,_5979872,_5977022).

'blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5980640,_5980642,_5980644,_5980646,_5980648,_5980650):-freeze(_5980648,freeze(_5980640,'blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_5980640,_5980642,_5980644,_5980646,_5980648,_5980650))).
'blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5969108,_5975078,'Interval.IV'('Rational.ratNeg'(_5975078),'Rational.ratNeg'(_5969108)),_5981050,_5981050).
'blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5969108,_5975078,_5984140,_5984146,_5984152):-!,hnf('Prelude.otherwise',_5988260,_5984146,_5988226),'blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5988260,_5969108,_5975078,_5984140,_5988226,_5984152).

'blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5989186,_5989188,_5989190,_5989192,_5989194,_5989196):-freeze(_5989194,freeze(_5989186,'blocked_blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5989186,_5989188,_5989190,_5989192,_5989194,_5989196))).
'blocked_blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5969108,_5975078,'Interval.IV'('Prelude.apply'('Interval.ri',0),'Rational.ratMax'('Rational.ratNeg'(_5969108),_5975078)),_5989596,_5989596).
'blocked_blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5969108,_5975078,_5992060,_5992066,_5992072):-!,hnf(reportFailure4PAKCS('Interval.iabs',['Prelude.False']),_5992060,_5992066,_5992072).
'blocked_blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5993850),_5969108,_5975078,'FAIL'(_5993850),_5993864,_5993864).
'blocked_blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5993932),_5969108,_5975078,'FAIL'(_5993932),_5993946,_5993946).
'blocked_blocked_blocked_Interval.iabs_1_Interval.IV_ComplexCase'('FAIL'(_5994014),_5969108,_5964502,'FAIL'(_5994014),_5994028,_5994028).
'blocked_blocked_Interval.iabs_1'('FAIL'(_5994096),'FAIL'(_5994096),_5994110,_5994110):-nonvar(_5994096).

'Interval.strictlyPositive'(_5995244,_5995246,_5995248,_5995250):-freeze(_5995248,'blocked_Interval.strictlyPositive'(_5995244,_5995246,_5995248,_5995250)).
'blocked_Interval.strictlyPositive'(_5995330,_5996048,_5996054,_5996060):-hnf(_5995330,_5997326,_5996054,_5997338),'blocked_Interval.strictlyPositive_1'(_5997326,_5996048,_5997338,_5996060).

'blocked_Interval.strictlyPositive_1'(_5997730,_5997732,_5997734,_5997736):-freeze(_5997734,'blocked_blocked_Interval.strictlyPositive_1'(_5997730,_5997732,_5997734,_5997736)).
'blocked_blocked_Interval.strictlyPositive_1'('Interval.IV'(_5995446,_5995464),_5998114,_5998120,_5998126):-!,hnf('Rational.ratGt'(_5995446,'Prelude.apply'('Interval.ri',0)),_5998114,_5998120,_5998126).
'blocked_blocked_Interval.strictlyPositive_1'('FAIL'(_5999280),'FAIL'(_5999280),_5999294,_5999294):-nonvar(_5999280).

'Interval.strictlyNegative'(_6000428,_6000430,_6000432,_6000434):-freeze(_6000432,'blocked_Interval.strictlyNegative'(_6000428,_6000430,_6000432,_6000434)).
'blocked_Interval.strictlyNegative'(_6000514,_6001232,_6001238,_6001244):-hnf(_6000514,_6002510,_6001238,_6002522),'blocked_Interval.strictlyNegative_1'(_6002510,_6001232,_6002522,_6001244).

'blocked_Interval.strictlyNegative_1'(_6002914,_6002916,_6002918,_6002920):-freeze(_6002918,'blocked_blocked_Interval.strictlyNegative_1'(_6002914,_6002916,_6002918,_6002920)).
'blocked_blocked_Interval.strictlyNegative_1'('Interval.IV'(_6000630,_6000648),_6003298,_6003304,_6003310):-!,hnf('Rational.ratLt'(_6000648,'Prelude.apply'('Interval.ri',0)),_6003298,_6003304,_6003310).
'blocked_blocked_Interval.strictlyNegative_1'('FAIL'(_6004464),'FAIL'(_6004464),_6004478,_6004478):-nonvar(_6004464).

'Interval.containsZero'(_6005460,_6005462,_6005464,_6005466):-freeze(_6005464,'blocked_Interval.containsZero'(_6005460,_6005462,_6005464,_6005466)).
'blocked_Interval.containsZero'(_6005546,_6006716,_6006722,_6006728):-hnf(_6005546,_6007850,_6006722,_6007862),'blocked_Interval.containsZero_1'(_6007850,_6006716,_6007862,_6006728).

'blocked_Interval.containsZero_1'(_6008230,_6008232,_6008234,_6008236):-freeze(_6008234,'blocked_blocked_Interval.containsZero_1'(_6008230,_6008232,_6008234,_6008236)).
'blocked_blocked_Interval.containsZero_1'('Interval.IV'(_6005662,_6005680),_6008614,_6008620,_6008626):-!,hnf('Prelude.&&'('Rational.ratLe'(_6005662,'Prelude.apply'('Interval.ri',0)),'Rational.ratGe'(_6005680,'Prelude.apply'('Interval.ri',0))),_6008614,_6008620,_6008626).
'blocked_blocked_Interval.containsZero_1'('FAIL'(_6010730),'FAIL'(_6010730),_6010744,_6010744):-nonvar(_6010730).

'Interval.ciFromRat'(_6011612,_6011614,_6011616,_6011618):-freeze(_6011616,'blocked_Interval.ciFromRat'(_6011612,_6011614,_6011616,_6011618)).
'blocked_Interval.ciFromRat'(_6011698,'Interval.CI'('Interval.fromRat'(_6011698),'Interval.fromRat'('Prelude.apply'('Interval.ri',0))),_6012210,_6012210).

'Interval.ciFromReal'(_6014410,_6014412,_6014414,_6014416):-freeze(_6014414,'blocked_Interval.ciFromReal'(_6014410,_6014412,_6014414,_6014416)).
'blocked_Interval.ciFromReal'(_6014496,'Interval.CI'(_6014496,'Interval.fromRat'('Prelude.apply'('Interval.ri',0))),_6014924,_6014924).

'Interval.ciadd'(_6016734,_6016736,_6016738,_6016740,_6016742):-freeze(_6016740,'blocked_Interval.ciadd'(_6016734,_6016736,_6016738,_6016740,_6016742)).
'blocked_Interval.ciadd'(_6016830,_6016848,_6017792,_6017798,_6017804):-hnf(_6016830,_6018682,_6017798,_6018700),'blocked_Interval.ciadd_1'(_6018682,_6016848,_6017792,_6018700,_6017804).

'blocked_Interval.ciadd_1'(_6019034,_6019036,_6019038,_6019040,_6019042):-freeze(_6019040,'blocked_blocked_Interval.ciadd_1'(_6019034,_6019036,_6019038,_6019040,_6019042)).
'blocked_blocked_Interval.ciadd_1'('Interval.CI'(_6016964,_6016982),_6016848,_6019666,_6019672,_6019678):-!,hnf(_6016848,_6021276,_6019672,_6021300),'blocked_blocked_Interval.ciadd_1_Interval.CI_3'(_6021276,_6016964,_6016982,_6019666,_6021300,_6019678).

'blocked_blocked_Interval.ciadd_1_Interval.CI_3'(_6021792,_6021794,_6021796,_6021798,_6021800,_6021802):-freeze(_6021800,'blocked_blocked_blocked_Interval.ciadd_1_Interval.CI_3'(_6021792,_6021794,_6021796,_6021798,_6021800,_6021802)).
'blocked_blocked_blocked_Interval.ciadd_1_Interval.CI_3'('Interval.CI'(_6017110,_6017128),_6016964,_6016982,'Interval.CI'('Interval.iadd'(_6016964,_6017110),'Interval.iadd'(_6016982,_6017128)),_6022202,_6022202):-!.
'blocked_blocked_blocked_Interval.ciadd_1_Interval.CI_3'('FAIL'(_6023816),_6016964,_6016982,'FAIL'(_6023816),_6023830,_6023830):-nonvar(_6023816).
'blocked_blocked_Interval.ciadd_1'('FAIL'(_6023902),_6016848,'FAIL'(_6023902),_6023916,_6023916):-nonvar(_6023902).

'Interval.cisub'(_6024640,_6024642,_6024644,_6024646,_6024648):-freeze(_6024646,'blocked_Interval.cisub'(_6024640,_6024642,_6024644,_6024646,_6024648)).
'blocked_Interval.cisub'(_6024736,_6024754,_6025698,_6025704,_6025710):-hnf(_6024736,_6026588,_6025704,_6026606),'blocked_Interval.cisub_1'(_6026588,_6024754,_6025698,_6026606,_6025710).

'blocked_Interval.cisub_1'(_6026940,_6026942,_6026944,_6026946,_6026948):-freeze(_6026946,'blocked_blocked_Interval.cisub_1'(_6026940,_6026942,_6026944,_6026946,_6026948)).
'blocked_blocked_Interval.cisub_1'('Interval.CI'(_6024870,_6024888),_6024754,_6027572,_6027578,_6027584):-!,hnf(_6024754,_6029182,_6027578,_6029206),'blocked_blocked_Interval.cisub_1_Interval.CI_3'(_6029182,_6024870,_6024888,_6027572,_6029206,_6027584).

'blocked_blocked_Interval.cisub_1_Interval.CI_3'(_6029698,_6029700,_6029702,_6029704,_6029706,_6029708):-freeze(_6029706,'blocked_blocked_blocked_Interval.cisub_1_Interval.CI_3'(_6029698,_6029700,_6029702,_6029704,_6029706,_6029708)).
'blocked_blocked_blocked_Interval.cisub_1_Interval.CI_3'('Interval.CI'(_6025016,_6025034),_6024870,_6024888,'Interval.CI'('Interval.isub'(_6024870,_6025016),'Interval.isub'(_6024888,_6025034)),_6030108,_6030108):-!.
'blocked_blocked_blocked_Interval.cisub_1_Interval.CI_3'('FAIL'(_6031722),_6024870,_6024888,'FAIL'(_6031722),_6031736,_6031736):-nonvar(_6031722).
'blocked_blocked_Interval.cisub_1'('FAIL'(_6031808),_6024754,'FAIL'(_6031808),_6031822,_6031822):-nonvar(_6031808).

'Interval.cineg'(_6032546,_6032548,_6032550,_6032552):-freeze(_6032550,'blocked_Interval.cineg'(_6032546,_6032548,_6032550,_6032552)).
'blocked_Interval.cineg'(_6032632,_6034064,_6034070,_6034076):-hnf(_6032632,_6034946,_6034070,_6034958),'blocked_Interval.cineg_1'(_6034946,_6034064,_6034958,_6034076).

'blocked_Interval.cineg_1'(_6035284,_6035286,_6035288,_6035290):-freeze(_6035288,'blocked_blocked_Interval.cineg_1'(_6035284,_6035286,_6035288,_6035290)).
'blocked_blocked_Interval.cineg_1'('Interval.CI'(_6032748,_6032766),_6035890,_6035896,_6035902):-!,hnf(_6032748,_6037492,_6035896,_6037510),'blocked_blocked_Interval.cineg_1_Interval.CI_1'(_6037492,_6032766,_6035890,_6037510,_6035902).

'blocked_blocked_Interval.cineg_1_Interval.CI_1'(_6037994,_6037996,_6037998,_6038000,_6038002):-freeze(_6038000,'blocked_blocked_blocked_Interval.cineg_1_Interval.CI_1'(_6037994,_6037996,_6037998,_6038000,_6038002)).
'blocked_blocked_blocked_Interval.cineg_1_Interval.CI_1'('Interval.IV'(_6032894,_6032912),_6032766,_6038758,_6038764,_6038770):-!,hnf(_6032766,_6041160,_6038764,_6041184),'blocked_blocked_blocked_Interval.cineg_1_Interval.CI_1_Interval.IV_3'(_6041160,_6032894,_6032912,_6038758,_6041184,_6038770).

'blocked_blocked_blocked_Interval.cineg_1_Interval.CI_1_Interval.IV_3'(_6041808,_6041810,_6041812,_6041814,_6041816,_6041818):-freeze(_6041816,'blocked_blocked_blocked_blocked_Interval.cineg_1_Interval.CI_1_Interval.IV_3'(_6041808,_6041810,_6041812,_6041814,_6041816,_6041818)).
'blocked_blocked_blocked_blocked_Interval.cineg_1_Interval.CI_1_Interval.IV_3'('Interval.IV'(_6033040,_6033058),_6032894,_6032912,'Interval.CI'('Interval.IV'('Rational.ratNeg'(_6032912),'Rational.ratNeg'(_6032894)),'Interval.IV'('Rational.ratNeg'(_6033058),'Rational.ratNeg'(_6033040))),_6042218,_6042218):-!.
'blocked_blocked_blocked_blocked_Interval.cineg_1_Interval.CI_1_Interval.IV_3'('FAIL'(_6044740),_6032894,_6032912,'FAIL'(_6044740),_6044754,_6044754):-nonvar(_6044740).
'blocked_blocked_blocked_Interval.cineg_1_Interval.CI_1'('FAIL'(_6044826),_6032766,'FAIL'(_6044826),_6044840,_6044840):-nonvar(_6044826).
'blocked_blocked_Interval.cineg_1'('FAIL'(_6044904),'FAIL'(_6044904),_6044918,_6044918):-nonvar(_6044904).

'Interval.cimul'(_6045634,_6045636,_6045638,_6045640,_6045642):-freeze(_6045640,'blocked_Interval.cimul'(_6045634,_6045636,_6045638,_6045640,_6045642)).
'blocked_Interval.cimul'(_6045730,_6045748,_6047308,_6047314,_6047320):-hnf(_6045730,_6048198,_6047314,_6048216),'blocked_Interval.cimul_1'(_6048198,_6045748,_6047308,_6048216,_6047320).

'blocked_Interval.cimul_1'(_6048550,_6048552,_6048554,_6048556,_6048558):-freeze(_6048556,'blocked_blocked_Interval.cimul_1'(_6048550,_6048552,_6048554,_6048556,_6048558)).
'blocked_blocked_Interval.cimul_1'('Interval.CI'(_6045864,_6045882),_6045748,_6049182,_6049188,_6049194):-!,hnf(_6045748,_6050792,_6049188,_6050816),'blocked_blocked_Interval.cimul_1_Interval.CI_3'(_6050792,_6045864,_6045882,_6049182,_6050816,_6049194).

'blocked_blocked_Interval.cimul_1_Interval.CI_3'(_6051308,_6051310,_6051312,_6051314,_6051316,_6051318):-freeze(_6051316,'blocked_blocked_blocked_Interval.cimul_1_Interval.CI_3'(_6051308,_6051310,_6051312,_6051314,_6051316,_6051318)).
'blocked_blocked_blocked_Interval.cimul_1_Interval.CI_3'('Interval.CI'(_6046010,_6046028),_6045864,_6045882,'Interval.CI'('Interval.isub'('Interval.imul'(_6052054,_6052074),'Interval.imul'(_6052094,_6052114)),'Interval.iadd'('Interval.imul'(_6052054,_6052114),'Interval.imul'(_6052094,_6052074))),_6051718,_6051724):-!,makeShare(_6045864,_6052054),makeShare(_6046010,_6052074),makeShare(_6045882,_6052094),makeShare(_6046028,_6052114),_6051718=_6051724.
'blocked_blocked_blocked_Interval.cimul_1_Interval.CI_3'('FAIL'(_6055496),_6045864,_6045882,'FAIL'(_6055496),_6055510,_6055510):-nonvar(_6055496).
'blocked_blocked_Interval.cimul_1'('FAIL'(_6055582),_6045748,'FAIL'(_6055582),_6055596,_6055596):-nonvar(_6055582).

'Interval.ciinv'(_6056320,_6056322,_6056324,_6056326):-freeze(_6056324,'blocked_Interval.ciinv'(_6056320,_6056322,_6056324,_6056326)).
'blocked_Interval.ciinv'(_6056406,_6059156,_6059162,_6059168):-hnf(_6056406,_6060038,_6059162,_6060050),'blocked_Interval.ciinv_1'(_6060038,_6059156,_6060050,_6059168).

'blocked_Interval.ciinv_1'(_6060376,_6060378,_6060380,_6060382):-freeze(_6060380,'blocked_blocked_Interval.ciinv_1'(_6060376,_6060378,_6060380,_6060382)).
'blocked_blocked_Interval.ciinv_1'('Interval.CI'(_6056522,_6056540),_6060760,_6060766,_6060772):-!,makeShare(_6056580,_6061346),makeShare(_6056522,_6061366),makeShare(_6056540,_6061386),makeShare(_6056598,_6061406),makeShare(_6056616,_6061426),hnf('Prelude.cond'(letrec4PAKCS(_6061346,'Interval.iadd'('Interval.imul'(_6061366,_6061366),'Interval.imul'(_6061386,_6061386))),'Prelude.cond'(letrec4PAKCS(_6061406,'Interval.ciinv._\'23selFP10\'23il'(_6061386)),'Prelude.cond'(letrec4PAKCS(_6061426,'Interval.ciinv._\'23selFP11\'23ih'(_6061386)),'Interval.CI'('Interval.idiv'(_6061366,_6061346),'Interval.idiv'('Interval.IV'('Rational.ratNeg'(_6061426),'Rational.ratNeg'(_6061406)),_6061346))))),_6060760,_6060766,_6060772).
'blocked_blocked_Interval.ciinv_1'('FAIL'(_6067508),'FAIL'(_6067508),_6067522,_6067522):-nonvar(_6067508).

'Interval.ciinv._\'23selFP10\'23il'(_6068788,_6068790,_6068792,_6068794):-freeze(_6068792,'blocked_Interval.ciinv._\'23selFP10\'23il'(_6068788,_6068790,_6068792,_6068794)).
'blocked_Interval.ciinv._\'23selFP10\'23il'(_6068874,_6069306,_6069312,_6069318):-hnf(_6068874,_6070800,_6069312,_6070812),'blocked_Interval.ciinv._\'23selFP10\'23il_1'(_6070800,_6069306,_6070812,_6069318).

'blocked_Interval.ciinv._\'23selFP10\'23il_1'(_6071240,_6071242,_6071244,_6071246):-freeze(_6071244,'blocked_blocked_Interval.ciinv._\'23selFP10\'23il_1'(_6071240,_6071242,_6071244,_6071246)).
'blocked_blocked_Interval.ciinv._\'23selFP10\'23il_1'('Interval.IV'(_6068990,_6069008),_6071624,_6071630,_6071636):-!,hnf(_6068990,_6071624,_6071630,_6071636).
'blocked_blocked_Interval.ciinv._\'23selFP10\'23il_1'('FAIL'(_6072190),'FAIL'(_6072190),_6072204,_6072204):-nonvar(_6072190).

'Interval.ciinv._\'23selFP11\'23ih'(_6073470,_6073472,_6073474,_6073476):-freeze(_6073474,'blocked_Interval.ciinv._\'23selFP11\'23ih'(_6073470,_6073472,_6073474,_6073476)).
'blocked_Interval.ciinv._\'23selFP11\'23ih'(_6073556,_6073988,_6073994,_6074000):-hnf(_6073556,_6075482,_6073994,_6075494),'blocked_Interval.ciinv._\'23selFP11\'23ih_1'(_6075482,_6073988,_6075494,_6074000).

'blocked_Interval.ciinv._\'23selFP11\'23ih_1'(_6075922,_6075924,_6075926,_6075928):-freeze(_6075926,'blocked_blocked_Interval.ciinv._\'23selFP11\'23ih_1'(_6075922,_6075924,_6075926,_6075928)).
'blocked_blocked_Interval.ciinv._\'23selFP11\'23ih_1'('Interval.IV'(_6073672,_6073690),_6076306,_6076312,_6076318):-!,hnf(_6073690,_6076306,_6076312,_6076318).
'blocked_blocked_Interval.ciinv._\'23selFP11\'23ih_1'('FAIL'(_6076872),'FAIL'(_6076872),_6076886,_6076886):-nonvar(_6076872).

'Interval.cipow'(_6077602,_6077604,_6077606,_6077608,_6077610):-freeze(_6077608,'blocked_Interval.cipow'(_6077602,_6077604,_6077606,_6077608,_6077610)).
'blocked_Interval.cipow'(_6077698,_6077716,_6084728,_6084734,_6084740):-makeShare(_6077716,_6082300),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6082300,0),_6085720,_6084734,_6085686),'blocked_Interval.cipow_ComplexCase'(_6085720,_6077698,_6082300,_6084728,_6085686,_6084740).

'blocked_Interval.cipow_ComplexCase'(_6086106,_6086108,_6086110,_6086112,_6086114,_6086116):-freeze(_6086114,freeze(_6086106,'blocked_blocked_Interval.cipow_ComplexCase'(_6086106,_6086108,_6086110,_6086112,_6086114,_6086116))).
'blocked_blocked_Interval.cipow_ComplexCase'('Prelude.True',_6077698,_6082300,_6086510,_6086516,_6086522):-hnf('Interval.ciFromRat'('Prelude.apply'('Interval.ri',1)),_6086510,_6086516,_6086522).
'blocked_blocked_Interval.cipow_ComplexCase'('Prelude.False',_6077698,_6082300,_6090642,_6090648,_6090654):-!,makeShare(_6082300,_6088330),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6088330,1),_6092782,_6090648,_6092748),'blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase'(_6092782,_6077698,_6088330,_6090642,_6092748,_6090654).

'blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase'(_6093390,_6093392,_6093394,_6093396,_6093398,_6093400):-freeze(_6093398,freeze(_6093390,'blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase'(_6093390,_6093392,_6093394,_6093396,_6093398,_6093400))).
'blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6077698,_6088330,_6093794,_6093800,_6093806):-hnf(_6077698,_6093794,_6093800,_6093806).
'blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6077698,_6088330,_6097856,_6097862,_6097868):-!,makeShare(_6088330,_6095204),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_6095204),0),_6101220,_6097862,_6101186),'blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6101220,_6077698,_6095204,_6097856,_6101186,_6097868).

'blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6102032,_6102034,_6102036,_6102038,_6102040,_6102042):-freeze(_6102040,freeze(_6102032,'blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6102032,_6102034,_6102036,_6102038,_6102040,_6102042))).
'blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6077698,_6095204,_6102436,_6102442,_6102448):-hnf('Interval.cipow'('Interval.ciinv'(_6077698),'Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_6095204)),_6102436,_6102442,_6102448).
'blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6077698,_6095204,_6107238,_6107244,_6107250):-!,makeShare(_6095204,_6104904),hnf('Prelude.even'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[]),_6104904),_6111826,_6107244,_6111792),'blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6111826,_6077698,_6104904,_6107238,_6111792,_6107250).

'blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6112842,_6112844,_6112846,_6112848,_6112850,_6112852):-freeze(_6112850,freeze(_6112842,'blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6112842,_6112844,_6112846,_6112848,_6112850,_6112852))).
'blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6077698,_6104904,_6113246,_6113252,_6113258):-makeShare(_6079766,_6113440),hnf('Prelude.cond'(letrec4PAKCS(_6113440,'Interval.cipow'(_6077698,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23div\'23Prelude.Integral\'23Prelude.Int\'23',_6104904),2))),'Interval.cimul'(_6113440,_6113440)),_6113246,_6113252,_6113258).
'blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6077698,_6104904,_6118622,_6118628,_6118634):-!,hnf('Prelude.otherwise',_6124434,_6118628,_6124400),'blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6124434,_6077698,_6104904,_6118622,_6124400,_6118634).

'blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6125642,_6125644,_6125646,_6125648,_6125650,_6125652):-freeze(_6125650,freeze(_6125642,'blocked_blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6125642,_6125644,_6125646,_6125648,_6125650,_6125652))).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6077698,_6104904,_6126046,_6126052,_6126058):-makeShare(_6077698,_6126180),hnf('Interval.cimul'(_6126180,'Interval.cipow'(_6126180,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_6104904,1))),_6126046,_6126052,_6126058).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6077698,_6104904,_6129010,_6129016,_6129022):-!,hnf(reportFailure4PAKCS('Interval.cipow',['Prelude.False']),_6129010,_6129016,_6129022).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6131088),_6077698,_6104904,'FAIL'(_6131088),_6131102,_6131102).
'blocked_blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6131170),_6077698,_6104904,'FAIL'(_6131170),_6131184,_6131184).
'blocked_blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6131252),_6077698,_6095204,'FAIL'(_6131252),_6131266,_6131266).
'blocked_blocked_blocked_Interval.cipow_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6131334),_6077698,_6088330,'FAIL'(_6131334),_6131348,_6131348).
'blocked_blocked_Interval.cipow_ComplexCase'('FAIL'(_6131416),_6077698,_6082300,'FAIL'(_6131416),_6131430,_6131430).

'Interval.ciMagnitudeSq'(_6132462,_6132464,_6132466,_6132468):-freeze(_6132466,'blocked_Interval.ciMagnitudeSq'(_6132462,_6132464,_6132466,_6132468)).
'blocked_Interval.ciMagnitudeSq'(_6132548,_6133388,_6133394,_6133400):-hnf(_6132548,_6134558,_6133394,_6134570),'blocked_Interval.ciMagnitudeSq_1'(_6134558,_6133388,_6134570,_6133400).

'blocked_Interval.ciMagnitudeSq_1'(_6134944,_6134946,_6134948,_6134950):-freeze(_6134948,'blocked_blocked_Interval.ciMagnitudeSq_1'(_6134944,_6134946,_6134948,_6134950)).
'blocked_blocked_Interval.ciMagnitudeSq_1'('Interval.CI'(_6132664,_6132682),_6135328,_6135334,_6135340):-!,makeShare(_6132664,_6135470),makeShare(_6132682,_6135490),hnf('Interval.iadd'('Interval.imul'(_6135470,_6135470),'Interval.imul'(_6135490,_6135490)),_6135328,_6135334,_6135340).
'blocked_blocked_Interval.ciMagnitudeSq_1'('FAIL'(_6137180),'FAIL'(_6137180),_6137194,_6137194):-nonvar(_6137180).

'Interval.ciRealPart'(_6138100,_6138102,_6138104,_6138106):-freeze(_6138104,'blocked_Interval.ciRealPart'(_6138100,_6138102,_6138104,_6138106)).
'blocked_Interval.ciRealPart'(_6138186,_6138546,_6138552,_6138558):-hnf(_6138186,_6139608,_6138552,_6139620),'blocked_Interval.ciRealPart_1'(_6139608,_6138546,_6139620,_6138558).

'blocked_Interval.ciRealPart_1'(_6139976,_6139978,_6139980,_6139982):-freeze(_6139980,'blocked_blocked_Interval.ciRealPart_1'(_6139976,_6139978,_6139980,_6139982)).
'blocked_blocked_Interval.ciRealPart_1'('Interval.CI'(_6138302,_6138320),_6140360,_6140366,_6140372):-!,hnf(_6138302,_6140360,_6140366,_6140372).
'blocked_blocked_Interval.ciRealPart_1'('FAIL'(_6140854),'FAIL'(_6140854),_6140868,_6140868):-nonvar(_6140854).

'Interval.ciImagPart'(_6141774,_6141776,_6141778,_6141780):-freeze(_6141778,'blocked_Interval.ciImagPart'(_6141774,_6141776,_6141778,_6141780)).
'blocked_Interval.ciImagPart'(_6141860,_6142220,_6142226,_6142232):-hnf(_6141860,_6143282,_6142226,_6143294),'blocked_Interval.ciImagPart_1'(_6143282,_6142220,_6143294,_6142232).

'blocked_Interval.ciImagPart_1'(_6143650,_6143652,_6143654,_6143656):-freeze(_6143654,'blocked_blocked_Interval.ciImagPart_1'(_6143650,_6143652,_6143654,_6143656)).
'blocked_blocked_Interval.ciImagPart_1'('Interval.CI'(_6141976,_6141994),_6144034,_6144040,_6144046):-!,hnf(_6141994,_6144034,_6144040,_6144046).
'blocked_blocked_Interval.ciImagPart_1'('FAIL'(_6144528),'FAIL'(_6144528),_6144542,_6144542):-nonvar(_6144528).

'Interval.piInterval'(_6145448,_6145450,_6145452):-freeze(_6145450,'blocked_Interval.piInterval'(_6145448,_6145450,_6145452)).
'blocked_Interval.piInterval'(_6148292,_6148298,_6148304):-makeShare(_6145534,_6148590),makeShare(_6145552,_6148610),makeShare(_6145570,_6148630),hnf('Prelude.cond'(letrec4PAKCS(_6148590,'Interval.iatanSmall'('Prelude.apply'('Prelude.apply'('Rational.rat',1),5))),'Prelude.cond'(letrec4PAKCS(_6148610,'Interval.iatanSmall'('Prelude.apply'('Prelude.apply'('Rational.rat',1),239))),'Prelude.cond'(letrec4PAKCS(_6148630,'Interval.IV'('Prelude.apply'('Interval.ri',4),'Prelude.apply'('Interval.ri',4))),'Interval.imul'(_6148630,'Interval.isub'('Interval.imul'(_6148630,_6148590),_6148610))))),_6148292,_6148298,_6148304).

'Interval.iatanSmall'(_6155586,_6155588,_6155590,_6155592):-freeze(_6155590,'blocked_Interval.iatanSmall'(_6155586,_6155588,_6155590,_6155592)).
'blocked_Interval.iatanSmall'(_6155672,_6159284,_6159290,_6159296):-makeShare(_6155700,_6159732),makeShare(_6155672,_6159752),makeShare(_6155718,_6159772),makeShare(_6155736,_6159792),hnf('Prelude.cond'(letrec4PAKCS(_6159732,'Prelude.foldl'(partcall(2,'Rational.ratAdd',[]),'Prelude.apply'('Interval.ri',0),'Prelude.map'(partcall(1,'Interval.iatanSmall._\'23lambda2',[_6159752]),'Prelude._impl\'23enumFromTo\'23Prelude.Enum\'23Prelude.Int\'23'(0,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(50,1))))),'Prelude.cond'(letrec4PAKCS(_6159772,'Rational.ratAbs'(_6159752)),'Prelude.cond'(letrec4PAKCS(_6159792,'Rational.ratDiv'('Rational.ratPow'(_6159772,'Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(2,50),1)),'Prelude.apply'('Interval.ri','Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(2,50),1)))),'Interval.IV'('Rational.ratSub'(_6159732,_6159792),'Rational.ratAdd'(_6159732,_6159792))))),_6159284,_6159290,_6159296).

'Interval.iatanSmall._\'23lambda2'(_6170260,_6170262,_6170264,_6170266,_6170268):-freeze(_6170266,'blocked_Interval.iatanSmall._\'23lambda2'(_6170260,_6170262,_6170264,_6170266,_6170268)).
'blocked_Interval.iatanSmall._\'23lambda2'(_6170356,_6170374,_6171888,_6171894,_6171900):-makeShare(_6170374,_6172052),hnf('Rational.ratMul'('Interval.iatanSmall._\'23lambda2._\'23caseor0'('Prelude.even'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[]),_6172052)),'Rational.ratDiv'('Rational.ratPow'(_6170356,'Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(2,_6172052),1)),'Prelude.apply'('Interval.ri','Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(2,_6172052),1)))),_6171888,_6171894,_6171900).

'Interval.iatanPoint'(_6177658,_6177660,_6177662,_6177664):-freeze(_6177662,'blocked_Interval.iatanPoint'(_6177658,_6177660,_6177662,_6177664)).
'blocked_Interval.iatanPoint'(_6177744,_6189968,_6189974,_6189980):-makeShare(_6177744,_6186172),hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_6186172),'Prelude.apply'('Interval.ri',0)),_6191126,_6189974,_6191098),'blocked_Interval.iatanPoint_ComplexCase'(_6191126,_6186172,_6189968,_6191098,_6189980).

'blocked_Interval.iatanPoint_ComplexCase'(_6191540,_6191542,_6191544,_6191546,_6191548):-freeze(_6191546,freeze(_6191540,'blocked_blocked_Interval.iatanPoint_ComplexCase'(_6191540,_6191542,_6191544,_6191546,_6191548))).
'blocked_blocked_Interval.iatanPoint_ComplexCase'('Prelude.True',_6186172,'Interval.IV'('Prelude.apply'('Interval.ri',0),'Prelude.apply'('Interval.ri',0)),_6191940,_6191940).
'blocked_blocked_Interval.iatanPoint_ComplexCase'('Prelude.False',_6186172,_6197590,_6197596,_6197602):-!,makeShare(_6186172,_6194436),hnf('Rational.ratLe'('Rational.ratAbs'(_6194436),'Prelude.apply'('Interval.ri',1)),_6199896,_6197596,_6199868),'blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase'(_6199896,_6194436,_6197590,_6199868,_6197602).

'blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase'(_6200532,_6200534,_6200536,_6200538,_6200540):-freeze(_6200538,freeze(_6200532,'blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase'(_6200532,_6200534,_6200536,_6200538,_6200540))).
'blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6194436,_6200926,_6200932,_6200938):-hnf('Interval.iatanSmall'(_6194436),_6200926,_6200932,_6200938).
'blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6194436,_6205788,_6205794,_6205800):-!,makeShare(_6194436,_6202864),hnf('Rational.ratGt'(_6202864,'Prelude.apply'('Interval.ri',0)),_6209318,_6205794,_6209290),'blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6209318,_6202864,_6205788,_6209290,_6205800).

'blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6210158,_6210160,_6210162,_6210164,_6210166):-freeze(_6210164,freeze(_6210158,'blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6210158,_6210160,_6210162,_6210164,_6210166))).
'blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6202864,_6210552,_6210558,_6210564):-makeShare(_6180030,_6210704),hnf('Prelude.cond'(letrec4PAKCS(_6210704,'Interval.idiv'('Interval.piInterval','Interval.IV'('Prelude.apply'('Interval.ri',2),'Prelude.apply'('Interval.ri',2)))),'Interval.isub'(_6210704,'Interval.iatanSmall'('Rational.ratDiv'('Prelude.apply'('Interval.ri',1),_6202864)))),_6210552,_6210558,_6210564).
'blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6202864,_6217484,_6217490,_6217496):-!,hnf('Prelude.otherwise',_6222238,_6217490,_6222210),'blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6222238,_6202864,_6217484,_6222210,_6217496).

'blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6223270,_6223272,_6223274,_6223276,_6223278):-freeze(_6223276,freeze(_6223270,'blocked_blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6223270,_6223272,_6223274,_6223276,_6223278))).
'blocked_blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6202864,_6223664,_6223670,_6223676):-makeShare(_6181788,_6224118),makeShare(_6181806,_6224138),makeShare(_6181824,_6224158),makeShare(_6181842,_6224178),hnf('Prelude.cond'(letrec4PAKCS(_6224118,'Interval.idiv'('Interval.piInterval','Interval.IV'('Prelude.apply'('Interval.ri',2),'Prelude.apply'('Interval.ri',2)))),'Prelude.cond'(letrec4PAKCS(_6224138,'Interval.iatanPoint._\'23selFP13\'23pl'(_6224118)),'Prelude.cond'(letrec4PAKCS(_6224158,'Interval.iatanPoint._\'23selFP14\'23ph'(_6224118)),'Prelude.cond'(letrec4PAKCS(_6224178,'Interval.IV'('Rational.ratNeg'(_6224158),'Rational.ratNeg'(_6224138))),'Interval.iadd'(_6224178,'Interval.iatanSmall'('Rational.ratDiv'('Prelude.apply'('Interval.ri',1),'Rational.ratNeg'(_6202864)))))))),_6223664,_6223670,_6223676).
'blocked_blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6202864,_6232724,_6232730,_6232736):-!,hnf(reportFailure4PAKCS('Interval.iatanPoint',['Prelude.False']),_6232724,_6232730,_6232736).
'blocked_blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6234584),_6202864,'FAIL'(_6234584),_6234598,_6234598).
'blocked_blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6234658),_6202864,'FAIL'(_6234658),_6234672,_6234672).
'blocked_blocked_blocked_Interval.iatanPoint_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6234732),_6194436,'FAIL'(_6234732),_6234746,_6234746).
'blocked_blocked_Interval.iatanPoint_ComplexCase'('FAIL'(_6234806),_6186172,'FAIL'(_6234806),_6234820,_6234820).

'Interval.iatanPoint._\'23selFP13\'23pl'(_6236280,_6236282,_6236284,_6236286):-freeze(_6236284,'blocked_Interval.iatanPoint._\'23selFP13\'23pl'(_6236280,_6236282,_6236284,_6236286)).
'blocked_Interval.iatanPoint._\'23selFP13\'23pl'(_6236366,_6236828,_6236834,_6236840):-hnf(_6236366,_6238502,_6236834,_6238514),'blocked_Interval.iatanPoint._\'23selFP13\'23pl_1'(_6238502,_6236828,_6238514,_6236840).

'blocked_Interval.iatanPoint._\'23selFP13\'23pl_1'(_6238972,_6238974,_6238976,_6238978):-freeze(_6238976,'blocked_blocked_Interval.iatanPoint._\'23selFP13\'23pl_1'(_6238972,_6238974,_6238976,_6238978)).
'blocked_blocked_Interval.iatanPoint._\'23selFP13\'23pl_1'('Interval.IV'(_6236482,_6236500),_6239356,_6239362,_6239368):-!,hnf(_6236482,_6239356,_6239362,_6239368).
'blocked_blocked_Interval.iatanPoint._\'23selFP13\'23pl_1'('FAIL'(_6239952),'FAIL'(_6239952),_6239966,_6239966):-nonvar(_6239952).

'Interval.iatanPoint._\'23selFP14\'23ph'(_6241422,_6241424,_6241426,_6241428):-freeze(_6241426,'blocked_Interval.iatanPoint._\'23selFP14\'23ph'(_6241422,_6241424,_6241426,_6241428)).
'blocked_Interval.iatanPoint._\'23selFP14\'23ph'(_6241508,_6241970,_6241976,_6241982):-hnf(_6241508,_6243644,_6241976,_6243656),'blocked_Interval.iatanPoint._\'23selFP14\'23ph_1'(_6243644,_6241970,_6243656,_6241982).

'blocked_Interval.iatanPoint._\'23selFP14\'23ph_1'(_6244114,_6244116,_6244118,_6244120):-freeze(_6244118,'blocked_blocked_Interval.iatanPoint._\'23selFP14\'23ph_1'(_6244114,_6244116,_6244118,_6244120)).
'blocked_blocked_Interval.iatanPoint._\'23selFP14\'23ph_1'('Interval.IV'(_6241624,_6241642),_6244498,_6244504,_6244510):-!,hnf(_6241642,_6244498,_6244504,_6244510).
'blocked_blocked_Interval.iatanPoint._\'23selFP14\'23ph_1'('FAIL'(_6245094),'FAIL'(_6245094),_6245108,_6245108):-nonvar(_6245094).

'Interval.iatan'(_6245824,_6245826,_6245828,_6245830):-freeze(_6245828,'blocked_Interval.iatan'(_6245824,_6245826,_6245828,_6245830)).
'blocked_Interval.iatan'(_6245910,_6248068,_6248074,_6248080):-hnf(_6245910,_6248950,_6248074,_6248962),'blocked_Interval.iatan_1'(_6248950,_6248068,_6248962,_6248080).

'blocked_Interval.iatan_1'(_6249288,_6249290,_6249292,_6249294):-freeze(_6249292,'blocked_blocked_Interval.iatan_1'(_6249288,_6249290,_6249292,_6249294)).
'blocked_blocked_Interval.iatan_1'('Interval.IV'(_6246026,_6246044),_6249672,_6249678,_6249684):-!,makeShare(_6246084,_6250194),makeShare(_6246102,_6250214),makeShare(_6246120,_6250234),makeShare(_6246138,_6250254),hnf('Prelude.cond'(letrec4PAKCS(_6250194,'Interval.iatanPoint'(_6246026)),'Prelude.cond'(letrec4PAKCS(_6250214,'Interval.iatan._\'23selFP18\'23al'(_6250194)),'Prelude.cond'(letrec4PAKCS(_6250234,'Interval.iatanPoint'(_6246044)),'Prelude.cond'(letrec4PAKCS(_6250254,'Interval.iatan._\'23selFP17\'23bh'(_6250234)),'Interval.IV'(_6250214,_6250254))))),_6249672,_6249678,_6249684).
'blocked_blocked_Interval.iatan_1'('FAIL'(_6255174),'FAIL'(_6255174),_6255188,_6255188):-nonvar(_6255174).

'Interval.iatan._\'23selFP18\'23al'(_6256454,_6256456,_6256458,_6256460):-freeze(_6256458,'blocked_Interval.iatan._\'23selFP18\'23al'(_6256454,_6256456,_6256458,_6256460)).
'blocked_Interval.iatan._\'23selFP18\'23al'(_6256540,_6256972,_6256978,_6256984):-hnf(_6256540,_6258466,_6256978,_6258478),'blocked_Interval.iatan._\'23selFP18\'23al_1'(_6258466,_6256972,_6258478,_6256984).

'blocked_Interval.iatan._\'23selFP18\'23al_1'(_6258906,_6258908,_6258910,_6258912):-freeze(_6258910,'blocked_blocked_Interval.iatan._\'23selFP18\'23al_1'(_6258906,_6258908,_6258910,_6258912)).
'blocked_blocked_Interval.iatan._\'23selFP18\'23al_1'('Interval.IV'(_6256656,_6256674),_6259290,_6259296,_6259302):-!,hnf(_6256656,_6259290,_6259296,_6259302).
'blocked_blocked_Interval.iatan._\'23selFP18\'23al_1'('FAIL'(_6259856),'FAIL'(_6259856),_6259870,_6259870):-nonvar(_6259856).

'Interval.iatan._\'23selFP17\'23bh'(_6261136,_6261138,_6261140,_6261142):-freeze(_6261140,'blocked_Interval.iatan._\'23selFP17\'23bh'(_6261136,_6261138,_6261140,_6261142)).
'blocked_Interval.iatan._\'23selFP17\'23bh'(_6261222,_6261654,_6261660,_6261666):-hnf(_6261222,_6263148,_6261660,_6263160),'blocked_Interval.iatan._\'23selFP17\'23bh_1'(_6263148,_6261654,_6263160,_6261666).

'blocked_Interval.iatan._\'23selFP17\'23bh_1'(_6263588,_6263590,_6263592,_6263594):-freeze(_6263592,'blocked_blocked_Interval.iatan._\'23selFP17\'23bh_1'(_6263588,_6263590,_6263592,_6263594)).
'blocked_blocked_Interval.iatan._\'23selFP17\'23bh_1'('Interval.IV'(_6261338,_6261356),_6263972,_6263978,_6263984):-!,hnf(_6261356,_6263972,_6263978,_6263984).
'blocked_blocked_Interval.iatan._\'23selFP17\'23bh_1'('FAIL'(_6264538),'FAIL'(_6264538),_6264552,_6264552):-nonvar(_6264538).

'Interval.icosPoint'(_6265420,_6265422,_6265424,_6265426):-freeze(_6265424,'blocked_Interval.icosPoint'(_6265420,_6265422,_6265424,_6265426)).
'blocked_Interval.icosPoint'(_6265506,_6268394,_6268400,_6268406):-makeShare(_6265534,_6268716),makeShare(_6265506,_6268736),makeShare(_6265552,_6268756),hnf('Prelude.cond'(letrec4PAKCS(_6268716,'Prelude.foldl'(partcall(2,'Rational.ratAdd',[]),'Prelude.apply'('Interval.ri',0),'Prelude.map'(partcall(1,'Interval.icosPoint._\'23lambda3',[_6268736]),'Prelude._impl\'23enumFromTo\'23Prelude.Enum\'23Prelude.Int\'23'(0,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(30,1))))),'Prelude.cond'(letrec4PAKCS(_6268756,'Rational.ratDiv'('Rational.ratPow'('Rational.ratAbs'(_6268736),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(2,30)),'Interval.factorial'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(2,30)))),'Interval.IV'('Rational.ratSub'(_6268716,_6268756),'Rational.ratAdd'(_6268716,_6268756)))),_6268394,_6268400,_6268406).

'Interval.icosPoint._\'23lambda3'(_6277254,_6277256,_6277258,_6277260,_6277262):-freeze(_6277260,'blocked_Interval.icosPoint._\'23lambda3'(_6277254,_6277256,_6277258,_6277260,_6277262)).
'blocked_Interval.icosPoint._\'23lambda3'(_6277350,_6277368,_6278490,_6278496,_6278502):-makeShare(_6277368,_6278644),hnf('Rational.ratMul'('Interval.icosPoint._\'23lambda3._\'23caseor0'('Prelude.even'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[]),_6278644)),'Rational.ratDiv'('Rational.ratPow'(_6277350,'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(2,_6278644)),'Interval.factorial'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(2,_6278644)))),_6278490,_6278496,_6278502).

'Interval.factorial'(_6283078,_6283080,_6283082,_6283084):-freeze(_6283082,'blocked_Interval.factorial'(_6283078,_6283080,_6283082,_6283084)).
'blocked_Interval.factorial'(_6283164,_6283670,_6283676,_6283682):-hnf('Prelude.apply'('Interval.ri','Interval.factorial.go.143'(partcall(1,'Prelude._inst\'23Prelude.Eq\'23Prelude.Int\'23',[]),partcall(1,'Prelude._inst\'23Prelude.Num\'23Prelude.Int\'23',[]),1,_6283164)),_6283670,_6283676,_6283682).

'Interval.factorial.go.143'(_6286548,_6286550,_6286552,_6286554,_6286556,_6286558,_6286560):-freeze(_6286558,'blocked_Interval.factorial.go.143'(_6286548,_6286550,_6286552,_6286554,_6286556,_6286558,_6286560)).
'blocked_Interval.factorial.go.143'(_6286664,_6286682,_6286700,_6286718,_6292900,_6292906,_6292912):-makeShare(_6286664,_6289906),makeShare(_6286718,_6289926),makeShare(_6286682,_6289946),hnf('Prelude.apply'('Prelude.apply'('Prelude.=='(_6289906),_6289926),'Prelude.apply'('Prelude.fromInt'(_6289946),0)),_6294316,_6292906,_6294270),'blocked_Interval.factorial.go.143_ComplexCase'(_6294316,_6289906,_6289946,_6286700,_6289926,_6292900,_6294270,_6292912).

'blocked_Interval.factorial.go.143_ComplexCase'(_6294796,_6294798,_6294800,_6294802,_6294804,_6294806,_6294808,_6294810):-freeze(_6294808,freeze(_6294796,'blocked_blocked_Interval.factorial.go.143_ComplexCase'(_6294796,_6294798,_6294800,_6294802,_6294804,_6294806,_6294808,_6294810))).
'blocked_blocked_Interval.factorial.go.143_ComplexCase'('Prelude.True',_6289906,_6289946,_6286700,_6289926,_6295220,_6295226,_6295232):-hnf(_6286700,_6295220,_6295226,_6295232).
'blocked_blocked_Interval.factorial.go.143_ComplexCase'('Prelude.False',_6289906,_6289946,_6286700,_6289926,_6297746,_6297752,_6297758):-!,hnf('Prelude.otherwise',_6300310,_6297752,_6300264),'blocked_blocked_Interval.factorial.go.143_ComplexCase_Prelude.False_ComplexCase'(_6300310,_6289906,_6289946,_6286700,_6289926,_6297746,_6300264,_6297758).

'blocked_blocked_Interval.factorial.go.143_ComplexCase_Prelude.False_ComplexCase'(_6300976,_6300978,_6300980,_6300982,_6300984,_6300986,_6300988,_6300990):-freeze(_6300988,freeze(_6300976,'blocked_blocked_blocked_Interval.factorial.go.143_ComplexCase_Prelude.False_ComplexCase'(_6300976,_6300978,_6300980,_6300982,_6300984,_6300986,_6300988,_6300990))).
'blocked_blocked_blocked_Interval.factorial.go.143_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6289906,_6289946,_6286700,_6289926,_6301400,_6301406,_6301412):-makeShare(_6289946,_6301764),makeShare(_6289926,_6301784),hnf('Interval.factorial.go.143'(_6289906,_6301764,'Prelude.apply'('Prelude.apply'('Prelude.*'(_6301764),_6286700),_6301784),'Prelude.apply'('Prelude.apply'('Prelude.-'(_6301764),_6301784),'Prelude.apply'('Prelude.fromInt'(_6301764),1))),_6301400,_6301406,_6301412).
'blocked_blocked_blocked_Interval.factorial.go.143_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6289906,_6289946,_6286700,_6289926,_6305802,_6305808,_6305814):-!,hnf(reportFailure4PAKCS('Interval.factorial.go.143',['Prelude.False']),_6305802,_6305808,_6305814).
'blocked_blocked_blocked_Interval.factorial.go.143_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6307548),_6289906,_6289946,_6286700,_6289926,'FAIL'(_6307548),_6307562,_6307562).
'blocked_blocked_Interval.factorial.go.143_ComplexCase'('FAIL'(_6307646),_6289906,_6289946,_6286700,_6289926,'FAIL'(_6307646),_6307660,_6307660).

'Interval.icos'(_6308366,_6308368,_6308370,_6308372):-freeze(_6308370,'blocked_Interval.icos'(_6308366,_6308368,_6308370,_6308372)).
'blocked_Interval.icos'(_6308452,_6323094,_6323100,_6323106):-makeShare(_6308452,_6319506),hnf(_6319506,_6323940,_6323100,_6323958),'blocked_Interval.icos_1'(_6323940,_6323940,_6323094,_6323958,_6323106).

'blocked_Interval.icos_1'(_6324298,_6324300,_6324302,_6324304,_6324306):-freeze(_6324304,'blocked_blocked_Interval.icos_1'(_6324298,_6324300,_6324302,_6324304,_6324306)).
'blocked_blocked_Interval.icos_1'('Interval.IV'(_6308568,_6308586),_6319506,_6333806,_6333812,_6333818):-!,makeShare(_6308568,_6329038),makeShare(_6308586,_6329058),hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_6329038),_6329058),_6335488,_6333812,_6335448),'blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase'(_6335488,_6329038,_6329058,_6319506,_6333806,_6335448,_6333818).

'blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase'(_6336032,_6336034,_6336036,_6336038,_6336040,_6336042,_6336044):-freeze(_6336042,freeze(_6336032,'blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase'(_6336032,_6336034,_6336036,_6336038,_6336040,_6336042,_6336044))).
'blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase'('Prelude.True',_6329038,_6329058,_6319506,_6336446,_6336452,_6336458):-hnf('Interval.icosPoint'(_6329038),_6336446,_6336452,_6336458).
'blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase'('Prelude.False',_6329038,_6329058,_6319506,_6346686,_6346692,_6346698):-!,hnf('Rational.ratGt'('Interval.width'(_6319506),'Rational.ratMul'('Interval.ivHi'('Interval.piInterval'),'Prelude.apply'('Interval.ri',2))),_6349596,_6346692,_6349556),'blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_6349596,_6329038,_6329058,_6319506,_6346686,_6349556,_6346698).

'blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_6350320,_6350322,_6350324,_6350326,_6350328,_6350330,_6350332):-freeze(_6350330,freeze(_6350320,'blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'(_6350320,_6350322,_6350324,_6350326,_6350328,_6350330,_6350332))).
'blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6329038,_6329058,_6319506,'Interval.IV'('Prelude.apply'('Interval.ri','Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1)),'Prelude.apply'('Interval.ri',1)),_6350740,_6350740).
'blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6329038,_6329058,_6319506,_6360498,_6360504,_6360510):-!,hnf('Prelude.otherwise',_6364632,_6360504,_6364592),'blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6364632,_6329038,_6329058,_6319506,_6360498,_6364592,_6360510).

'blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6365560,_6365562,_6365564,_6365566,_6365568,_6365570,_6365572):-freeze(_6365570,freeze(_6365560,'blocked_blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6365560,_6365562,_6365564,_6365566,_6365568,_6365570,_6365572))).
'blocked_blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6329038,_6329058,_6319506,_6365974,_6365980,_6365986):-makeShare(_6310744,_6369798),makeShare(_6329038,_6369818),makeShare(_6310762,_6369838),makeShare(_6329058,_6369858),makeShare(_6310780,_6369878),makeShare(_6310798,_6369898),makeShare(_6310816,_6369918),makeShare(_6310834,_6369938),makeShare(_6310852,_6369958),makeShare(_6310870,_6369978),makeShare(_6310888,_6369998),makeShare(_6310906,_6370018),makeShare(_6310924,_6370038),makeShare(_6310942,_6370058),makeShare(_6310960,_6370078),makeShare(_6310978,_6370098),hnf('Prelude.cond'(letrec4PAKCS(_6369798,'Interval.icosPoint'(_6369818)),'Prelude.cond'(letrec4PAKCS(_6369838,'Interval.icosPoint'(_6369858)),'Prelude.cond'(letrec4PAKCS(_6369878,'Rational.ratMin'('Interval.ivLo'(_6369798),'Interval.ivLo'(_6369838))),'Prelude.cond'(letrec4PAKCS(_6369898,'Rational.ratMax'('Interval.ivHi'(_6369798),'Interval.ivHi'(_6369838))),'Prelude.cond'(letrec4PAKCS(_6369918,'Interval.ivLo'('Interval.piInterval')),'Prelude.cond'(letrec4PAKCS(_6369938,'Interval.ivHi'('Interval.piInterval')),'Prelude.cond'(letrec4PAKCS(_6369958,'Rational.ratCeiling'('Rational.ratDiv'(_6369818,_6369938))),'Prelude.cond'(letrec4PAKCS(_6369978,'Rational.ratFloor'('Rational.ratDiv'(_6369858,_6369918))),'Prelude.cond'(letrec4PAKCS(_6369998,'Prelude.apply'('Prelude.any'(partcall(1,'Prelude.even',[partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[])])),'Prelude._impl\'23enumFromTo\'23Prelude.Enum\'23Prelude.Int\'23'(_6369958,_6369978))),'Prelude.cond'(letrec4PAKCS(_6370018,'Prelude.apply'('Prelude.any'('Prelude.odd'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[]))),'Prelude._impl\'23enumFromTo\'23Prelude.Enum\'23Prelude.Int\'23'(_6369958,_6369978))),'Prelude.cond'(letrec4PAKCS(_6370038,'Interval.icos._\'23caseor0'(_6370018,_6369878)),'Prelude.cond'(letrec4PAKCS(_6370058,'Interval.icos._\'23caseor0._\'23caseor0'(_6369998,_6369898)),'Prelude.cond'(letrec4PAKCS(_6370078,'Rational.ratMax'('Prelude.apply'('Interval.ri','Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1)),_6370038)),'Prelude.cond'(letrec4PAKCS(_6370098,'Rational.ratMin'('Prelude.apply'('Interval.ri',1),_6370058)),'Interval.IV'(_6370078,_6370098))))))))))))))),_6365974,_6365980,_6365986).
'blocked_blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6329038,_6329058,_6319506,_6392142,_6392148,_6392154):-!,hnf(reportFailure4PAKCS('Interval.icos',['Prelude.False']),_6392142,_6392148,_6392154).
'blocked_blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6394006),_6329038,_6329058,_6319506,'FAIL'(_6394006),_6394020,_6394020).
'blocked_blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6394096),_6329038,_6329058,_6319506,'FAIL'(_6394096),_6394110,_6394110).
'blocked_blocked_blocked_Interval.icos_1_Interval.IV_ComplexCase'('FAIL'(_6394186),_6329038,_6329058,_6319506,'FAIL'(_6394186),_6394200,_6394200).
'blocked_blocked_Interval.icos_1'('FAIL'(_6394276),_6319506,'FAIL'(_6394276),_6394290,_6394290):-nonvar(_6394276).

'Interval.ivLo'(_6394976,_6394978,_6394980,_6394982):-freeze(_6394980,'blocked_Interval.ivLo'(_6394976,_6394978,_6394980,_6394982)).
'blocked_Interval.ivLo'(_6395062,_6395386,_6395392,_6395398):-hnf(_6395062,_6396232,_6395392,_6396244),'blocked_Interval.ivLo_1'(_6396232,_6395386,_6396244,_6395398).

'blocked_Interval.ivLo_1'(_6396564,_6396566,_6396568,_6396570):-freeze(_6396568,'blocked_blocked_Interval.ivLo_1'(_6396564,_6396566,_6396568,_6396570)).
'blocked_blocked_Interval.ivLo_1'('Interval.IV'(_6395178,_6395196),_6396948,_6396954,_6396960):-!,hnf(_6395178,_6396948,_6396954,_6396960).
'blocked_blocked_Interval.ivLo_1'('FAIL'(_6397406),'FAIL'(_6397406),_6397420,_6397420):-nonvar(_6397406).

'Interval.ivHi'(_6398098,_6398100,_6398102,_6398104):-freeze(_6398102,'blocked_Interval.ivHi'(_6398098,_6398100,_6398102,_6398104)).
'blocked_Interval.ivHi'(_6398184,_6398508,_6398514,_6398520):-hnf(_6398184,_6399354,_6398514,_6399366),'blocked_Interval.ivHi_1'(_6399354,_6398508,_6399366,_6398520).

'blocked_Interval.ivHi_1'(_6399686,_6399688,_6399690,_6399692):-freeze(_6399690,'blocked_blocked_Interval.ivHi_1'(_6399686,_6399688,_6399690,_6399692)).
'blocked_blocked_Interval.ivHi_1'('Interval.IV'(_6398300,_6398318),_6400070,_6400076,_6400082):-!,hnf(_6398318,_6400070,_6400076,_6400082).
'blocked_blocked_Interval.ivHi_1'('FAIL'(_6400528),'FAIL'(_6400528),_6400542,_6400542):-nonvar(_6400528).

'Interval.isin'(_6401220,_6401222,_6401224,_6401226):-freeze(_6401224,'blocked_Interval.isin'(_6401220,_6401222,_6401224,_6401226)).
'blocked_Interval.isin'(_6401306,_6402892,_6402898,_6402904):-makeShare(_6401334,_6403096),makeShare(_6401352,_6403116),hnf('Prelude.cond'(letrec4PAKCS(_6403096,'Interval.idiv'('Interval.piInterval','Interval.IV'('Prelude.apply'('Interval.ri',2),'Prelude.apply'('Interval.ri',2)))),'Prelude.cond'(letrec4PAKCS(_6403116,'Interval.isub'(_6401306,_6403096)),'Interval.icos'(_6403116))),_6402892,_6402898,_6402904).

'Interval.iatan2'(_6407522,_6407524,_6407526,_6407528,_6407530):-freeze(_6407528,'blocked_Interval.iatan2'(_6407522,_6407524,_6407526,_6407528,_6407530)).
'blocked_Interval.iatan2'(_6407618,_6407636,_6432832,_6432838,_6432844):-makeShare(_6407636,_6427892),hnf('Interval.strictlyPositive'(_6427892),_6433860,_6432838,_6433826),'blocked_Interval.iatan2_ComplexCase'(_6433860,_6407618,_6427892,_6432832,_6433826,_6432844).

'blocked_Interval.iatan2_ComplexCase'(_6434252,_6434254,_6434256,_6434258,_6434260,_6434262):-freeze(_6434260,freeze(_6434252,'blocked_blocked_Interval.iatan2_ComplexCase'(_6434252,_6434254,_6434256,_6434258,_6434260,_6434262))).
'blocked_blocked_Interval.iatan2_ComplexCase'('Prelude.True',_6407618,_6427892,_6434656,_6434662,_6434668):-makeShare(_6407846,_6435014),makeShare(_6407618,_6435034),makeShare(_6427892,_6435054),makeShare(_6407864,_6435074),hnf('Prelude.cond'(letrec4PAKCS(_6435014,'Interval.iatanPoint'('Rational.ratDiv'('Interval.ivLo'(_6435034),'Interval.ivHi'(_6435054)))),'Prelude.cond'(letrec4PAKCS(_6435074,'Interval.iatanPoint'('Rational.ratDiv'('Interval.ivHi'(_6435034),'Interval.ivLo'(_6435054)))),'Interval.IV'('Interval.ivLo'(_6435014),'Interval.ivHi'(_6435074)))),_6434656,_6434662,_6434668).
'blocked_blocked_Interval.iatan2_ComplexCase'('Prelude.False',_6407618,_6427892,_6450100,_6450106,_6450112):-!,makeShare(_6427892,_6444392),makeShare(_6407618,_6444412),hnf('Prelude.&&'('Interval.strictlyNegative'(_6444392),'Rational.ratGe'('Interval.ivLo'(_6444412),'Prelude.apply'('Interval.ri',0))),_6452276,_6450106,_6452242),'blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase'(_6452276,_6444412,_6444392,_6450100,_6452242,_6450112).

'blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase'(_6452902,_6452904,_6452906,_6452908,_6452910,_6452912):-freeze(_6452910,freeze(_6452902,'blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase'(_6452902,_6452904,_6452906,_6452908,_6452910,_6452912))).
'blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6444412,_6444392,_6453306,_6453312,_6453318):-makeShare(_6410482,_6453672),makeShare(_6444412,_6453692),makeShare(_6444392,_6453712),makeShare(_6410500,_6453732),hnf('Prelude.cond'(letrec4PAKCS(_6453672,'Interval.iadd'('Interval.iatanPoint'('Rational.ratDiv'('Interval.ivHi'(_6453692),'Interval.ivLo'(_6453712))),'Interval.piInterval')),'Prelude.cond'(letrec4PAKCS(_6453732,'Interval.iadd'('Interval.iatanPoint'('Rational.ratDiv'('Interval.ivLo'(_6453692),'Interval.ivHi'(_6453712))),'Interval.piInterval')),'Interval.IV'('Interval.ivLo'(_6453672),'Interval.ivHi'(_6453732)))),_6453306,_6453312,_6453318).
'blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6444412,_6444392,_6468246,_6468252,_6468258):-!,makeShare(_6444392,_6463092),makeShare(_6444412,_6463112),hnf('Prelude.&&'('Interval.strictlyNegative'(_6463092),'Rational.ratLe'('Interval.ivHi'(_6463112),'Prelude.apply'('Interval.ri',0))),_6471646,_6468252,_6471612),'blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6471646,_6463112,_6463092,_6468246,_6471612,_6468258).

'blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6472476,_6472478,_6472480,_6472482,_6472484,_6472486):-freeze(_6472484,freeze(_6472476,'blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6472476,_6472478,_6472480,_6472482,_6472484,_6472486))).
'blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6463112,_6463092,_6472880,_6472886,_6472892):-makeShare(_6413454,_6473750),makeShare(_6413472,_6473770),makeShare(_6413490,_6473790),makeShare(_6413508,_6473810),makeShare(_6463112,_6473830),makeShare(_6463092,_6473850),makeShare(_6413526,_6473870),hnf('Prelude.cond'(letrec4PAKCS(_6473750,'Interval.iatan2._\'23selFP19\'23pl'('Interval.piInterval')),'Prelude.cond'(letrec4PAKCS(_6473770,'Interval.iatan2._\'23selFP20\'23ph'('Interval.piInterval')),'Prelude.cond'(letrec4PAKCS(_6473790,'Interval.IV'('Rational.ratNeg'(_6473770),'Rational.ratNeg'(_6473750))),'Prelude.cond'(letrec4PAKCS(_6473810,'Interval.iadd'('Interval.iatanPoint'('Rational.ratDiv'('Interval.ivHi'(_6473830),'Interval.ivLo'(_6473850))),_6473790)),'Prelude.cond'(letrec4PAKCS(_6473870,'Interval.iadd'('Interval.iatanPoint'('Rational.ratDiv'('Interval.ivLo'(_6473830),'Interval.ivHi'(_6473850))),_6473790)),'Interval.IV'('Interval.ivLo'(_6473810),'Interval.ivHi'(_6473870))))))),_6472880,_6472886,_6472892).
'blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6463112,_6463092,_6488210,_6488216,_6488222):-!,makeShare(_6463092,_6485292),hnf('Interval.strictlyNegative'(_6485292),_6492834,_6488216,_6492800),'blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6492834,_6463112,_6485292,_6488210,_6492800,_6488222).

'blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6493856,_6493858,_6493860,_6493862,_6493864,_6493866):-freeze(_6493864,freeze(_6493856,'blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6493856,_6493858,_6493860,_6493862,_6493864,_6493866))).
'blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6463112,_6485292,_6494260,_6494266,_6494272):-makeShare(_6417352,_6495094),makeShare(_6417370,_6495114),makeShare(_6417388,_6495134),makeShare(_6417406,_6495154),makeShare(_6463112,_6495174),makeShare(_6485292,_6495194),makeShare(_6417424,_6495214),hnf('Prelude.cond'(letrec4PAKCS(_6495094,'Interval.iatan2._\'23selFP21\'23pl'('Interval.piInterval')),'Prelude.cond'(letrec4PAKCS(_6495114,'Interval.iatan2._\'23selFP22\'23ph'('Interval.piInterval')),'Prelude.cond'(letrec4PAKCS(_6495134,'Interval.IV'('Rational.ratNeg'(_6495114),'Rational.ratNeg'(_6495094))),'Prelude.cond'(letrec4PAKCS(_6495154,'Interval.iadd'('Interval.iatanPoint'('Rational.ratDiv'('Interval.ivLo'(_6495174),'Interval.ivLo'(_6495194))),_6495134)),'Prelude.cond'(letrec4PAKCS(_6495214,'Interval.iadd'('Interval.iatanPoint'('Rational.ratDiv'('Interval.ivHi'(_6495174),'Interval.ivHi'(_6495194))),'Interval.piInterval')),'Interval.IV'('Interval.ivLo'(_6495154),'Interval.ivHi'(_6495214))))))),_6494260,_6494266,_6494272).
'blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6463112,_6485292,_6507460,_6507466,_6507472):-!,hnf('Prelude.otherwise',_6513308,_6507466,_6513274),'blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6513308,_6463112,_6485292,_6507460,_6513274,_6507472).

'blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6514522,_6514524,_6514526,_6514528,_6514530,_6514532):-freeze(_6514530,freeze(_6514522,'blocked_blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6514522,_6514524,_6514526,_6514528,_6514530,_6514532))).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6463112,_6485292,_6514926,_6514932,_6514938):-makeShare(_6421194,_6515120),makeShare(_6421212,_6515140),hnf('Prelude.cond'(letrec4PAKCS(_6515120,'Interval.piInterval'),'Prelude.cond'(letrec4PAKCS(_6515140,'Interval.iatan2._\'23selFP24\'23ph'(_6515120)),'Interval.IV'('Rational.ratNeg'(_6515140),_6515140))),_6514926,_6514932,_6514938).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6463112,_6485292,_6519246,_6519252,_6519258):-!,hnf(reportFailure4PAKCS('Interval.iatan2',['Prelude.False']),_6519246,_6519252,_6519258).
'blocked_blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6521336),_6463112,_6485292,'FAIL'(_6521336),_6521350,_6521350).
'blocked_blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6521418),_6463112,_6485292,'FAIL'(_6521418),_6521432,_6521432).
'blocked_blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6521500),_6463112,_6463092,'FAIL'(_6521500),_6521514,_6521514).
'blocked_blocked_blocked_Interval.iatan2_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6521582),_6444412,_6444392,'FAIL'(_6521582),_6521596,_6521596).
'blocked_blocked_Interval.iatan2_ComplexCase'('FAIL'(_6521664),_6407618,_6427892,'FAIL'(_6521664),_6521678,_6521678).

'Interval.iatan2._\'23selFP19\'23pl'(_6522994,_6522996,_6522998,_6523000):-freeze(_6522998,'blocked_Interval.iatan2._\'23selFP19\'23pl'(_6522994,_6522996,_6522998,_6523000)).
'blocked_Interval.iatan2._\'23selFP19\'23pl'(_6523080,_6523518,_6523524,_6523530):-hnf(_6523080,_6525048,_6523524,_6525060),'blocked_Interval.iatan2._\'23selFP19\'23pl_1'(_6525048,_6523518,_6525060,_6523530).

'blocked_Interval.iatan2._\'23selFP19\'23pl_1'(_6525494,_6525496,_6525498,_6525500):-freeze(_6525498,'blocked_blocked_Interval.iatan2._\'23selFP19\'23pl_1'(_6525494,_6525496,_6525498,_6525500)).
'blocked_blocked_Interval.iatan2._\'23selFP19\'23pl_1'('Interval.IV'(_6523196,_6523214),_6525878,_6525884,_6525890):-!,hnf(_6523196,_6525878,_6525884,_6525890).
'blocked_blocked_Interval.iatan2._\'23selFP19\'23pl_1'('FAIL'(_6526450),'FAIL'(_6526450),_6526464,_6526464):-nonvar(_6526450).

'Interval.iatan2._\'23selFP20\'23ph'(_6527768,_6527770,_6527772,_6527774):-freeze(_6527772,'blocked_Interval.iatan2._\'23selFP20\'23ph'(_6527768,_6527770,_6527772,_6527774)).
'blocked_Interval.iatan2._\'23selFP20\'23ph'(_6527854,_6528292,_6528298,_6528304):-hnf(_6527854,_6529822,_6528298,_6529834),'blocked_Interval.iatan2._\'23selFP20\'23ph_1'(_6529822,_6528292,_6529834,_6528304).

'blocked_Interval.iatan2._\'23selFP20\'23ph_1'(_6530268,_6530270,_6530272,_6530274):-freeze(_6530272,'blocked_blocked_Interval.iatan2._\'23selFP20\'23ph_1'(_6530268,_6530270,_6530272,_6530274)).
'blocked_blocked_Interval.iatan2._\'23selFP20\'23ph_1'('Interval.IV'(_6527970,_6527988),_6530652,_6530658,_6530664):-!,hnf(_6527988,_6530652,_6530658,_6530664).
'blocked_blocked_Interval.iatan2._\'23selFP20\'23ph_1'('FAIL'(_6531224),'FAIL'(_6531224),_6531238,_6531238):-nonvar(_6531224).

'Interval.iatan2._\'23selFP21\'23pl'(_6532542,_6532544,_6532546,_6532548):-freeze(_6532546,'blocked_Interval.iatan2._\'23selFP21\'23pl'(_6532542,_6532544,_6532546,_6532548)).
'blocked_Interval.iatan2._\'23selFP21\'23pl'(_6532628,_6533066,_6533072,_6533078):-hnf(_6532628,_6534596,_6533072,_6534608),'blocked_Interval.iatan2._\'23selFP21\'23pl_1'(_6534596,_6533066,_6534608,_6533078).

'blocked_Interval.iatan2._\'23selFP21\'23pl_1'(_6535042,_6535044,_6535046,_6535048):-freeze(_6535046,'blocked_blocked_Interval.iatan2._\'23selFP21\'23pl_1'(_6535042,_6535044,_6535046,_6535048)).
'blocked_blocked_Interval.iatan2._\'23selFP21\'23pl_1'('Interval.IV'(_6532744,_6532762),_6535426,_6535432,_6535438):-!,hnf(_6532744,_6535426,_6535432,_6535438).
'blocked_blocked_Interval.iatan2._\'23selFP21\'23pl_1'('FAIL'(_6535998),'FAIL'(_6535998),_6536012,_6536012):-nonvar(_6535998).

'Interval.iatan2._\'23selFP22\'23ph'(_6537316,_6537318,_6537320,_6537322):-freeze(_6537320,'blocked_Interval.iatan2._\'23selFP22\'23ph'(_6537316,_6537318,_6537320,_6537322)).
'blocked_Interval.iatan2._\'23selFP22\'23ph'(_6537402,_6537840,_6537846,_6537852):-hnf(_6537402,_6539370,_6537846,_6539382),'blocked_Interval.iatan2._\'23selFP22\'23ph_1'(_6539370,_6537840,_6539382,_6537852).

'blocked_Interval.iatan2._\'23selFP22\'23ph_1'(_6539816,_6539818,_6539820,_6539822):-freeze(_6539820,'blocked_blocked_Interval.iatan2._\'23selFP22\'23ph_1'(_6539816,_6539818,_6539820,_6539822)).
'blocked_blocked_Interval.iatan2._\'23selFP22\'23ph_1'('Interval.IV'(_6537518,_6537536),_6540200,_6540206,_6540212):-!,hnf(_6537536,_6540200,_6540206,_6540212).
'blocked_blocked_Interval.iatan2._\'23selFP22\'23ph_1'('FAIL'(_6540772),'FAIL'(_6540772),_6540786,_6540786):-nonvar(_6540772).

'Interval.iatan2._\'23selFP24\'23ph'(_6542090,_6542092,_6542094,_6542096):-freeze(_6542094,'blocked_Interval.iatan2._\'23selFP24\'23ph'(_6542090,_6542092,_6542094,_6542096)).
'blocked_Interval.iatan2._\'23selFP24\'23ph'(_6542176,_6542614,_6542620,_6542626):-hnf(_6542176,_6544144,_6542620,_6544156),'blocked_Interval.iatan2._\'23selFP24\'23ph_1'(_6544144,_6542614,_6544156,_6542626).

'blocked_Interval.iatan2._\'23selFP24\'23ph_1'(_6544590,_6544592,_6544594,_6544596):-freeze(_6544594,'blocked_blocked_Interval.iatan2._\'23selFP24\'23ph_1'(_6544590,_6544592,_6544594,_6544596)).
'blocked_blocked_Interval.iatan2._\'23selFP24\'23ph_1'('Interval.IV'(_6542292,_6542310),_6544974,_6544980,_6544986):-!,hnf(_6542310,_6544974,_6544980,_6544986).
'blocked_blocked_Interval.iatan2._\'23selFP24\'23ph_1'('FAIL'(_6545546),'FAIL'(_6545546),_6545560,_6545560):-nonvar(_6545546).

'Interval.cinthroot'(_6546428,_6546430,_6546432,_6546434,_6546436):-freeze(_6546434,'blocked_Interval.cinthroot'(_6546428,_6546430,_6546432,_6546434,_6546436)).
'blocked_Interval.cinthroot'(_6546524,_6546542,_6565400,_6565406,_6565412):-makeShare(_6546524,_6561402),hnf('Prelude._impl\'23\'3C\'3D\'23Prelude.Ord\'23Prelude.Int\'23'(_6561402,0),_6566536,_6565406,_6566502),'blocked_Interval.cinthroot_ComplexCase'(_6566536,_6561402,_6546542,_6565400,_6566502,_6565412).

'blocked_Interval.cinthroot_ComplexCase'(_6566946,_6566948,_6566950,_6566952,_6566954,_6566956):-freeze(_6566954,freeze(_6566946,'blocked_blocked_Interval.cinthroot_ComplexCase'(_6566946,_6566948,_6566950,_6566952,_6566954,_6566956))).
'blocked_blocked_Interval.cinthroot_ComplexCase'('Prelude.True',_6561402,_6546542,_6567350,_6567356,_6567362):-hnf('Prelude.error'(['^c','^i','^n','^t','^h','^r','^o','^o','^t',^:,'^ ','^n','^o','^n',^-,'^p','^o','^s','^i','^t','^i','^v','^e','^ ','^r','^o','^o','^t','^ ','^i','^n','^d','^e','^x']),_6567350,_6567356,_6567362).
'blocked_blocked_Interval.cinthroot_ComplexCase'('Prelude.False',_6561402,_6546542,_6580840,_6580846,_6580852):-!,makeShare(_6561402,_6577756),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6577756,1),_6583124,_6580846,_6583090),'blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase'(_6583124,_6577756,_6546542,_6580840,_6583090,_6580852).

'blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase'(_6583756,_6583758,_6583760,_6583762,_6583764,_6583766):-freeze(_6583764,freeze(_6583756,'blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase'(_6583756,_6583758,_6583760,_6583762,_6583764,_6583766))).
'blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6577756,_6546542,_6584160,_6584166,_6584172):-hnf(_6546542,_6584160,_6584166,_6584172).
'blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6577756,_6546542,_6590154,_6590160,_6590166):-!,hnf('Prelude.otherwise',_6593662,_6590160,_6593628),'blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6593662,_6577756,_6546542,_6590154,_6593628,_6590166).

'blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6594486,_6594488,_6594490,_6594492,_6594494,_6594496):-freeze(_6594494,freeze(_6594486,'blocked_blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6594486,_6594488,_6594490,_6594492,_6594494,_6594496))).
'blocked_blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6577756,_6546542,_6594890,_6594896,_6594902):-makeShare(_6552692,_6597346),makeShare(_6546542,_6597366),makeShare(_6552710,_6597386),makeShare(_6552728,_6597406),makeShare(_6552746,_6597426),makeShare(_6552764,_6597446),makeShare(_6577756,_6597466),makeShare(_6552782,_6597486),makeShare(_6552800,_6597506),makeShare(_6552818,_6597526),makeShare(_6552836,_6597546),makeShare(_6552854,_6597566),hnf('Prelude.cond'(letrec4PAKCS(_6597346,'Interval.ciRealPart'(_6597366)),'Prelude.cond'(letrec4PAKCS(_6597386,'Interval.ciImagPart'(_6597366)),'Prelude.cond'(letrec4PAKCS(_6597406,'Interval.iadd'('Interval.imul'(_6597346,_6597346),'Interval.imul'(_6597386,_6597386))),'Prelude.cond'(letrec4PAKCS(_6597426,'Interval.isqrt'(_6597406)),'Prelude.cond'(letrec4PAKCS(_6597446,'Interval.inth'(_6597466,_6597426)),'Prelude.cond'(letrec4PAKCS(_6597486,'Interval.IV'('Prelude.apply'('Interval.ri',_6597466),'Prelude.apply'('Interval.ri',_6597466))),'Prelude.cond'(letrec4PAKCS(_6597506,'Interval.iatan2'(_6597386,_6597346)),'Prelude.cond'(letrec4PAKCS(_6597526,'Interval.idiv'(_6597506,_6597486)),'Prelude.cond'(letrec4PAKCS(_6597546,'Interval.icos'(_6597526)),'Prelude.cond'(letrec4PAKCS(_6597566,'Interval.isin'(_6597526)),'Interval.CI'('Interval.imul'(_6597446,_6597546),'Interval.imul'(_6597446,_6597566)))))))))))),_6594890,_6594896,_6594902).
'blocked_blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6577756,_6546542,_6611562,_6611568,_6611574):-!,hnf(reportFailure4PAKCS('Interval.cinthroot',['Prelude.False']),_6611562,_6611568,_6611574).
'blocked_blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6613280),_6577756,_6546542,'FAIL'(_6613280),_6613294,_6613294).
'blocked_blocked_blocked_Interval.cinthroot_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6613362),_6577756,_6546542,'FAIL'(_6613362),_6613376,_6613376).
'blocked_blocked_Interval.cinthroot_ComplexCase'('FAIL'(_6613444),_6561402,_6546542,'FAIL'(_6613444),_6613458,_6613458).

'Interval.showInterval'(_6614452,_6614454,_6614456,_6614458):-freeze(_6614456,'blocked_Interval.showInterval'(_6614452,_6614454,_6614456,_6614458)).
'blocked_Interval.showInterval'(_6614538,_6616520,_6616526,_6616532):-hnf(_6614538,_6617654,_6616526,_6617666),'blocked_Interval.showInterval_1'(_6617654,_6616520,_6617666,_6616532).

'blocked_Interval.showInterval_1'(_6618034,_6618036,_6618038,_6618040):-freeze(_6618038,'blocked_blocked_Interval.showInterval_1'(_6618034,_6618036,_6618038,_6618040)).
'blocked_blocked_Interval.showInterval_1'('Interval.IV'(_6614654,_6614672),_6618418,_6618424,_6618430):-!,hnf('Prelude.++'(['^['],'Prelude.++'('Prelude.apply'('Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23',_6614654),'Prelude.++'(['^,','^ '],'Prelude.++'('Prelude.apply'('Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23',_6614672),['^]'])))),_6618418,_6618424,_6618430).
'blocked_blocked_Interval.showInterval_1'('FAIL'(_6622392),'FAIL'(_6622392),_6622406,_6622406):-nonvar(_6622392).

'Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0'(_6625128,_6625130,_6625132,_6625134,_6625136,_6625138,_6625140):-freeze(_6625138,'blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0'(_6625128,_6625130,_6625132,_6625134,_6625136,_6625138,_6625140)).
'blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0'(_6625244,_6625262,_6625280,_6625298,_6626428,_6626434,_6626440):-hnf(_6625244,_6629458,_6626434,_6629488),'blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0_1'(_6629458,_6625262,_6625280,_6625298,_6626428,_6629488,_6626440).

'blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0_1'(_6630198,_6630200,_6630202,_6630204,_6630206,_6630208,_6630210):-freeze(_6630208,freeze(_6630198,'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0_1'(_6630198,_6630200,_6630202,_6630204,_6630206,_6630208,_6630210))).
'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0_1'('Prelude.EQ',_6625262,_6625280,_6625298,_6630588,_6630594,_6630600):-hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23compare\'23Prelude.Ord\'23Rational.Rational\'23',_6625262),_6625280),_6630588,_6630594,_6630600).
'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0_1'('Prelude.LT',_6625262,_6625280,_6625298,_6632710,_6632716,_6632722):-hnf(_6625298,_6632710,_6632716,_6632722).
'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0_1'('Prelude.GT',_6625262,_6625280,_6625298,_6633874,_6633880,_6633886):-!,hnf(_6625298,_6633874,_6633880,_6633886).
'blocked_blocked_Interval._impl\'23compare\'23Prelude.Ord\'23Interval.Interval\'23._\'23caseor0_1'('FAIL'(_6634782),_6625262,_6625280,_6625298,'FAIL'(_6634782),_6634796,_6634796).

'Interval.refine._\'23caseor0'(_6635978,_6635980,_6635982,_6635984,_6635986,_6635988):-freeze(_6635986,'blocked_Interval.refine._\'23caseor0'(_6635978,_6635980,_6635982,_6635984,_6635986,_6635988)).
'blocked_Interval.refine._\'23caseor0'(_6636084,_6636102,_6636120,_6636734,_6636740,_6636746):-hnf(_6636084,_6638100,_6636740,_6638124),'blocked_Interval.refine._\'23caseor0_1'(_6638100,_6636102,_6636120,_6636734,_6638124,_6636746).

'blocked_Interval.refine._\'23caseor0_1'(_6638550,_6638552,_6638554,_6638556,_6638558,_6638560):-freeze(_6638558,freeze(_6638550,'blocked_blocked_Interval.refine._\'23caseor0_1'(_6638550,_6638552,_6638554,_6638556,_6638558,_6638560))).
'blocked_blocked_Interval.refine._\'23caseor0_1'('Prelude.True',_6636102,_6636120,_6638954,_6638960,_6638966):-hnf(_6636102,_6638954,_6638960,_6638966).
'blocked_blocked_Interval.refine._\'23caseor0_1'('Prelude.False',_6636102,_6636120,_6639804,_6639810,_6639816):-!,hnf('Prelude.snd'('Interval.bisect'(_6636120)),_6639804,_6639810,_6639816).
'blocked_blocked_Interval.refine._\'23caseor0_1'('FAIL'(_6640738),_6636102,_6636120,'FAIL'(_6640738),_6640752,_6640752).

'Interval.ipow._\'23caseor0'(_6641850,_6641852,_6641854,_6641856,_6641858):-freeze(_6641856,'blocked_Interval.ipow._\'23caseor0'(_6641850,_6641852,_6641854,_6641856,_6641858)).
'blocked_Interval.ipow._\'23caseor0'(_6641946,_6641964,_6643378,_6643384,_6643390):-hnf(_6641946,_6644664,_6643384,_6644682),'blocked_Interval.ipow._\'23caseor0_1'(_6644664,_6641964,_6643378,_6644682,_6643390).

'blocked_Interval.ipow._\'23caseor0_1'(_6645088,_6645090,_6645092,_6645094,_6645096):-freeze(_6645094,freeze(_6645088,'blocked_blocked_Interval.ipow._\'23caseor0_1'(_6645088,_6645090,_6645092,_6645094,_6645096))).
'blocked_blocked_Interval.ipow._\'23caseor0_1'('Prelude.True',_6641964,'Interval.IV'('Prelude.apply'('Interval.ri',0),'Prelude.foldl1'(partcall(2,'Rational.ratMax',[]),_6641964)),_6645488,_6645488).
'blocked_blocked_Interval.ipow._\'23caseor0_1'('Prelude.False',_6641964,'Interval.IV'('Prelude.foldl1'(partcall(2,'Rational.ratMin',[]),_6647344),'Prelude.foldl1'(partcall(2,'Rational.ratMax',[]),_6647344)),_6647248,_6647254):-!,makeShare(_6641964,_6647344),_6647248=_6647254.
'blocked_blocked_Interval.ipow._\'23caseor0_1'('FAIL'(_6649008),_6641964,'FAIL'(_6649008),_6649022,_6649022).

'Interval.bisectDown._\'23caseor0'(_6650340,_6650342,_6650344,_6650346,_6650348,_6650350,_6650352,_6650354,_6650356,_6650358):-freeze(_6650356,'blocked_Interval.bisectDown._\'23caseor0'(_6650340,_6650342,_6650344,_6650346,_6650348,_6650350,_6650352,_6650354,_6650356,_6650358)).
'blocked_Interval.bisectDown._\'23caseor0'(_6650486,_6650504,_6650522,_6650540,_6650558,_6650576,_6650594,_6652100,_6652106,_6652112):-hnf(_6650486,_6653642,_6652106,_6653690),'blocked_Interval.bisectDown._\'23caseor0_1'(_6653642,_6650504,_6650522,_6650540,_6650558,_6650576,_6650594,_6652100,_6653690,_6652112).

'blocked_Interval.bisectDown._\'23caseor0_1'(_6654172,_6654174,_6654176,_6654178,_6654180,_6654182,_6654184,_6654186,_6654188,_6654190):-freeze(_6654188,freeze(_6654172,'blocked_blocked_Interval.bisectDown._\'23caseor0_1'(_6654172,_6654174,_6654176,_6654178,_6654180,_6654182,_6654184,_6654186,_6654188,_6654190))).
'blocked_blocked_Interval.bisectDown._\'23caseor0_1'('Prelude.True',_6650504,_6650522,_6650540,_6650558,_6650576,_6650594,_6654616,_6654622,_6654628):-hnf('Interval.bisectDown'(_6650522,_6650540,_6650576,_6650504,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_6650594,1)),_6654616,_6654622,_6654628).
'blocked_blocked_Interval.bisectDown._\'23caseor0_1'('Prelude.False',_6650504,_6650522,_6650540,_6650558,_6650576,_6650594,_6656992,_6656998,_6657004):-!,hnf('Interval.bisectDown'(_6650522,_6650540,_6650558,_6650576,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_6650594,1)),_6656992,_6656998,_6657004).
'blocked_blocked_Interval.bisectDown._\'23caseor0_1'('FAIL'(_6659076),_6650504,_6650522,_6650540,_6650558,_6650576,_6650594,'FAIL'(_6659076),_6659090,_6659090).

'Interval.bisectUp._\'23caseor0'(_6660372,_6660374,_6660376,_6660378,_6660380,_6660382,_6660384,_6660386,_6660388,_6660390):-freeze(_6660388,'blocked_Interval.bisectUp._\'23caseor0'(_6660372,_6660374,_6660376,_6660378,_6660380,_6660382,_6660384,_6660386,_6660388,_6660390)).
'blocked_Interval.bisectUp._\'23caseor0'(_6660518,_6660536,_6660554,_6660572,_6660590,_6660608,_6660626,_6662120,_6662126,_6662132):-hnf(_6660518,_6663590,_6662126,_6663638),'blocked_Interval.bisectUp._\'23caseor0_1'(_6663590,_6660536,_6660554,_6660572,_6660590,_6660608,_6660626,_6662120,_6663638,_6662132).

'blocked_Interval.bisectUp._\'23caseor0_1'(_6664108,_6664110,_6664112,_6664114,_6664116,_6664118,_6664120,_6664122,_6664124,_6664126):-freeze(_6664124,freeze(_6664108,'blocked_blocked_Interval.bisectUp._\'23caseor0_1'(_6664108,_6664110,_6664112,_6664114,_6664116,_6664118,_6664120,_6664122,_6664124,_6664126))).
'blocked_blocked_Interval.bisectUp._\'23caseor0_1'('Prelude.True',_6660536,_6660554,_6660572,_6660590,_6660608,_6660626,_6664552,_6664558,_6664564):-hnf('Interval.bisectUp'(_6660554,_6660572,_6660536,_6660590,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_6660626,1)),_6664552,_6664558,_6664564).
'blocked_blocked_Interval.bisectUp._\'23caseor0_1'('Prelude.False',_6660536,_6660554,_6660572,_6660590,_6660608,_6660626,_6666904,_6666910,_6666916):-!,hnf('Interval.bisectUp'(_6660554,_6660572,_6660590,_6660608,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_6660626,1)),_6666904,_6666910,_6666916).
'blocked_blocked_Interval.bisectUp._\'23caseor0_1'('FAIL'(_6668964),_6660536,_6660554,_6660572,_6660590,_6660608,_6660626,'FAIL'(_6668964),_6668978,_6668978).

'Interval.iatanSmall._\'23lambda2._\'23caseor0'(_6670744,_6670746,_6670748,_6670750):-freeze(_6670748,'blocked_Interval.iatanSmall._\'23lambda2._\'23caseor0'(_6670744,_6670746,_6670748,_6670750)).
'blocked_Interval.iatanSmall._\'23lambda2._\'23caseor0'(_6670830,_6671792,_6671798,_6671804):-hnf(_6670830,_6673718,_6671798,_6673730),'blocked_Interval.iatanSmall._\'23lambda2._\'23caseor0_1'(_6673718,_6671792,_6673730,_6671804).

'blocked_Interval.iatanSmall._\'23lambda2._\'23caseor0_1'(_6674236,_6674238,_6674240,_6674242):-freeze(_6674240,freeze(_6674236,'blocked_blocked_Interval.iatanSmall._\'23lambda2._\'23caseor0_1'(_6674236,_6674238,_6674240,_6674242))).
'blocked_blocked_Interval.iatanSmall._\'23lambda2._\'23caseor0_1'('Prelude.True',_6674620,_6674626,_6674632):-hnf('Prelude.apply'('Interval.ri',1),_6674620,_6674626,_6674632).
'blocked_blocked_Interval.iatanSmall._\'23lambda2._\'23caseor0_1'('Prelude.False',_6675768,_6675774,_6675780):-!,hnf('Prelude.apply'('Interval.ri','Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1)),_6675768,_6675774,_6675780).
'blocked_blocked_Interval.iatanSmall._\'23lambda2._\'23caseor0_1'('FAIL'(_6677056),'FAIL'(_6677056),_6677070,_6677070).

'Interval.icosPoint._\'23lambda3._\'23caseor0'(_6678750,_6678752,_6678754,_6678756):-freeze(_6678754,'blocked_Interval.icosPoint._\'23lambda3._\'23caseor0'(_6678750,_6678752,_6678754,_6678756)).
'blocked_Interval.icosPoint._\'23lambda3._\'23caseor0'(_6678836,_6679792,_6679798,_6679804):-hnf(_6678836,_6681682,_6679798,_6681694),'blocked_Interval.icosPoint._\'23lambda3._\'23caseor0_1'(_6681682,_6679792,_6681694,_6679804).

'blocked_Interval.icosPoint._\'23lambda3._\'23caseor0_1'(_6682194,_6682196,_6682198,_6682200):-freeze(_6682198,freeze(_6682194,'blocked_blocked_Interval.icosPoint._\'23lambda3._\'23caseor0_1'(_6682194,_6682196,_6682198,_6682200))).
'blocked_blocked_Interval.icosPoint._\'23lambda3._\'23caseor0_1'('Prelude.True',_6682578,_6682584,_6682590):-hnf('Prelude.apply'('Interval.ri',1),_6682578,_6682584,_6682590).
'blocked_blocked_Interval.icosPoint._\'23lambda3._\'23caseor0_1'('Prelude.False',_6683720,_6683726,_6683732):-!,hnf('Prelude.apply'('Interval.ri','Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1)),_6683720,_6683726,_6683732).
'blocked_blocked_Interval.icosPoint._\'23lambda3._\'23caseor0_1'('FAIL'(_6685002),'FAIL'(_6685002),_6685016,_6685016).

'Interval.icos._\'23caseor0'(_6686098,_6686100,_6686102,_6686104,_6686106):-freeze(_6686104,'blocked_Interval.icos._\'23caseor0'(_6686098,_6686100,_6686102,_6686104,_6686106)).
'blocked_Interval.icos._\'23caseor0'(_6686194,_6686212,_6687052,_6687058,_6687064):-hnf(_6686194,_6688338,_6687058,_6688356),'blocked_Interval.icos._\'23caseor0_1'(_6688338,_6686212,_6687052,_6688356,_6687064).

'blocked_Interval.icos._\'23caseor0_1'(_6688762,_6688764,_6688766,_6688768,_6688770):-freeze(_6688768,freeze(_6688762,'blocked_blocked_Interval.icos._\'23caseor0_1'(_6688762,_6688764,_6688766,_6688768,_6688770))).
'blocked_blocked_Interval.icos._\'23caseor0_1'('Prelude.True',_6686212,_6689156,_6689162,_6689168):-hnf('Rational.ratMin'(_6686212,'Prelude.apply'('Interval.ri','Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1))),_6689156,_6689162,_6689168).
'blocked_blocked_Interval.icos._\'23caseor0_1'('Prelude.False',_6686212,_6690994,_6691000,_6691006):-!,hnf(_6686212,_6690994,_6691000,_6691006).
'blocked_blocked_Interval.icos._\'23caseor0_1'('FAIL'(_6691466),_6686212,'FAIL'(_6691466),_6691480,_6691480).

'Interval.icos._\'23caseor0._\'23caseor0'(_6692978,_6692980,_6692982,_6692984,_6692986):-freeze(_6692984,'blocked_Interval.icos._\'23caseor0._\'23caseor0'(_6692978,_6692980,_6692982,_6692984,_6692986)).
'blocked_Interval.icos._\'23caseor0._\'23caseor0'(_6693074,_6693092,_6693920,_6693926,_6693932):-hnf(_6693074,_6695638,_6693926,_6695656),'blocked_Interval.icos._\'23caseor0._\'23caseor0_1'(_6695638,_6693092,_6693920,_6695656,_6693932).

'blocked_Interval.icos._\'23caseor0._\'23caseor0_1'(_6696134,_6696136,_6696138,_6696140,_6696142):-freeze(_6696140,freeze(_6696134,'blocked_blocked_Interval.icos._\'23caseor0._\'23caseor0_1'(_6696134,_6696136,_6696138,_6696140,_6696142))).
'blocked_blocked_Interval.icos._\'23caseor0._\'23caseor0_1'('Prelude.True',_6693092,_6696528,_6696534,_6696540):-hnf('Rational.ratMax'(_6693092,'Prelude.apply'('Interval.ri',1)),_6696528,_6696534,_6696540).
'blocked_blocked_Interval.icos._\'23caseor0._\'23caseor0_1'('Prelude.False',_6693092,_6698006,_6698012,_6698018):-!,hnf(_6693092,_6698006,_6698012,_6698018).
'blocked_blocked_Interval.icos._\'23caseor0._\'23caseor0_1'('FAIL'(_6698550),_6693092,'FAIL'(_6698550),_6698564,_6698564).

:-costCenters(['']).




%%%%% Number of shared variables: 176

