%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Data.List').
:-importModule('Positive').
:-importModule('Prelude').
:-importModule('PrimeFactors').
:-importModule('RadExpr').
:-importModule('Rational').

:-curryModule('Normalize').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Normalize.rZero','Normalize.rZero',0,'Normalize.rZero',nofix,notype).
functiontype('Normalize.rOne','Normalize.rOne',0,'Normalize.rOne',nofix,notype).
functiontype('Normalize.rNegOne','Normalize.rNegOne',0,'Normalize.rNegOne',nofix,notype).
functiontype('Normalize.normalize',normalize,0,'Normalize.normalize',nofix,notype).
functiontype('Normalize.normalizeOnce',normalizeOnce,1,'Normalize.normalizeOnce',nofix,notype).
functiontype('Normalize.fixN','Normalize.fixN',3,'Normalize.fixN',nofix,notype).
functiontype('Normalize.flattenArith',flattenArith,1,'Normalize.flattenArith',nofix,notype).
functiontype('Normalize.foldConstants',foldConstants,1,'Normalize.foldConstants',nofix,notype).
functiontype('Normalize.foldConstantsAdd','Normalize.foldConstantsAdd',2,'Normalize.foldConstantsAdd',nofix,notype).
functiontype('Normalize.foldConstantsMul','Normalize.foldConstantsMul',2,'Normalize.foldConstantsMul',nofix,notype).
functiontype('Normalize.simplifyPowers',simplifyPowers,1,'Normalize.simplifyPowers',nofix,notype).
functiontype('Normalize.extractPerfectPowers',extractPerfectPowers,1,'Normalize.extractPerfectPowers',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit','Normalize.extractPerfectPowersLit',2,'Normalize.extractPerfectPowersLit',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit.buildExtracted.145','Normalize.extractPerfectPowersLit.buildExtracted.145',3,'Normalize.extractPerfectPowersLit.buildExtracted.145',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut','Normalize.extractPerfectPowersLit._#selFP8#numOut',1,'Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn','Normalize.extractPerfectPowersLit._#selFP9#numIn',1,'Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut','Normalize.extractPerfectPowersLit._#selFP6#denOut',1,'Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn','Normalize.extractPerfectPowersLit._#selFP7#denIn',1,'Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2','Normalize.extractPerfectPowersLit._#selFP4#numOut2',1,'Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2','Normalize.extractPerfectPowersLit._#selFP5#numIn2',1,'Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2',nofix,notype).
functiontype('Normalize.intPow','Normalize.intPow',2,'Normalize.intPow',nofix,notype).
functiontype('Normalize.absInt','Normalize.absInt',1,'Normalize.absInt',nofix,notype).
functiontype('Normalize.extractNthPower','Normalize.extractNthPower',2,'Normalize.extractNthPower',nofix,notype).
functiontype('Normalize.extractNthPower._\'23lambda11','Normalize.extractNthPower._#lambda11',3,'Normalize.extractNthPower._\'23lambda11',nofix,notype).
functiontype('Normalize.extractNthPower._\'23lambda12','Normalize.extractNthPower._#lambda12',3,'Normalize.extractNthPower._\'23lambda12',nofix,notype).
functiontype('Normalize.collectCoefficients',collectCoefficients,1,'Normalize.collectCoefficients',nofix,notype).
functiontype('Normalize.collectCoefficients._\'23selFP11\'23lits','Normalize.collectCoefficients._#selFP11#lits',1,'Normalize.collectCoefficients._\'23selFP11\'23lits',nofix,notype).
functiontype('Normalize.collectCoefficients._\'23selFP12\'23rest','Normalize.collectCoefficients._#selFP12#rest',1,'Normalize.collectCoefficients._\'23selFP12\'23rest',nofix,notype).
functiontype('Normalize.flattenMulN','Normalize.flattenMulN',1,'Normalize.flattenMulN',nofix,notype).
functiontype('Normalize.partitionLitsN','Normalize.partitionLitsN',1,'Normalize.partitionLitsN',nofix,notype).
functiontype('Normalize.partitionLitsN._\'23selFP14\'23ls','Normalize.partitionLitsN._#selFP14#ls',1,'Normalize.partitionLitsN._\'23selFP14\'23ls',nofix,notype).
functiontype('Normalize.partitionLitsN._\'23selFP15\'23rs','Normalize.partitionLitsN._#selFP15#rs',1,'Normalize.partitionLitsN._\'23selFP15\'23rs',nofix,notype).
functiontype('Normalize.buildMulN','Normalize.buildMulN',1,'Normalize.buildMulN',nofix,notype).
functiontype('Normalize.applyCoeffMul','Normalize.applyCoeffMul',2,'Normalize.applyCoeffMul',nofix,notype).
functiontype('Normalize.collectTerms',collectTerms,1,'Normalize.collectTerms',nofix,notype).
functiontype('Normalize.collectTerms._\'23lambda20','Normalize.collectTerms._#lambda20',2,'Normalize.collectTerms._\'23lambda20',nofix,notype).
functiontype('Normalize.collectTerms._\'23lambda21','Normalize.collectTerms._#lambda21',1,'Normalize.collectTerms._\'23lambda21',nofix,notype).
functiontype('Normalize.flattenAddN','Normalize.flattenAddN',1,'Normalize.flattenAddN',nofix,notype).
functiontype('Normalize.splitCoeff','Normalize.splitCoeff',1,'Normalize.splitCoeff',nofix,notype).
functiontype('Normalize.splitCoeff._\'23selFP17\'23c','Normalize.splitCoeff._#selFP17#c',1,'Normalize.splitCoeff._\'23selFP17\'23c',nofix,notype).
functiontype('Normalize.splitCoeff._\'23selFP18\'23b','Normalize.splitCoeff._#selFP18#b',1,'Normalize.splitCoeff._\'23selFP18\'23b',nofix,notype).
functiontype('Normalize.groupTermsAL','Normalize.groupTermsAL',0,'Normalize.groupTermsAL',nofix,notype).
functiontype('Normalize.insertTerm','Normalize.insertTerm',2,'Normalize.insertTerm',nofix,notype).
functiontype('Normalize.insertTerm._\'23selFP20\'23c','Normalize.insertTerm._#selFP20#c',1,'Normalize.insertTerm._\'23selFP20\'23c',nofix,notype).
functiontype('Normalize.insertTerm._\'23selFP21\'23base','Normalize.insertTerm._#selFP21#base',1,'Normalize.insertTerm._\'23selFP21\'23base',nofix,notype).
functiontype('Normalize.insertWithAdd','Normalize.insertWithAdd',3,'Normalize.insertWithAdd',nofix,notype).
functiontype('Normalize.applyCoeffAdd','Normalize.applyCoeffAdd',2,'Normalize.applyCoeffAdd',nofix,notype).
functiontype('Normalize.isLitOne','Normalize.isLitOne',1,'Normalize.isLitOne',nofix,notype).
functiontype('Normalize.buildAddN','Normalize.buildAddN',1,'Normalize.buildAddN',nofix,notype).
functiontype('Normalize.sortCommutative',sortCommutative,1,'Normalize.sortCommutative',nofix,notype).
functiontype('Normalize.flattenAddS','Normalize.flattenAddS',1,'Normalize.flattenAddS',nofix,notype).
functiontype('Normalize.flattenMulS','Normalize.flattenMulS',1,'Normalize.flattenMulS',nofix,notype).
functiontype('Normalize.buildMulS','Normalize.buildMulS',1,'Normalize.buildMulS',nofix,notype).
functiontype('Normalize.distribute',distribute,1,'Normalize.distribute',nofix,notype).
functiontype('Normalize.distribute._\'23lambda31','Normalize.distribute._#lambda31',2,'Normalize.distribute._\'23lambda31',nofix,notype).
functiontype('Normalize.distribute._\'23lambda33','Normalize.distribute._#lambda33',2,'Normalize.distribute._\'23lambda33',nofix,notype).
functiontype('Normalize.distribute._\'23lambda35','Normalize.distribute._#lambda35',2,'Normalize.distribute._\'23lambda35',nofix,notype).
functiontype('Normalize.distribute._\'23lambda36','Normalize.distribute._#lambda36',2,'Normalize.distribute._\'23lambda36',nofix,notype).
functiontype('Normalize.isSum','Normalize.isSum',1,'Normalize.isSum',nofix,notype).
functiontype('Normalize.isTinySum','Normalize.isTinySum',1,'Normalize.isTinySum',nofix,notype).
functiontype('Normalize.flattenS','Normalize.flattenS',1,'Normalize.flattenS',nofix,notype).
functiontype('Normalize.rebuildAddD','Normalize.rebuildAddD',1,'Normalize.rebuildAddD',nofix,notype).
functiontype('Normalize.fixN._\'23caseor0','Normalize.fixN._#caseor0',5,'Normalize.fixN._\'23caseor0',nofix,notype).
functiontype('Normalize.flattenArith._\'23caseor0','Normalize.flattenArith._#caseor0',2,'Normalize.flattenArith._\'23caseor0',nofix,notype).
functiontype('Normalize.flattenArith._\'23caseor0._\'23caseor0','Normalize.flattenArith._#caseor0._#caseor0',2,'Normalize.flattenArith._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstants._\'23caseor0','Normalize.foldConstants._#caseor0',2,'Normalize.foldConstants._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstants._\'23caseor0._\'23caseor0','Normalize.foldConstants._#caseor0._#caseor0',2,'Normalize.foldConstants._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.foldConstants._#caseor0._#caseor0._#caseor0',3,'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.foldConstants._#caseor0._#caseor0._#caseor0._#caseor0',3,'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.foldConstants._#caseor0._#caseor0._#caseor0._#caseor0._#caseor0',3,'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstantsAdd._\'23caseor0','Normalize.foldConstantsAdd._#caseor0',4,'Normalize.foldConstantsAdd._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.foldConstantsAdd._#caseor0._#caseor0._#caseor0',3,'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.foldConstantsAdd._#caseor0._#caseor0._#caseor0._#caseor0',5,'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0','Normalize.foldConstantsAdd._#caseor0._#caseor0',5,'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstantsMul._\'23caseor0','Normalize.foldConstantsMul._#caseor0',4,'Normalize.foldConstantsMul._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.foldConstantsMul._#caseor0._#caseor0._#caseor0',4,'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.foldConstantsMul._#caseor0._#caseor0._#caseor0._#caseor0',3,'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.foldConstantsMul._\'23caseor0._\'23caseor0','Normalize.foldConstantsMul._#caseor0._#caseor0',5,'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.simplifyPowers._\'23caseor0._\'23caseor0','Normalize.simplifyPowers._#caseor0._#caseor0',5,'Normalize.simplifyPowers._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.simplifyPowers._\'23caseor0','Normalize.simplifyPowers._#caseor0',4,'Normalize.simplifyPowers._\'23caseor0',nofix,notype).
functiontype('Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.simplifyPowers._#caseor0._#caseor0._#caseor0',3,'Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.simplifyPowers._#caseor0._#caseor0._#caseor0._#caseor0',3,'Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.extractPerfectPowers._\'23caseor0','Normalize.extractPerfectPowers._#caseor0',3,'Normalize.extractPerfectPowers._\'23caseor0',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit._\'23caseor0','Normalize.extractPerfectPowersLit._#caseor0',6,'Normalize.extractPerfectPowersLit._\'23caseor0',nofix,notype).
functiontype('Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0','Normalize.extractPerfectPowersLit.buildExtracted.145._#caseor0',6,'Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0',nofix,notype).
functiontype('Normalize.collectCoefficients._\'23caseor0','Normalize.collectCoefficients._#caseor0',2,'Normalize.collectCoefficients._\'23caseor0',nofix,notype).
functiontype('Normalize.flattenMulN._\'23caseor0','Normalize.flattenMulN._#caseor0',2,'Normalize.flattenMulN._\'23caseor0',nofix,notype).
functiontype('Normalize.partitionLitsN._\'23caseor0._\'23caseor0','Normalize.partitionLitsN._#caseor0._#caseor0',5,'Normalize.partitionLitsN._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.partitionLitsN._\'23caseor0','Normalize.partitionLitsN._#caseor0',5,'Normalize.partitionLitsN._\'23caseor0',nofix,notype).
functiontype('Normalize.applyCoeffMul._\'23caseor0','Normalize.applyCoeffMul._#caseor0',3,'Normalize.applyCoeffMul._\'23caseor0',nofix,notype).
functiontype('Normalize.flattenAddN._\'23caseor0','Normalize.flattenAddN._#caseor0',2,'Normalize.flattenAddN._\'23caseor0',nofix,notype).
functiontype('Normalize.splitCoeff._\'23caseor0._\'23caseor0','Normalize.splitCoeff._#caseor0._#caseor0',3,'Normalize.splitCoeff._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.splitCoeff._\'23caseor0','Normalize.splitCoeff._#caseor0',3,'Normalize.splitCoeff._\'23caseor0',nofix,notype).
functiontype('Normalize.isLitOne._\'23caseor0','Normalize.isLitOne._#caseor0',2,'Normalize.isLitOne._\'23caseor0',nofix,notype).
functiontype('Normalize.flattenAddS._\'23caseor0','Normalize.flattenAddS._#caseor0',2,'Normalize.flattenAddS._\'23caseor0',nofix,notype).
functiontype('Normalize.flattenMulS._\'23caseor0','Normalize.flattenMulS._#caseor0',2,'Normalize.flattenMulS._\'23caseor0',nofix,notype).
functiontype('Normalize.distribute._\'23caseor0._\'23caseor0','Normalize.distribute._#caseor0._#caseor0',3,'Normalize.distribute._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.distribute._#caseor0._#caseor0._#caseor0',3,'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.distribute._\'23caseor0','Normalize.distribute._#caseor0',3,'Normalize.distribute._\'23caseor0',nofix,notype).
functiontype('Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.distribute._#caseor0._#caseor0._#caseor0._#caseor0._#caseor0',3,'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.distribute._#caseor0._#caseor0._#caseor0._#caseor0',3,'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.distribute._#caseor0._#caseor0._#caseor0._#caseor0._#caseor0._#caseor0._#caseor0',3,'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Normalize.distribute._#caseor0._#caseor0._#caseor0._#caseor0._#caseor0._#caseor0',3,'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Normalize.isSum._\'23caseor0','Normalize.isSum._#caseor0',2,'Normalize.isSum._\'23caseor0',nofix,notype).
functiontype('Normalize.flattenS._\'23caseor0','Normalize.flattenS._#caseor0',2,'Normalize.flattenS._\'23caseor0',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Normalize.rZero'(_4954030,_4954032,_4954034):-freeze(_4954032,'blocked_Normalize.rZero'(_4954030,_4954032,_4954034)).
'blocked_Normalize.rZero'(_4954188,_4954194,_4954200):-hnf('Rational.fromInt'(0),_4954188,_4954194,_4954200).

'Normalize.rOne'(_4955294,_4955296,_4955298):-freeze(_4955296,'blocked_Normalize.rOne'(_4955294,_4955296,_4955298)).
'blocked_Normalize.rOne'(_4955452,_4955458,_4955464):-hnf('Rational.fromInt'(1),_4955452,_4955458,_4955464).

'Normalize.rNegOne'(_4956666,_4956668,_4956670):-freeze(_4956668,'blocked_Normalize.rNegOne'(_4956666,_4956668,_4956670)).
'blocked_Normalize.rNegOne'(_4956908,_4956914,_4956920):-hnf('Rational.fromInt'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1)),_4956908,_4956914,_4956920).

'Normalize.normalize'(_4958648,_4958650,_4958652):-freeze(_4958650,'blocked_Normalize.normalize'(_4958648,_4958650,_4958652)).
'blocked_Normalize.normalize'(_4958890,_4958896,_4958902):-hnf(partcall(1,'Normalize.fixN',[partcall(1,'Normalize.normalizeOnce',[]),10]),_4958890,_4958896,_4958902).

'Normalize.normalizeOnce'(_4960610,_4960612,_4960614,_4960616):-freeze(_4960614,'blocked_Normalize.normalizeOnce'(_4960610,_4960612,_4960614,_4960616)).
'blocked_Normalize.normalizeOnce'(_4960696,_4961384,_4961390,_4961396):-hnf('Normalize.collectTerms'('Normalize.collectCoefficients'('Normalize.distribute'('Normalize.sortCommutative'('Normalize.extractPerfectPowers'('Normalize.simplifyPowers'('Normalize.foldConstants'('Normalize.flattenArith'(_4960696)))))))),_4961384,_4961390,_4961396).

'Normalize.fixN'(_4964468,_4964470,_4964472,_4964474,_4964476,_4964478):-freeze(_4964476,'blocked_Normalize.fixN'(_4964468,_4964470,_4964472,_4964474,_4964476,_4964478)).
'blocked_Normalize.fixN'(_4964574,_4964592,_4964610,_4967960,_4967966,_4967972):-makeShare(_4964574,_4966458),hnf('Prelude._impl\'23\'3C\'3D\'23Prelude.Ord\'23Prelude.Int\'23'(_4966458,0),_4968966,_4967966,_4968926),'blocked_Normalize.fixN_ComplexCase'(_4968966,_4966458,_4964592,_4964610,_4967960,_4968926,_4967972).

'blocked_Normalize.fixN_ComplexCase'(_4969354,_4969356,_4969358,_4969360,_4969362,_4969364,_4969366):-freeze(_4969364,freeze(_4969354,'blocked_blocked_Normalize.fixN_ComplexCase'(_4969354,_4969356,_4969358,_4969360,_4969362,_4969364,_4969366))).
'blocked_blocked_Normalize.fixN_ComplexCase'('Prelude.True',_4966458,_4964592,_4964610,_4969768,_4969774,_4969780):-hnf(_4964610,_4969768,_4969774,_4969780).
'blocked_blocked_Normalize.fixN_ComplexCase'('Prelude.False',_4966458,_4964592,_4964610,_4970674,_4970680,_4970686):-!,makeShare(_4964976,_4971024),makeShare(_4964592,_4971044),makeShare(_4964610,_4971064),hnf('Prelude.cond'(letrec4PAKCS(_4971024,'Prelude.apply'(_4971044,_4971064)),'Normalize.fixN._\'23caseor0'('RadExpr._impl\'23\'3D\'3D\'23Prelude.Eq\'23RadExpr.RadExpr\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23',[]),_4971024,_4971064),_4971064,_4966458,_4971044,_4971024)),_4970674,_4970680,_4970686).
'blocked_blocked_Normalize.fixN_ComplexCase'('FAIL'(_4974696),_4966458,_4964592,_4964610,'FAIL'(_4974696),_4974710,_4974710).

'Normalize.flattenArith'(_4975750,_4975752,_4975754,_4975756):-freeze(_4975754,'blocked_Normalize.flattenArith'(_4975750,_4975752,_4975754,_4975756)).
'blocked_Normalize.flattenArith'(_4975836,_4980780,_4980786,_4980792):-makeShare(_4975836,_4979186),hnf(_4979186,_4981950,_4980786,_4981968),'blocked_Normalize.flattenArith_1'(_4981950,_4981950,_4980780,_4981968,_4980792).

'blocked_Normalize.flattenArith_1'(_4982368,_4982370,_4982372,_4982374,_4982376):-freeze(_4982374,freeze(_4982368,'blocked_blocked_Normalize.flattenArith_1'(_4982368,_4982370,_4982372,_4982374,_4982376))).
'blocked_blocked_Normalize.flattenArith_1'('RadExpr.Neg'(_4975952),_4979186,_4982760,_4982766,_4982772):-makeShare(_4975986,_4982914),makeShare(_4975952,_4982934),hnf('Prelude.cond'(letrec4PAKCS(_4982914,'RadExpr.Neg'('Normalize.flattenArith'(_4982934))),'Normalize.flattenArith._\'23caseor0'(_4982934,_4982914)),_4982760,_4982766,_4982772).
'blocked_blocked_Normalize.flattenArith_1'('RadExpr.Add'(_4976726,_4976744),_4979186,'RadExpr.Add'('Normalize.flattenArith'(_4976726),'Normalize.flattenArith'(_4976744)),_4985538,_4985538).
'blocked_blocked_Normalize.flattenArith_1'('RadExpr.Mul'(_4977182,_4977200),_4979186,'RadExpr.Mul'('Normalize.flattenArith'(_4977182),'Normalize.flattenArith'(_4977200)),_4987156,_4987156).
'blocked_blocked_Normalize.flattenArith_1'('RadExpr.Inv'(_4977638),_4979186,_4988760,_4988766,_4988772):-makeShare(_4977672,_4988914),makeShare(_4977638,_4988934),hnf('Prelude.cond'(letrec4PAKCS(_4988914,'RadExpr.Inv'('Normalize.flattenArith'(_4988934))),'Normalize.flattenArith._\'23caseor0._\'23caseor0'(_4988934,_4988914)),_4988760,_4988766,_4988772).
'blocked_blocked_Normalize.flattenArith_1'('RadExpr.Root'(_4978412,_4978430),_4979186,'RadExpr.Root'(_4978412,'Normalize.flattenArith'(_4978430)),_4991622,_4991622).
'blocked_blocked_Normalize.flattenArith_1'('RadExpr.Pow'(_4978784,_4978802),_4979186,'RadExpr.Pow'('Normalize.flattenArith'(_4978784),_4978802),_4993004,_4993004).
'blocked_blocked_Normalize.flattenArith_1'('RadExpr.Lit'(_4979156),_4979186,_4994366,_4994372,_4994378):-!,hnf(_4979186,_4994366,_4994372,_4994378).
'blocked_blocked_Normalize.flattenArith_1'('FAIL'(_4994886),_4979186,'FAIL'(_4994886),_4994900,_4994900).

'Normalize.foldConstants'(_4995962,_4995964,_4995966,_4995968):-freeze(_4995966,'blocked_Normalize.foldConstants'(_4995962,_4995964,_4995966,_4995968)).
'blocked_Normalize.foldConstants'(_4996048,_5002380,_5002386,_5002392):-hnf(_4996048,_5003586,_5002386,_5003598),'blocked_Normalize.foldConstants_1'(_5003586,_5002380,_5003598,_5002392).

'blocked_Normalize.foldConstants_1'(_5003984,_5003986,_5003988,_5003990):-freeze(_5003988,freeze(_5003984,'blocked_blocked_Normalize.foldConstants_1'(_5003984,_5003986,_5003988,_5003990))).
'blocked_blocked_Normalize.foldConstants_1'('RadExpr.Lit'(_4996164),'RadExpr.Lit'(_4996164),_5004372,_5004372).
'blocked_blocked_Normalize.foldConstants_1'('RadExpr.Neg'(_4996358),_5005254,_5005260,_5005266):-makeShare(_4996392,_5005468),makeShare(_4996410,_5005488),hnf('Prelude.cond'(letrec4PAKCS(_5005468,'Normalize.foldConstants'(_4996358)),'Prelude.cond'(letrec4PAKCS(_5005488,'RadExpr.Neg'(_5005468)),'Normalize.foldConstants._\'23caseor0'(_5005468,_5005488))),_5005254,_5005260,_5005266).
'blocked_blocked_Normalize.foldConstants_1'('RadExpr.Add'(_4997464,_4997482),_5008612,_5008618,_5008624):-hnf('Normalize.foldConstantsAdd'('Normalize.foldConstants'(_4997464),'Normalize.foldConstants'(_4997482)),_5008612,_5008618,_5008624).
'blocked_blocked_Normalize.foldConstants_1'('RadExpr.Mul'(_4997920,_4997938),_5010282,_5010288,_5010294):-hnf('Normalize.foldConstantsMul'('Normalize.foldConstants'(_4997920),'Normalize.foldConstants'(_4997938)),_5010282,_5010288,_5010294).
'blocked_blocked_Normalize.foldConstants_1'('RadExpr.Inv'(_4998376),_5011944,_5011950,_5011956):-makeShare(_4998410,_5012158),makeShare(_4998428,_5012178),hnf('Prelude.cond'(letrec4PAKCS(_5012158,'Normalize.foldConstants'(_4998376)),'Prelude.cond'(letrec4PAKCS(_5012178,'RadExpr.Inv'(_5012158)),'Normalize.foldConstants._\'23caseor0._\'23caseor0'(_5012158,_5012178))),_5011944,_5011950,_5011956).
'blocked_blocked_Normalize.foldConstants_1'('RadExpr.Root'(_4999482,_4999500),_5015386,_5015392,_5015398):-makeShare(_4999540,_5015700),makeShare(_4999558,_5015720),makeShare(_4999482,_5015740),hnf('Prelude.cond'(letrec4PAKCS(_5015700,'Normalize.foldConstants'(_4999500)),'Prelude.cond'(letrec4PAKCS(_5015720,'RadExpr.Root'(_5015740,_5015700)),'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0'(_5015700,_5015740,_5015720))),_5015386,_5015392,_5015398).
'blocked_blocked_Normalize.foldConstants_1'('RadExpr.Pow'(_5000752,_5000770),_5019460,_5019466,_5019472):-!,makeShare(_5000810,_5019828),makeShare(_5000828,_5019848),makeShare(_5000770,_5019868),hnf('Prelude.cond'(letrec4PAKCS(_5019828,'Normalize.foldConstants'(_5000752)),'Prelude.cond'(letrec4PAKCS(_5019848,'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5019868,0),_5019828,_5019868)),'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5019828,_5019868,_5019848))),_5019460,_5019466,_5019472).
'blocked_blocked_Normalize.foldConstants_1'('FAIL'(_5024380),'FAIL'(_5024380),_5024394,_5024394).

'Normalize.foldConstantsAdd'(_5025562,_5025564,_5025566,_5025568,_5025570):-freeze(_5025568,'blocked_Normalize.foldConstantsAdd'(_5025562,_5025564,_5025566,_5025568,_5025570)).
'blocked_Normalize.foldConstantsAdd'(_5025658,_5025676,_5029602,_5029608,_5029614):-makeShare(_5025658,_5028078),makeShare(_5025676,_5028098),hnf('Prelude.(,)'(_5028078,_5028098),_5031026,_5029608,_5030992),'blocked_Normalize.foldConstantsAdd_ComplexCase'(_5031026,_5028078,_5028098,_5029602,_5030992,_5029614).

'blocked_Normalize.foldConstantsAdd_ComplexCase'(_5031496,_5031498,_5031500,_5031502,_5031504,_5031506):-freeze(_5031504,freeze(_5031496,'blocked_blocked_Normalize.foldConstantsAdd_ComplexCase'(_5031496,_5031498,_5031500,_5031502,_5031504,_5031506))).
'blocked_blocked_Normalize.foldConstantsAdd_ComplexCase'('Prelude.(,)'(_5025946,_5025964),_5028078,_5028098,_5031892,_5031898,_5031904):-!,makeShare(_5026004,_5032520),makeShare(_5026264,_5032540),makeShare(_5028078,_5032560),makeShare(_5028098,_5032580),makeShare(_5025964,_5032600),hnf('Prelude.cond'(letrec4PAKCS(_5032520,'Prelude.cond'(letrec4PAKCS(_5032540,'RadExpr.Add'(_5032560,_5032580)),'Normalize.foldConstantsAdd._\'23caseor0'(_5032600,_5032580,_5032560,_5032540))),'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0'(_5025946,_5032600,_5032560,_5032580,_5032520)),_5031892,_5031898,_5031904).
'blocked_blocked_Normalize.foldConstantsAdd_ComplexCase'('FAIL'(_5037160),_5028078,_5028098,'FAIL'(_5037160),_5037174,_5037174).

'Normalize.foldConstantsMul'(_5038358,_5038360,_5038362,_5038364,_5038366):-freeze(_5038364,'blocked_Normalize.foldConstantsMul'(_5038358,_5038360,_5038362,_5038364,_5038366)).
'blocked_Normalize.foldConstantsMul'(_5038454,_5038472,_5042398,_5042404,_5042410):-makeShare(_5038454,_5040874),makeShare(_5038472,_5040894),hnf('Prelude.(,)'(_5040874,_5040894),_5043822,_5042404,_5043788),'blocked_Normalize.foldConstantsMul_ComplexCase'(_5043822,_5040874,_5040894,_5042398,_5043788,_5042410).

'blocked_Normalize.foldConstantsMul_ComplexCase'(_5044292,_5044294,_5044296,_5044298,_5044300,_5044302):-freeze(_5044300,freeze(_5044292,'blocked_blocked_Normalize.foldConstantsMul_ComplexCase'(_5044292,_5044294,_5044296,_5044298,_5044300,_5044302))).
'blocked_blocked_Normalize.foldConstantsMul_ComplexCase'('Prelude.(,)'(_5038742,_5038760),_5040874,_5040894,_5044688,_5044694,_5044700):-!,makeShare(_5038800,_5045316),makeShare(_5039060,_5045336),makeShare(_5040874,_5045356),makeShare(_5040894,_5045376),makeShare(_5038760,_5045396),hnf('Prelude.cond'(letrec4PAKCS(_5045316,'Prelude.cond'(letrec4PAKCS(_5045336,'RadExpr.Mul'(_5045356,_5045376)),'Normalize.foldConstantsMul._\'23caseor0'(_5045396,_5045356,_5045376,_5045336))),'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0'(_5038742,_5045356,_5045376,_5045396,_5045316)),_5044688,_5044694,_5044700).
'blocked_blocked_Normalize.foldConstantsMul_ComplexCase'('FAIL'(_5049956),_5040874,_5040894,'FAIL'(_5049956),_5049970,_5049970).

'Normalize.simplifyPowers'(_5051078,_5051080,_5051082,_5051084):-freeze(_5051082,'blocked_Normalize.simplifyPowers'(_5051078,_5051080,_5051082,_5051084)).
'blocked_Normalize.simplifyPowers'(_5051164,_5057090,_5057096,_5057102):-makeShare(_5051164,_5055290),hnf(_5055290,_5058332,_5057096,_5058350),'blocked_Normalize.simplifyPowers_1'(_5058332,_5058332,_5057090,_5058350,_5057102).

'blocked_Normalize.simplifyPowers_1'(_5058762,_5058764,_5058766,_5058768,_5058770):-freeze(_5058768,freeze(_5058762,'blocked_blocked_Normalize.simplifyPowers_1'(_5058762,_5058764,_5058766,_5058768,_5058770))).
'blocked_blocked_Normalize.simplifyPowers_1'('RadExpr.Mul'(_5051280,_5051298),_5055290,_5059162,_5059168,_5059174):-makeShare(_5051338,_5059420),makeShare(_5051280,_5059440),makeShare(_5051298,_5059460),hnf('Prelude.cond'(letrec4PAKCS(_5059420,'RadExpr.Mul'('Normalize.simplifyPowers'(_5059440),'Normalize.simplifyPowers'(_5059460))),'Normalize.simplifyPowers._\'23caseor0'(_5059440,_5059440,_5059460,_5059420)),_5059162,_5059168,_5059174).
'blocked_blocked_Normalize.simplifyPowers_1'('RadExpr.Pow'(_5052372,_5052390),_5055290,_5062870,_5062876,_5062882):-makeShare(_5052430,_5063100),makeShare(_5052372,_5063120),makeShare(_5052390,_5063140),hnf('Prelude.cond'(letrec4PAKCS(_5063100,'RadExpr.Pow'('Normalize.simplifyPowers'(_5063120),_5063140)),'Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0'(_5063120,_5063140,_5063100)),_5062870,_5062876,_5062882).
'blocked_blocked_Normalize.simplifyPowers_1'('RadExpr.Root'(_5053310,_5053328),_5055290,_5066376,_5066382,_5066388):-makeShare(_5053368,_5066606),makeShare(_5053310,_5066626),makeShare(_5053328,_5066646),hnf('Prelude.cond'(letrec4PAKCS(_5066606,'RadExpr.Root'(_5066626,'Normalize.simplifyPowers'(_5066646))),'Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5066646,_5066626,_5066606)),_5066376,_5066382,_5066388).
'blocked_blocked_Normalize.simplifyPowers_1'('RadExpr.Neg'(_5054248),_5055290,'RadExpr.Neg'('Normalize.simplifyPowers'(_5054248)),_5069946,_5069946).
'blocked_blocked_Normalize.simplifyPowers_1'('RadExpr.Add'(_5054526,_5054544),_5055290,'RadExpr.Add'('Normalize.simplifyPowers'(_5054526),'Normalize.simplifyPowers'(_5054544)),_5071176,_5071176).
'blocked_blocked_Normalize.simplifyPowers_1'('RadExpr.Inv'(_5054982),_5055290,'RadExpr.Inv'('Normalize.simplifyPowers'(_5054982)),_5072822,_5072822).
'blocked_blocked_Normalize.simplifyPowers_1'('RadExpr.Lit'(_5055260),_5055290,_5074038,_5074044,_5074050):-!,hnf(_5055290,_5074038,_5074044,_5074050).
'blocked_blocked_Normalize.simplifyPowers_1'('FAIL'(_5074570),_5055290,'FAIL'(_5074570),_5074584,_5074584).

'Normalize.extractPerfectPowers'(_5075912,_5075914,_5075916,_5075918):-freeze(_5075916,'blocked_Normalize.extractPerfectPowers'(_5075912,_5075914,_5075916,_5075918)).
'blocked_Normalize.extractPerfectPowers'(_5075998,_5080420,_5080426,_5080432):-makeShare(_5075998,_5078922),hnf(_5078922,_5081878,_5080426,_5081896),'blocked_Normalize.extractPerfectPowers_1'(_5081878,_5081878,_5080420,_5081896,_5080432).

'blocked_Normalize.extractPerfectPowers_1'(_5082344,_5082346,_5082348,_5082350,_5082352):-freeze(_5082350,freeze(_5082344,'blocked_blocked_Normalize.extractPerfectPowers_1'(_5082344,_5082346,_5082348,_5082350,_5082352))).
'blocked_blocked_Normalize.extractPerfectPowers_1'('RadExpr.Root'(_5076114,_5076132),_5078922,_5082756,_5082762,_5082768):-makeShare(_5076172,_5082986),makeShare(_5076114,_5083006),makeShare(_5076132,_5083026),hnf('Prelude.cond'(letrec4PAKCS(_5082986,'RadExpr.Root'(_5083006,'Normalize.extractPerfectPowers'(_5083026))),'Normalize.extractPerfectPowers._\'23caseor0'(_5083026,_5083006,_5082986)),_5082756,_5082762,_5082768).
'blocked_blocked_Normalize.extractPerfectPowers_1'('RadExpr.Neg'(_5077052),_5078922,'RadExpr.Neg'('Normalize.extractPerfectPowers'(_5077052)),_5086218,_5086218).
'blocked_blocked_Normalize.extractPerfectPowers_1'('RadExpr.Add'(_5077330,_5077348),_5078922,'RadExpr.Add'('Normalize.extractPerfectPowers'(_5077330),'Normalize.extractPerfectPowers'(_5077348)),_5087520,_5087520).
'blocked_blocked_Normalize.extractPerfectPowers_1'('RadExpr.Mul'(_5077786,_5077804),_5078922,'RadExpr.Mul'('Normalize.extractPerfectPowers'(_5077786),'Normalize.extractPerfectPowers'(_5077804)),_5089282,_5089282).
'blocked_blocked_Normalize.extractPerfectPowers_1'('RadExpr.Inv'(_5078242),_5078922,'RadExpr.Inv'('Normalize.extractPerfectPowers'(_5078242)),_5091036,_5091036).
'blocked_blocked_Normalize.extractPerfectPowers_1'('RadExpr.Pow'(_5078520,_5078538),_5078922,'RadExpr.Pow'('Normalize.extractPerfectPowers'(_5078520),_5078538),_5092338,_5092338).
'blocked_blocked_Normalize.extractPerfectPowers_1'('RadExpr.Lit'(_5078892),_5078922,_5093796,_5093802,_5093808):-!,hnf(_5078922,_5093796,_5093802,_5093808).
'blocked_blocked_Normalize.extractPerfectPowers_1'('FAIL'(_5094364),_5078922,'FAIL'(_5094364),_5094378,_5094378).

'Normalize.extractPerfectPowersLit'(_5095820,_5095822,_5095824,_5095826,_5095828):-freeze(_5095826,'blocked_Normalize.extractPerfectPowersLit'(_5095820,_5095822,_5095824,_5095826,_5095828)).
'blocked_Normalize.extractPerfectPowersLit'(_5095916,_5095934,_5107630,_5107636,_5107642):-makeShare(_5095934,_5104654),hnf('Rational.ratGt'(_5104654,'Normalize.rZero'),_5109306,_5107636,_5109272),'blocked_Normalize.extractPerfectPowersLit_ComplexCase'(_5109306,_5095916,_5104654,_5107630,_5109272,_5107642).

'blocked_Normalize.extractPerfectPowersLit_ComplexCase'(_5109806,_5109808,_5109810,_5109812,_5109814,_5109816):-freeze(_5109814,freeze(_5109806,'blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase'(_5109806,_5109808,_5109810,_5109812,_5109814,_5109816))).
'blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase'('Prelude.True',_5095916,_5104654,_5110210,_5110216,_5110222):-makeShare(_5096228,_5112032),makeShare(_5104654,_5112052),makeShare(_5096246,_5112072),makeShare(_5096264,_5112092),makeShare(_5095916,_5112112),makeShare(_5096282,_5112132),makeShare(_5096300,_5112152),makeShare(_5096318,_5112172),makeShare(_5096336,_5112192),makeShare(_5096354,_5112212),hnf('Prelude.cond'(letrec4PAKCS(_5112032,'Rational.numerator'(_5112052)),'Prelude.cond'(letrec4PAKCS(_5112072,'Rational.denominator'(_5112052)),'Prelude.cond'(letrec4PAKCS(_5112092,'Normalize.extractNthPower'(_5112112,'Normalize.absInt'(_5112032))),'Prelude.cond'(letrec4PAKCS(_5112132,'Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut'(_5112092)),'Prelude.cond'(letrec4PAKCS(_5112152,'Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn'(_5112092)),'Prelude.cond'(letrec4PAKCS(_5112172,'Normalize.extractNthPower'(_5112112,'Normalize.absInt'(_5112072))),'Prelude.cond'(letrec4PAKCS(_5112192,'Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut'(_5112172)),'Prelude.cond'(letrec4PAKCS(_5112212,'Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn'(_5112172)),'Normalize.extractPerfectPowersLit._\'23caseor0'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5112212,1),_5112152,_5112132,_5112192,_5112212,_5112112))))))))),_5110210,_5110216,_5110222).
'blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase'('Prelude.False',_5095916,_5104654,_5127582,_5127588,_5127594):-!,makeShare(_5104654,_5124622),makeShare(_5095916,_5124642),hnf('Prelude.&&'('Rational.ratLt'(_5124622,'Normalize.rZero'),'Prelude.apply'('Prelude.odd'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[])),_5124642)),_5130406,_5127588,_5130372),'blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase'(_5130406,_5124642,_5124622,_5127582,_5130372,_5127594).

'blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase'(_5131140,_5131142,_5131144,_5131146,_5131148,_5131150):-freeze(_5131148,freeze(_5131140,'blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase'(_5131140,_5131142,_5131144,_5131146,_5131148,_5131150))).
'blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5124642,_5124622,'RadExpr.Neg'('Normalize.extractPerfectPowers'('RadExpr.Root'(_5124642,'RadExpr.Lit'('Rational.ratNeg'(_5124622))))),_5131550,_5131550).
'blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5124642,_5124622,_5135004,_5135010,_5135016):-!,hnf('Prelude.otherwise',_5139052,_5135010,_5139018),'blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5139052,_5124642,_5124622,_5135004,_5139018,_5135016).

'blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5139966,_5139968,_5139970,_5139972,_5139974,_5139976):-freeze(_5139974,freeze(_5139966,'blocked_blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5139966,_5139968,_5139970,_5139972,_5139974,_5139976))).
'blocked_blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5124642,_5124622,'RadExpr.Root'(_5124642,'RadExpr.Lit'(_5124622)),_5140376,_5140376).
'blocked_blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5124642,_5124622,_5142168,_5142174,_5142180):-!,hnf(reportFailure4PAKCS('Normalize.extractPerfectPowersLit',['Prelude.False']),_5142168,_5142174,_5142180).
'blocked_blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5144066),_5124642,_5124622,'FAIL'(_5144066),_5144080,_5144080).
'blocked_blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5144148),_5124642,_5124622,'FAIL'(_5144148),_5144162,_5144162).
'blocked_blocked_Normalize.extractPerfectPowersLit_ComplexCase'('FAIL'(_5144230),_5095916,_5104654,'FAIL'(_5144230),_5144244,_5144244).

'Normalize.extractPerfectPowersLit.buildExtracted.145'(_5146416,_5146418,_5146420,_5146422,_5146424,_5146426):-freeze(_5146424,'blocked_Normalize.extractPerfectPowersLit.buildExtracted.145'(_5146416,_5146418,_5146420,_5146422,_5146424,_5146426)).
'blocked_Normalize.extractPerfectPowersLit.buildExtracted.145'(_5146522,_5146540,_5146558,_5148158,_5148164,_5148170):-makeShare(_5146586,_5148620),makeShare(_5146540,_5148640),makeShare(_5146604,_5148660),makeShare(_5146558,_5148680),hnf('Prelude.cond'(letrec4PAKCS(_5148620,'Rational.ratEq'(_5148640,'Normalize.rOne')),'Prelude.cond'(letrec4PAKCS(_5148660,'Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5148680,1)),'Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0'('Prelude.&&'(_5148620,_5148660),_5148620,_5148660,_5148640,_5146522,_5148680))),_5148158,_5148164,_5148170).

'Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut'(_5155500,_5155502,_5155504,_5155506):-freeze(_5155504,'blocked_Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut'(_5155500,_5155502,_5155504,_5155506)).
'blocked_Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut'(_5155586,_5156150,_5156156,_5156162):-hnf(_5155586,_5158436,_5156156,_5158448),'blocked_Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut_1'(_5158436,_5156150,_5158448,_5156162).

'blocked_Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut_1'(_5159008,_5159010,_5159012,_5159014):-freeze(_5159012,'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut_1'(_5159008,_5159010,_5159012,_5159014)).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut_1'('Prelude.(,)'(_5155702,_5155720),_5159378,_5159384,_5159390):-!,hnf(_5155702,_5159378,_5159384,_5159390).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP8\'23numOut_1'('FAIL'(_5160076),'FAIL'(_5160076),_5160090,_5160090):-nonvar(_5160076).

'Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn'(_5162154,_5162156,_5162158,_5162160):-freeze(_5162158,'blocked_Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn'(_5162154,_5162156,_5162158,_5162160)).
'blocked_Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn'(_5162240,_5162798,_5162804,_5162810):-hnf(_5162240,_5165048,_5162804,_5165060),'blocked_Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn_1'(_5165048,_5162798,_5165060,_5162810).

'blocked_Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn_1'(_5165614,_5165616,_5165618,_5165620):-freeze(_5165618,'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn_1'(_5165614,_5165616,_5165618,_5165620)).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn_1'('Prelude.(,)'(_5162356,_5162374),_5165984,_5165990,_5165996):-!,hnf(_5162374,_5165984,_5165990,_5165996).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP9\'23numIn_1'('FAIL'(_5166676),'FAIL'(_5166676),_5166690,_5166690):-nonvar(_5166676).

'Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut'(_5168792,_5168794,_5168796,_5168798):-freeze(_5168796,'blocked_Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut'(_5168792,_5168794,_5168796,_5168798)).
'blocked_Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut'(_5168878,_5169442,_5169448,_5169454):-hnf(_5168878,_5171728,_5169448,_5171740),'blocked_Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut_1'(_5171728,_5169442,_5171740,_5169454).

'blocked_Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut_1'(_5172300,_5172302,_5172304,_5172306):-freeze(_5172304,'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut_1'(_5172300,_5172302,_5172304,_5172306)).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut_1'('Prelude.(,)'(_5168994,_5169012),_5172670,_5172676,_5172682):-!,hnf(_5168994,_5172670,_5172676,_5172682).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP6\'23denOut_1'('FAIL'(_5173368),'FAIL'(_5173368),_5173382,_5173382):-nonvar(_5173368).

'Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn'(_5175446,_5175448,_5175450,_5175452):-freeze(_5175450,'blocked_Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn'(_5175446,_5175448,_5175450,_5175452)).
'blocked_Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn'(_5175532,_5176090,_5176096,_5176102):-hnf(_5175532,_5178340,_5176096,_5178352),'blocked_Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn_1'(_5178340,_5176090,_5178352,_5176102).

'blocked_Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn_1'(_5178906,_5178908,_5178910,_5178912):-freeze(_5178910,'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn_1'(_5178906,_5178908,_5178910,_5178912)).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn_1'('Prelude.(,)'(_5175648,_5175666),_5179276,_5179282,_5179288):-!,hnf(_5175666,_5179276,_5179282,_5179288).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP7\'23denIn_1'('FAIL'(_5179968),'FAIL'(_5179968),_5179982,_5179982):-nonvar(_5179968).

'Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2'(_5182122,_5182124,_5182126,_5182128):-freeze(_5182126,'blocked_Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2'(_5182122,_5182124,_5182126,_5182128)).
'blocked_Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2'(_5182208,_5182778,_5182784,_5182790):-hnf(_5182208,_5185100,_5182784,_5185112),'blocked_Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2_1'(_5185100,_5182778,_5185112,_5182790).

'blocked_Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2_1'(_5185678,_5185680,_5185682,_5185684):-freeze(_5185682,'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2_1'(_5185678,_5185680,_5185682,_5185684)).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2_1'('Prelude.(,)'(_5182324,_5182342),_5186048,_5186054,_5186060):-!,hnf(_5182324,_5186048,_5186054,_5186060).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2_1'('FAIL'(_5186752),'FAIL'(_5186752),_5186766,_5186766):-nonvar(_5186752).

'Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2'(_5188868,_5188870,_5188872,_5188874):-freeze(_5188872,'blocked_Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2'(_5188868,_5188870,_5188872,_5188874)).
'blocked_Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2'(_5188954,_5189518,_5189524,_5189530):-hnf(_5188954,_5191804,_5189524,_5191816),'blocked_Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2_1'(_5191804,_5189518,_5191816,_5189530).

'blocked_Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2_1'(_5192376,_5192378,_5192380,_5192382):-freeze(_5192380,'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2_1'(_5192376,_5192378,_5192380,_5192382)).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2_1'('Prelude.(,)'(_5189070,_5189088),_5192746,_5192752,_5192758):-!,hnf(_5189088,_5192746,_5192752,_5192758).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2_1'('FAIL'(_5193444),'FAIL'(_5193444),_5193458,_5193458):-nonvar(_5193444).

'Normalize.intPow'(_5194250,_5194252,_5194254,_5194256,_5194258):-freeze(_5194256,'blocked_Normalize.intPow'(_5194250,_5194252,_5194254,_5194256,_5194258)).
'blocked_Normalize.intPow'(_5194346,_5194364,_5197378,_5197384,_5197390):-makeShare(_5194364,_5195876),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5195876,0),_5198442,_5197384,_5198408),'blocked_Normalize.intPow_ComplexCase'(_5198442,_5194346,_5195876,_5197378,_5198408,_5197390).

'blocked_Normalize.intPow_ComplexCase'(_5198840,_5198842,_5198844,_5198846,_5198848,_5198850):-freeze(_5198848,freeze(_5198840,'blocked_blocked_Normalize.intPow_ComplexCase'(_5198840,_5198842,_5198844,_5198846,_5198848,_5198850))).
'blocked_blocked_Normalize.intPow_ComplexCase'('Prelude.True',_5194346,_5195876,1,_5199250,_5199250).
'blocked_blocked_Normalize.intPow_ComplexCase'('Prelude.False',_5194346,_5195876,_5201004,_5201010,_5201016):-!,hnf('Prelude.otherwise',_5203216,_5201010,_5203182),'blocked_blocked_Normalize.intPow_ComplexCase_Prelude.False_ComplexCase'(_5203216,_5194346,_5195876,_5201004,_5203182,_5201016).

'blocked_blocked_Normalize.intPow_ComplexCase_Prelude.False_ComplexCase'(_5203824,_5203826,_5203828,_5203830,_5203832,_5203834):-freeze(_5203832,freeze(_5203824,'blocked_blocked_blocked_Normalize.intPow_ComplexCase_Prelude.False_ComplexCase'(_5203824,_5203826,_5203828,_5203830,_5203832,_5203834))).
'blocked_blocked_blocked_Normalize.intPow_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5194346,_5195876,_5204228,_5204234,_5204240):-makeShare(_5194346,_5204362),hnf('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_5204362,'Normalize.intPow'(_5204362,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_5195876,1))),_5204228,_5204234,_5204240).
'blocked_blocked_blocked_Normalize.intPow_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5194346,_5195876,_5206824,_5206830,_5206836):-!,hnf(reportFailure4PAKCS('Normalize.intPow',['Prelude.False']),_5206824,_5206830,_5206836).
'blocked_blocked_blocked_Normalize.intPow_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5208314),_5194346,_5195876,'FAIL'(_5208314),_5208328,_5208328).
'blocked_blocked_Normalize.intPow_ComplexCase'('FAIL'(_5208396),_5194346,_5195876,'FAIL'(_5208396),_5208410,_5208410).

'Normalize.absInt'(_5209214,_5209216,_5209218,_5209220):-freeze(_5209218,'blocked_Normalize.absInt'(_5209214,_5209216,_5209218,_5209220)).
'blocked_Normalize.absInt'(_5209300,_5211494,_5211500,_5211506):-makeShare(_5209300,_5209992),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_5209992),0),_5212544,_5211500,_5212516),'blocked_Normalize.absInt_ComplexCase'(_5212544,_5209992,_5211494,_5212516,_5211506).

'blocked_Normalize.absInt_ComplexCase'(_5212940,_5212942,_5212944,_5212946,_5212948):-freeze(_5212946,freeze(_5212940,'blocked_blocked_Normalize.absInt_ComplexCase'(_5212940,_5212942,_5212944,_5212946,_5212948))).
'blocked_blocked_Normalize.absInt_ComplexCase'('Prelude.True',_5209992,_5213334,_5213340,_5213346):-hnf('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_5209992),_5213334,_5213340,_5213346).
'blocked_blocked_Normalize.absInt_ComplexCase'('Prelude.False',_5209992,_5214536,_5214542,_5214548):-!,hnf(_5209992,_5214536,_5214542,_5214548).
'blocked_blocked_Normalize.absInt_ComplexCase'('FAIL'(_5215014),_5209992,'FAIL'(_5215014),_5215028,_5215028).

'Normalize.extractNthPower'(_5216166,_5216168,_5216170,_5216172,_5216174):-freeze(_5216172,'blocked_Normalize.extractNthPower'(_5216166,_5216168,_5216170,_5216172,_5216174)).
'blocked_Normalize.extractNthPower'(_5216262,_5216280,_5218408,_5218414,_5218420):-makeShare(_5216308,_5218874),makeShare(_5216326,_5218894),makeShare(_5216262,_5218914),makeShare(_5216344,_5218934),hnf('Prelude.cond'(letrec4PAKCS(_5218874,'PrimeFactors.factorise'('Positive.unsafePositive'('Normalize.absInt'(_5216280)))),'Prelude.cond'(letrec4PAKCS(_5218894,'Prelude.foldl'(partcall(2,'Normalize.extractNthPower._\'23lambda11',[_5218914]),1,_5218874)),'Prelude.cond'(letrec4PAKCS(_5218934,'Prelude.foldl'(partcall(2,'Normalize.extractNthPower._\'23lambda12',[_5218914]),1,_5218874)),'Prelude.(,)'('Rational.fromInt'(_5218894),_5218934)))),_5218408,_5218414,_5218420).

'Normalize.extractNthPower._\'23lambda11'(_5226002,_5226004,_5226006,_5226008,_5226010,_5226012):-freeze(_5226010,'blocked_Normalize.extractNthPower._\'23lambda11'(_5226002,_5226004,_5226006,_5226008,_5226010,_5226012)).
'blocked_Normalize.extractNthPower._\'23lambda11'(_5226108,_5226126,_5226144,_5227264,_5227270,_5227276):-hnf(_5226144,_5229026,_5227270,_5229050),'blocked_Normalize.extractNthPower._\'23lambda11_3'(_5229026,_5226108,_5226126,_5227264,_5229050,_5227276).

'blocked_Normalize.extractNthPower._\'23lambda11_3'(_5229536,_5229538,_5229540,_5229542,_5229544,_5229546):-freeze(_5229544,'blocked_blocked_Normalize.extractNthPower._\'23lambda11_3'(_5229536,_5229538,_5229540,_5229542,_5229544,_5229546)).
'blocked_blocked_Normalize.extractNthPower._\'23lambda11_3'('Prelude.(,)'(_5226260,_5226278),_5226108,_5226126,_5229926,_5229932,_5229938):-!,hnf('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_5226126,'Normalize.intPow'(_5226260,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23div\'23Prelude.Integral\'23Prelude.Int\'23',_5226278),_5226108))),_5229926,_5229932,_5229938).
'blocked_blocked_Normalize.extractNthPower._\'23lambda11_3'('FAIL'(_5232468),_5226108,_5226126,'FAIL'(_5232468),_5232482,_5232482):-nonvar(_5232468).

'Normalize.extractNthPower._\'23lambda12'(_5234078,_5234080,_5234082,_5234084,_5234086,_5234088):-freeze(_5234086,'blocked_Normalize.extractNthPower._\'23lambda12'(_5234078,_5234080,_5234082,_5234084,_5234086,_5234088)).
'blocked_Normalize.extractNthPower._\'23lambda12'(_5234184,_5234202,_5234220,_5235340,_5235346,_5235352):-hnf(_5234220,_5237102,_5235346,_5237126),'blocked_Normalize.extractNthPower._\'23lambda12_3'(_5237102,_5234184,_5234202,_5235340,_5237126,_5235352).

'blocked_Normalize.extractNthPower._\'23lambda12_3'(_5237612,_5237614,_5237616,_5237618,_5237620,_5237622):-freeze(_5237620,'blocked_blocked_Normalize.extractNthPower._\'23lambda12_3'(_5237612,_5237614,_5237616,_5237618,_5237620,_5237622)).
'blocked_blocked_Normalize.extractNthPower._\'23lambda12_3'('Prelude.(,)'(_5234336,_5234354),_5234184,_5234202,_5238002,_5238008,_5238014):-!,hnf('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_5234202,'Normalize.intPow'(_5234336,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23mod\'23Prelude.Integral\'23Prelude.Int\'23',_5234354),_5234184))),_5238002,_5238008,_5238014).
'blocked_blocked_Normalize.extractNthPower._\'23lambda12_3'('FAIL'(_5240544),_5234184,_5234202,'FAIL'(_5240544),_5240558,_5240558):-nonvar(_5240544).

'Normalize.collectCoefficients'(_5241860,_5241862,_5241864,_5241866):-freeze(_5241864,'blocked_Normalize.collectCoefficients'(_5241860,_5241862,_5241864,_5241866)).
'blocked_Normalize.collectCoefficients'(_5241946,_5250214,_5250220,_5250226):-makeShare(_5241946,_5247806),hnf(_5247806,_5251636,_5250220,_5251654),'blocked_Normalize.collectCoefficients_1'(_5251636,_5251636,_5250214,_5251654,_5250226).

'blocked_Normalize.collectCoefficients_1'(_5252096,_5252098,_5252100,_5252102,_5252104):-freeze(_5252102,freeze(_5252096,'blocked_blocked_Normalize.collectCoefficients_1'(_5252096,_5252098,_5252100,_5252102,_5252104))).
'blocked_blocked_Normalize.collectCoefficients_1'('RadExpr.Mul'(_5242062,_5242080),_5247806,_5252496,_5252502,_5252508):-makeShare(_5242120,_5253318),makeShare(_5242138,_5253338),makeShare(_5242156,_5253358),makeShare(_5242174,_5253378),makeShare(_5242192,_5253398),makeShare(_5242210,_5253418),hnf('Prelude.cond'(letrec4PAKCS(_5253318,'Normalize.flattenMulN'(_5247806)),'Prelude.cond'(letrec4PAKCS(_5253338,'Normalize.partitionLitsN'('Prelude.map'(partcall(1,'Normalize.collectCoefficients',[]),_5253318))),'Prelude.cond'(letrec4PAKCS(_5253358,'Normalize.collectCoefficients._\'23selFP11\'23lits'(_5253338)),'Prelude.cond'(letrec4PAKCS(_5253378,'Normalize.collectCoefficients._\'23selFP12\'23rest'(_5253338)),'Prelude.cond'(letrec4PAKCS(_5253398,'Prelude.foldl'(partcall(2,'Rational.ratMul',[]),'Normalize.rOne',_5253358)),'Prelude.cond'(letrec4PAKCS(_5253418,'Normalize.buildMulN'(_5253378)),'Normalize.applyCoeffMul'(_5253398,_5253418))))))),_5252496,_5252502,_5252508).
'blocked_blocked_Normalize.collectCoefficients_1'('RadExpr.Neg'(_5245192),_5247806,_5261948,_5261954,_5261960):-makeShare(_5245226,_5262170),makeShare(_5245244,_5262190),hnf('Prelude.cond'(letrec4PAKCS(_5262170,'Normalize.collectCoefficients'(_5245192)),'Prelude.cond'(letrec4PAKCS(_5262190,'RadExpr.Neg'(_5262170)),'Normalize.collectCoefficients._\'23caseor0'(_5262170,_5262190))),_5261948,_5261954,_5261960).
'blocked_blocked_Normalize.collectCoefficients_1'('RadExpr.Add'(_5246298,_5246316),_5247806,'RadExpr.Add'('Normalize.collectCoefficients'(_5246298),'Normalize.collectCoefficients'(_5246316)),_5265500,_5265500).
'blocked_blocked_Normalize.collectCoefficients_1'('RadExpr.Inv'(_5246754),_5247806,'RadExpr.Inv'('Normalize.collectCoefficients'(_5246754)),_5267236,_5267236).
'blocked_blocked_Normalize.collectCoefficients_1'('RadExpr.Root'(_5247032,_5247050),_5247806,'RadExpr.Root'(_5247032,'Normalize.collectCoefficients'(_5247050)),_5268538,_5268538).
'blocked_blocked_Normalize.collectCoefficients_1'('RadExpr.Pow'(_5247404,_5247422),_5247806,'RadExpr.Pow'('Normalize.collectCoefficients'(_5247404),_5247422),_5270004,_5270004).
'blocked_blocked_Normalize.collectCoefficients_1'('RadExpr.Lit'(_5247776),_5247806,_5271450,_5271456,_5271462):-!,hnf(_5247806,_5271450,_5271456,_5271462).
'blocked_blocked_Normalize.collectCoefficients_1'('FAIL'(_5272012),_5247806,'FAIL'(_5272012),_5272026,_5272026).

'Normalize.collectCoefficients._\'23selFP11\'23lits'(_5273942,_5273944,_5273946,_5273948):-freeze(_5273946,'blocked_Normalize.collectCoefficients._\'23selFP11\'23lits'(_5273942,_5273944,_5273946,_5273948)).
'blocked_Normalize.collectCoefficients._\'23selFP11\'23lits'(_5274028,_5274562,_5274568,_5274574):-hnf(_5274028,_5276668,_5274568,_5276680),'blocked_Normalize.collectCoefficients._\'23selFP11\'23lits_1'(_5276668,_5274562,_5276680,_5274574).

'blocked_Normalize.collectCoefficients._\'23selFP11\'23lits_1'(_5277210,_5277212,_5277214,_5277216):-freeze(_5277214,'blocked_blocked_Normalize.collectCoefficients._\'23selFP11\'23lits_1'(_5277210,_5277212,_5277214,_5277216)).
'blocked_blocked_Normalize.collectCoefficients._\'23selFP11\'23lits_1'('Prelude.(,)'(_5274144,_5274162),_5277580,_5277586,_5277592):-!,hnf(_5274144,_5277580,_5277586,_5277592).
'blocked_blocked_Normalize.collectCoefficients._\'23selFP11\'23lits_1'('FAIL'(_5278248),'FAIL'(_5278248),_5278262,_5278262):-nonvar(_5278248).

'Normalize.collectCoefficients._\'23selFP12\'23rest'(_5280174,_5280176,_5280178,_5280180):-freeze(_5280178,'blocked_Normalize.collectCoefficients._\'23selFP12\'23rest'(_5280174,_5280176,_5280178,_5280180)).
'blocked_Normalize.collectCoefficients._\'23selFP12\'23rest'(_5280260,_5280794,_5280800,_5280806):-hnf(_5280260,_5282900,_5280800,_5282912),'blocked_Normalize.collectCoefficients._\'23selFP12\'23rest_1'(_5282900,_5280794,_5282912,_5280806).

'blocked_Normalize.collectCoefficients._\'23selFP12\'23rest_1'(_5283442,_5283444,_5283446,_5283448):-freeze(_5283446,'blocked_blocked_Normalize.collectCoefficients._\'23selFP12\'23rest_1'(_5283442,_5283444,_5283446,_5283448)).
'blocked_blocked_Normalize.collectCoefficients._\'23selFP12\'23rest_1'('Prelude.(,)'(_5280376,_5280394),_5283812,_5283818,_5283824):-!,hnf(_5280394,_5283812,_5283818,_5283824).
'blocked_blocked_Normalize.collectCoefficients._\'23selFP12\'23rest_1'('FAIL'(_5284480),'FAIL'(_5284480),_5284494,_5284494):-nonvar(_5284480).

'Normalize.flattenMulN'(_5285476,_5285478,_5285480,_5285482):-freeze(_5285480,'blocked_Normalize.flattenMulN'(_5285476,_5285478,_5285480,_5285482)).
'blocked_Normalize.flattenMulN'(_5285562,_5286242,_5286248,_5286254):-makeShare(_5285590,_5286388),makeShare(_5285562,_5286408),hnf('Prelude.cond'(letrec4PAKCS(_5286388,[_5286408]),'Normalize.flattenMulN._\'23caseor0'(_5286408,_5286388)),_5286242,_5286248,_5286254).

'Normalize.partitionLitsN'(_5289394,_5289396,_5289398,_5289400):-freeze(_5289398,'blocked_Normalize.partitionLitsN'(_5289394,_5289396,_5289398,_5289400)).
'blocked_Normalize.partitionLitsN'(_5289480,_5292400,_5292406,_5292412):-hnf(_5289480,_5293642,_5292406,_5293654),'blocked_Normalize.partitionLitsN_1'(_5293642,_5292400,_5293654,_5292412).

'blocked_Normalize.partitionLitsN_1'(_5294040,_5294042,_5294044,_5294046):-freeze(_5294044,'blocked_blocked_Normalize.partitionLitsN_1'(_5294040,_5294042,_5294044,_5294046)).
'blocked_blocked_Normalize.partitionLitsN_1'([],'Prelude.(,)'([],[]),_5294288,_5294288).
'blocked_blocked_Normalize.partitionLitsN_1'([_5289864|_5289882],_5295106,_5295112,_5295118):-!,makeShare(_5289922,_5295868),makeShare(_5289940,_5295888),makeShare(_5289958,_5295908),makeShare(_5289976,_5295928),makeShare(_5289864,_5295948),hnf('Prelude.cond'(letrec4PAKCS(_5295868,'Normalize.partitionLitsN'(_5289882)),'Prelude.cond'(letrec4PAKCS(_5295888,'Normalize.partitionLitsN._\'23selFP14\'23ls'(_5295868)),'Prelude.cond'(letrec4PAKCS(_5295908,'Normalize.partitionLitsN._\'23selFP15\'23rs'(_5295868)),'Prelude.cond'(letrec4PAKCS(_5295928,'Prelude.(,)'(_5295888,[_5295948|_5295908])),'Normalize.partitionLitsN._\'23caseor0'(_5295948,_5295888,_5295948,_5295908,_5295928))))),_5295106,_5295112,_5295118).
'blocked_blocked_Normalize.partitionLitsN_1'('FAIL'(_5301882),'FAIL'(_5301882),_5301896,_5301896):-nonvar(_5301882).

'Normalize.partitionLitsN._\'23selFP14\'23ls'(_5303542,_5303544,_5303546,_5303548):-freeze(_5303546,'blocked_Normalize.partitionLitsN._\'23selFP14\'23ls'(_5303542,_5303544,_5303546,_5303548)).
'blocked_Normalize.partitionLitsN._\'23selFP14\'23ls'(_5303628,_5304120,_5304126,_5304132):-hnf(_5303628,_5305974,_5304126,_5305986),'blocked_Normalize.partitionLitsN._\'23selFP14\'23ls_1'(_5305974,_5304120,_5305986,_5304132).

'blocked_Normalize.partitionLitsN._\'23selFP14\'23ls_1'(_5306474,_5306476,_5306478,_5306480):-freeze(_5306478,'blocked_blocked_Normalize.partitionLitsN._\'23selFP14\'23ls_1'(_5306474,_5306476,_5306478,_5306480)).
'blocked_blocked_Normalize.partitionLitsN._\'23selFP14\'23ls_1'('Prelude.(,)'(_5303744,_5303762),_5306844,_5306850,_5306856):-!,hnf(_5303744,_5306844,_5306850,_5306856).
'blocked_blocked_Normalize.partitionLitsN._\'23selFP14\'23ls_1'('FAIL'(_5307470),'FAIL'(_5307470),_5307484,_5307484):-nonvar(_5307470).

'Normalize.partitionLitsN._\'23selFP15\'23rs'(_5309130,_5309132,_5309134,_5309136):-freeze(_5309134,'blocked_Normalize.partitionLitsN._\'23selFP15\'23rs'(_5309130,_5309132,_5309134,_5309136)).
'blocked_Normalize.partitionLitsN._\'23selFP15\'23rs'(_5309216,_5309708,_5309714,_5309720):-hnf(_5309216,_5311562,_5309714,_5311574),'blocked_Normalize.partitionLitsN._\'23selFP15\'23rs_1'(_5311562,_5309708,_5311574,_5309720).

'blocked_Normalize.partitionLitsN._\'23selFP15\'23rs_1'(_5312062,_5312064,_5312066,_5312068):-freeze(_5312066,'blocked_blocked_Normalize.partitionLitsN._\'23selFP15\'23rs_1'(_5312062,_5312064,_5312066,_5312068)).
'blocked_blocked_Normalize.partitionLitsN._\'23selFP15\'23rs_1'('Prelude.(,)'(_5309332,_5309350),_5312432,_5312438,_5312444):-!,hnf(_5309350,_5312432,_5312438,_5312444).
'blocked_blocked_Normalize.partitionLitsN._\'23selFP15\'23rs_1'('FAIL'(_5313058),'FAIL'(_5313058),_5313072,_5313072):-nonvar(_5313058).

'Normalize.buildMulN'(_5313978,_5313980,_5313982,_5313984):-freeze(_5313982,'blocked_Normalize.buildMulN'(_5313978,_5313980,_5313982,_5313984)).
'blocked_Normalize.buildMulN'(_5314064,_5315084,_5315090,_5315096):-hnf(_5314064,_5316146,_5315090,_5316158),'blocked_Normalize.buildMulN_1'(_5316146,_5315084,_5316158,_5315096).

'blocked_Normalize.buildMulN_1'(_5316520,_5316522,_5316524,_5316526):-freeze(_5316524,freeze(_5316520,'blocked_blocked_Normalize.buildMulN_1'(_5316520,_5316522,_5316524,_5316526))).
'blocked_blocked_Normalize.buildMulN_1'([],'RadExpr.Lit'('Normalize.rOne'),_5316774,_5316774).
'blocked_blocked_Normalize.buildMulN_1'([_5314364|_5314382],_5318386,_5318392,_5318398):-!,makeShare(_5314382,_5317570),hnf(_5317570,_5319928,_5318392,_5319952),'blocked_blocked_Normalize.buildMulN_1_[|]_2'(_5319928,_5314364,_5319928,_5318386,_5319952,_5318398).

'blocked_blocked_Normalize.buildMulN_1_[|]_2'(_5320444,_5320446,_5320448,_5320450,_5320452,_5320454):-freeze(_5320452,freeze(_5320444,'blocked_blocked_blocked_Normalize.buildMulN_1_[|]_2'(_5320444,_5320446,_5320448,_5320450,_5320452,_5320454))).
'blocked_blocked_blocked_Normalize.buildMulN_1_[|]_2'([],_5314364,_5317570,_5320712,_5320718,_5320724):-hnf(_5314364,_5320712,_5320718,_5320724).
'blocked_blocked_blocked_Normalize.buildMulN_1_[|]_2'([_5314596|_5314614],_5314364,_5317570,'RadExpr.Mul'(_5314364,'Normalize.buildMulN'(_5317570)),_5321484,_5321484):-!.
'blocked_blocked_blocked_Normalize.buildMulN_1_[|]_2'('FAIL'(_5322684),_5314364,_5317570,'FAIL'(_5322684),_5322698,_5322698).
'blocked_blocked_Normalize.buildMulN_1'('FAIL'(_5322766),'FAIL'(_5322766),_5322780,_5322780).

'Normalize.applyCoeffMul'(_5323834,_5323836,_5323838,_5323840,_5323842):-freeze(_5323840,'blocked_Normalize.applyCoeffMul'(_5323834,_5323836,_5323838,_5323840,_5323842)).
'blocked_Normalize.applyCoeffMul'(_5323930,_5323948,_5328890,_5328896,_5328902):-makeShare(_5323930,_5326902),hnf('Rational.ratEq'(_5326902,'Normalize.rZero'),_5330206,_5328896,_5330172),'blocked_Normalize.applyCoeffMul_ComplexCase'(_5330206,_5326902,_5323948,_5328890,_5330172,_5328902).

'blocked_Normalize.applyCoeffMul_ComplexCase'(_5330646,_5330648,_5330650,_5330652,_5330654,_5330656):-freeze(_5330654,freeze(_5330646,'blocked_blocked_Normalize.applyCoeffMul_ComplexCase'(_5330646,_5330648,_5330650,_5330652,_5330654,_5330656))).
'blocked_blocked_Normalize.applyCoeffMul_ComplexCase'('Prelude.True',_5326902,_5323948,'RadExpr.Lit'('Normalize.rZero'),_5331056,_5331056).
'blocked_blocked_Normalize.applyCoeffMul_ComplexCase'('Prelude.False',_5326902,_5323948,_5334386,_5334392,_5334398):-!,makeShare(_5326902,_5332500),hnf('Rational.ratEq'(_5332500,'Normalize.rOne'),_5336850,_5334392,_5336816),'blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase'(_5336850,_5332500,_5323948,_5334386,_5336816,_5334398).

'blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase'(_5337512,_5337514,_5337516,_5337518,_5337520,_5337522):-freeze(_5337520,freeze(_5337512,'blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase'(_5337512,_5337514,_5337516,_5337518,_5337520,_5337522))).
'blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5332500,_5323948,_5337916,_5337922,_5337928):-hnf(_5323948,_5337916,_5337922,_5337928).
'blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5332500,_5323948,_5341170,_5341176,_5341182):-!,makeShare(_5332500,_5339260),hnf('Rational.ratEq'(_5339260,'Normalize.rNegOne'),_5344858,_5341176,_5344824),'blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5344858,_5339260,_5323948,_5341170,_5344824,_5341182).

'blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5345724,_5345726,_5345728,_5345730,_5345732,_5345734):-freeze(_5345732,freeze(_5345724,'blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5345724,_5345726,_5345728,_5345730,_5345732,_5345734))).
'blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5339260,_5323948,'RadExpr.Neg'(_5323948),_5346134,_5346134).
'blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5339260,_5323948,_5349210,_5349216,_5349222):-!,hnf('Prelude.otherwise',_5354122,_5349216,_5354088),'blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5354122,_5339260,_5323948,_5349210,_5354088,_5349222).

'blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5355180,_5355182,_5355184,_5355186,_5355188,_5355190):-freeze(_5355188,freeze(_5355180,'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5355180,_5355182,_5355184,_5355186,_5355188,_5355190))).
'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5339260,_5323948,_5355584,_5355590,_5355596):-makeShare(_5325326,_5355822),makeShare(_5339260,_5355842),makeShare(_5323948,_5355862),hnf('Prelude.cond'(letrec4PAKCS(_5355822,'RadExpr.Mul'('RadExpr.Lit'(_5355842),_5355862)),'Normalize.applyCoeffMul._\'23caseor0'(_5355862,_5355842,_5355822)),_5355584,_5355590,_5355596).
'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5339260,_5323948,_5359458,_5359464,_5359470):-!,hnf(reportFailure4PAKCS('Normalize.applyCoeffMul',['Prelude.False']),_5359458,_5359464,_5359470).
'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5361440),_5339260,_5323948,'FAIL'(_5361440),_5361454,_5361454).
'blocked_blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5361522),_5339260,_5323948,'FAIL'(_5361522),_5361536,_5361536).
'blocked_blocked_blocked_Normalize.applyCoeffMul_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5361604),_5332500,_5323948,'FAIL'(_5361604),_5361618,_5361618).
'blocked_blocked_Normalize.applyCoeffMul_ComplexCase'('FAIL'(_5361686),_5326902,_5323948,'FAIL'(_5361686),_5361700,_5361700).

'Normalize.collectTerms'(_5362732,_5362734,_5362736,_5362738):-freeze(_5362736,'blocked_Normalize.collectTerms'(_5362732,_5362734,_5362736,_5362738)).
'blocked_Normalize.collectTerms'(_5362818,_5369618,_5369624,_5369630):-makeShare(_5362818,_5367616),hnf(_5367616,_5370788,_5369624,_5370806),'blocked_Normalize.collectTerms_1'(_5370788,_5370788,_5369618,_5370806,_5369630).

'blocked_Normalize.collectTerms_1'(_5371206,_5371208,_5371210,_5371212,_5371214):-freeze(_5371212,freeze(_5371206,'blocked_blocked_Normalize.collectTerms_1'(_5371206,_5371208,_5371210,_5371212,_5371214))).
'blocked_blocked_Normalize.collectTerms_1'('RadExpr.Add'(_5362934,_5362952),_5367616,_5371606,_5371612,_5371618):-makeShare(_5362992,_5372218),makeShare(_5363010,_5372238),makeShare(_5363028,_5372258),makeShare(_5363046,_5372278),makeShare(_5363064,_5372298),hnf('Prelude.cond'(letrec4PAKCS(_5372218,'Normalize.flattenAddN'(_5367616)),'Prelude.cond'(letrec4PAKCS(_5372238,'Prelude.map'(partcall(1,'Normalize.collectTerms',[]),_5372218)),'Prelude.cond'(letrec4PAKCS(_5372258,'Prelude.apply'('Normalize.groupTermsAL',_5372238)),'Prelude.cond'(letrec4PAKCS(_5372278,'Prelude.apply'('Prelude.apply'('Data.List.sortBy',partcall(2,'Normalize.collectTerms._\'23lambda20',[])),_5372258)),'Prelude.cond'(letrec4PAKCS(_5372298,'Prelude.apply'('Prelude.concatMap'(partcall(1,'Normalize.collectTerms._\'23lambda21',[])),_5372278)),'Normalize.buildAddN'(_5372298)))))),_5371606,_5371612,_5371618).
'blocked_blocked_Normalize.collectTerms_1'('RadExpr.Neg'(_5365830),_5367616,'RadExpr.Neg'('Normalize.collectTerms'(_5365830)),_5380042,_5380042).
'blocked_blocked_Normalize.collectTerms_1'('RadExpr.Mul'(_5366108,_5366126),_5367616,'RadExpr.Mul'('Normalize.collectTerms'(_5366108),'Normalize.collectTerms'(_5366126)),_5381248,_5381248).
'blocked_blocked_Normalize.collectTerms_1'('RadExpr.Inv'(_5366564),_5367616,'RadExpr.Inv'('Normalize.collectTerms'(_5366564)),_5382858,_5382858).
'blocked_blocked_Normalize.collectTerms_1'('RadExpr.Root'(_5366842,_5366860),_5367616,'RadExpr.Root'(_5366842,'Normalize.collectTerms'(_5366860)),_5384076,_5384076).
'blocked_blocked_Normalize.collectTerms_1'('RadExpr.Pow'(_5367214,_5367232),_5367616,'RadExpr.Pow'('Normalize.collectTerms'(_5367214),_5367232),_5385458,_5385458).
'blocked_blocked_Normalize.collectTerms_1'('RadExpr.Lit'(_5367586),_5367616,_5386820,_5386826,_5386832):-!,hnf(_5367616,_5386820,_5386826,_5386832).
'blocked_blocked_Normalize.collectTerms_1'('FAIL'(_5387340),_5367616,'FAIL'(_5387340),_5387354,_5387354).

'Normalize.collectTerms._\'23lambda20'(_5388824,_5388826,_5388828,_5388830,_5388832):-freeze(_5388830,'blocked_Normalize.collectTerms._\'23lambda20'(_5388824,_5388826,_5388828,_5388830,_5388832)).
'blocked_Normalize.collectTerms._\'23lambda20'(_5388920,_5388938,_5389952,_5389958,_5389964):-hnf(_5388920,_5391598,_5389958,_5391616),'blocked_Normalize.collectTerms._\'23lambda20_1'(_5391598,_5388938,_5389952,_5391616,_5389964).

'blocked_Normalize.collectTerms._\'23lambda20_1'(_5392076,_5392078,_5392080,_5392082,_5392084):-freeze(_5392082,'blocked_blocked_Normalize.collectTerms._\'23lambda20_1'(_5392076,_5392078,_5392080,_5392082,_5392084)).
'blocked_blocked_Normalize.collectTerms._\'23lambda20_1'('Prelude.(,)'(_5389054,_5389072),_5388938,_5392820,_5392826,_5392832):-!,hnf(_5388938,_5395186,_5392826,_5395210),'blocked_blocked_Normalize.collectTerms._\'23lambda20_1_Prelude.(,)_3'(_5395186,_5389054,_5389072,_5392820,_5395210,_5392832).

'blocked_blocked_Normalize.collectTerms._\'23lambda20_1_Prelude.(,)_3'(_5395828,_5395830,_5395832,_5395834,_5395836,_5395838):-freeze(_5395836,'blocked_blocked_blocked_Normalize.collectTerms._\'23lambda20_1_Prelude.(,)_3'(_5395828,_5395830,_5395832,_5395834,_5395836,_5395838)).
'blocked_blocked_blocked_Normalize.collectTerms._\'23lambda20_1_Prelude.(,)_3'('Prelude.(,)'(_5389200,_5389218),_5389054,_5389072,_5396218,_5396224,_5396230):-!,hnf('Prelude.apply'('Prelude.apply'('RadExpr._impl\'23\'3C\'3D\'23Prelude.Ord\'23RadExpr.RadExpr\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',[])),_5389054),_5389200),_5396218,_5396224,_5396230).
'blocked_blocked_blocked_Normalize.collectTerms._\'23lambda20_1_Prelude.(,)_3'('FAIL'(_5398488),_5389054,_5389072,'FAIL'(_5398488),_5398502,_5398502):-nonvar(_5398488).
'blocked_blocked_Normalize.collectTerms._\'23lambda20_1'('FAIL'(_5398574),_5388938,'FAIL'(_5398574),_5398588,_5398588):-nonvar(_5398574).

'Normalize.collectTerms._\'23lambda21'(_5400062,_5400064,_5400066,_5400068):-freeze(_5400066,'blocked_Normalize.collectTerms._\'23lambda21'(_5400062,_5400064,_5400066,_5400068)).
'blocked_Normalize.collectTerms._\'23lambda21'(_5400148,_5401298,_5401304,_5401310):-hnf(_5400148,_5402936,_5401304,_5402948),'blocked_Normalize.collectTerms._\'23lambda21_1'(_5402936,_5401298,_5402948,_5401310).

'blocked_Normalize.collectTerms._\'23lambda21_1'(_5403400,_5403402,_5403404,_5403406):-freeze(_5403404,'blocked_blocked_Normalize.collectTerms._\'23lambda21_1'(_5403400,_5403402,_5403404,_5403406)).
'blocked_blocked_Normalize.collectTerms._\'23lambda21_1'('Prelude.(,)'(_5400264,_5400282),_5405176,_5405182,_5405188):-!,makeShare(_5400282,_5403864),hnf('Rational.ratEq'(_5403864,'Normalize.rZero'),_5407636,_5405182,_5407602),'blocked_blocked_Normalize.collectTerms._\'23lambda21_1_Prelude.(,)_ComplexCase'(_5407636,_5400264,_5403864,_5405176,_5407602,_5405188).

'blocked_blocked_Normalize.collectTerms._\'23lambda21_1_Prelude.(,)_ComplexCase'(_5408298,_5408300,_5408302,_5408304,_5408306,_5408308):-freeze(_5408306,freeze(_5408298,'blocked_blocked_blocked_Normalize.collectTerms._\'23lambda21_1_Prelude.(,)_ComplexCase'(_5408298,_5408300,_5408302,_5408304,_5408306,_5408308))).
'blocked_blocked_blocked_Normalize.collectTerms._\'23lambda21_1_Prelude.(,)_ComplexCase'('Prelude.True',_5400264,_5403864,[],_5408708,_5408708).
'blocked_blocked_blocked_Normalize.collectTerms._\'23lambda21_1_Prelude.(,)_ComplexCase'('Prelude.False',_5400264,_5403864,['Normalize.applyCoeffAdd'(_5403864,_5400264)],_5409792,_5409792):-!.
'blocked_blocked_blocked_Normalize.collectTerms._\'23lambda21_1_Prelude.(,)_ComplexCase'('FAIL'(_5411126),_5400264,_5403864,'FAIL'(_5411126),_5411140,_5411140).
'blocked_blocked_Normalize.collectTerms._\'23lambda21_1'('FAIL'(_5411208),'FAIL'(_5411208),_5411222,_5411222):-nonvar(_5411208).

'Normalize.flattenAddN'(_5412204,_5412206,_5412208,_5412210):-freeze(_5412208,'blocked_Normalize.flattenAddN'(_5412204,_5412206,_5412208,_5412210)).
'blocked_Normalize.flattenAddN'(_5412290,_5412970,_5412976,_5412982):-makeShare(_5412318,_5413116),makeShare(_5412290,_5413136),hnf('Prelude.cond'(letrec4PAKCS(_5413116,[_5413136]),'Normalize.flattenAddN._\'23caseor0'(_5413136,_5413116)),_5412970,_5412976,_5412982).

'Normalize.splitCoeff'(_5415970,_5415972,_5415974,_5415976):-freeze(_5415974,'blocked_Normalize.splitCoeff'(_5415970,_5415972,_5415974,_5415976)).
'blocked_Normalize.splitCoeff'(_5416056,_5416806,_5416812,_5416818):-makeShare(_5416084,_5416978),makeShare(_5416056,_5416998),hnf('Prelude.cond'(letrec4PAKCS(_5416978,'Prelude.(,)'('Normalize.rOne',_5416998)),'Normalize.splitCoeff._\'23caseor0'(_5416998,_5416998,_5416978)),_5416806,_5416812,_5416818).

'Normalize.splitCoeff._\'23selFP17\'23c'(_5420564,_5420566,_5420568,_5420570):-freeze(_5420568,'blocked_Normalize.splitCoeff._\'23selFP17\'23c'(_5420564,_5420566,_5420568,_5420570)).
'blocked_Normalize.splitCoeff._\'23selFP17\'23c'(_5420650,_5421112,_5421118,_5421124):-hnf(_5420650,_5422786,_5421118,_5422798),'blocked_Normalize.splitCoeff._\'23selFP17\'23c_1'(_5422786,_5421112,_5422798,_5421124).

'blocked_Normalize.splitCoeff._\'23selFP17\'23c_1'(_5423256,_5423258,_5423260,_5423262):-freeze(_5423260,'blocked_blocked_Normalize.splitCoeff._\'23selFP17\'23c_1'(_5423256,_5423258,_5423260,_5423262)).
'blocked_blocked_Normalize.splitCoeff._\'23selFP17\'23c_1'('Prelude.(,)'(_5420766,_5420784),_5423626,_5423632,_5423638):-!,hnf(_5420766,_5423626,_5423632,_5423638).
'blocked_blocked_Normalize.splitCoeff._\'23selFP17\'23c_1'('FAIL'(_5424222),'FAIL'(_5424222),_5424236,_5424236):-nonvar(_5424222).

'Normalize.splitCoeff._\'23selFP18\'23b'(_5425706,_5425708,_5425710,_5425712):-freeze(_5425710,'blocked_Normalize.splitCoeff._\'23selFP18\'23b'(_5425706,_5425708,_5425710,_5425712)).
'blocked_Normalize.splitCoeff._\'23selFP18\'23b'(_5425792,_5426254,_5426260,_5426266):-hnf(_5425792,_5427928,_5426260,_5427940),'blocked_Normalize.splitCoeff._\'23selFP18\'23b_1'(_5427928,_5426254,_5427940,_5426266).

'blocked_Normalize.splitCoeff._\'23selFP18\'23b_1'(_5428398,_5428400,_5428402,_5428404):-freeze(_5428402,'blocked_blocked_Normalize.splitCoeff._\'23selFP18\'23b_1'(_5428398,_5428400,_5428402,_5428404)).
'blocked_blocked_Normalize.splitCoeff._\'23selFP18\'23b_1'('Prelude.(,)'(_5425908,_5425926),_5428768,_5428774,_5428780):-!,hnf(_5425926,_5428768,_5428774,_5428780).
'blocked_blocked_Normalize.splitCoeff._\'23selFP18\'23b_1'('FAIL'(_5429364),'FAIL'(_5429364),_5429378,_5429378):-nonvar(_5429364).

'Normalize.groupTermsAL'(_5430398,_5430400,_5430402):-freeze(_5430400,'blocked_Normalize.groupTermsAL'(_5430398,_5430400,_5430402)).
'blocked_Normalize.groupTermsAL'(_5430654,_5430660,_5430666):-hnf(partcall(1,'Prelude.foldl',[[],partcall(2,'Normalize.insertTerm',[])]),_5430654,_5430660,_5430666).

'Normalize.insertTerm'(_5432278,_5432280,_5432282,_5432284,_5432286):-freeze(_5432284,'blocked_Normalize.insertTerm'(_5432278,_5432280,_5432282,_5432284,_5432286)).
'blocked_Normalize.insertTerm'(_5432374,_5432392,_5433890,_5433896,_5433902):-makeShare(_5432420,_5434280),makeShare(_5432438,_5434300),makeShare(_5432456,_5434320),hnf('Prelude.cond'(letrec4PAKCS(_5434280,'Normalize.splitCoeff'(_5432392)),'Prelude.cond'(letrec4PAKCS(_5434300,'Normalize.insertTerm._\'23selFP20\'23c'(_5434280)),'Prelude.cond'(letrec4PAKCS(_5434320,'Normalize.insertTerm._\'23selFP21\'23base'(_5434280)),'Normalize.insertWithAdd'(_5434320,_5434300,_5432374)))),_5433890,_5433896,_5433902).

'Normalize.insertTerm._\'23selFP20\'23c'(_5439826,_5439828,_5439830,_5439832):-freeze(_5439830,'blocked_Normalize.insertTerm._\'23selFP20\'23c'(_5439826,_5439828,_5439830,_5439832)).
'blocked_Normalize.insertTerm._\'23selFP20\'23c'(_5439912,_5440374,_5440380,_5440386):-hnf(_5439912,_5442048,_5440380,_5442060),'blocked_Normalize.insertTerm._\'23selFP20\'23c_1'(_5442048,_5440374,_5442060,_5440386).

'blocked_Normalize.insertTerm._\'23selFP20\'23c_1'(_5442518,_5442520,_5442522,_5442524):-freeze(_5442522,'blocked_blocked_Normalize.insertTerm._\'23selFP20\'23c_1'(_5442518,_5442520,_5442522,_5442524)).
'blocked_blocked_Normalize.insertTerm._\'23selFP20\'23c_1'('Prelude.(,)'(_5440028,_5440046),_5442888,_5442894,_5442900):-!,hnf(_5440028,_5442888,_5442894,_5442900).
'blocked_blocked_Normalize.insertTerm._\'23selFP20\'23c_1'('FAIL'(_5443484),'FAIL'(_5443484),_5443498,_5443498):-nonvar(_5443484).

'Normalize.insertTerm._\'23selFP21\'23base'(_5445068,_5445070,_5445072,_5445074):-freeze(_5445072,'blocked_Normalize.insertTerm._\'23selFP21\'23base'(_5445068,_5445070,_5445072,_5445074)).
'blocked_Normalize.insertTerm._\'23selFP21\'23base'(_5445154,_5445634,_5445640,_5445646):-hnf(_5445154,_5447416,_5445640,_5447428),'blocked_Normalize.insertTerm._\'23selFP21\'23base_1'(_5447416,_5445634,_5447428,_5445646).

'blocked_Normalize.insertTerm._\'23selFP21\'23base_1'(_5447904,_5447906,_5447908,_5447910):-freeze(_5447908,'blocked_blocked_Normalize.insertTerm._\'23selFP21\'23base_1'(_5447904,_5447906,_5447908,_5447910)).
'blocked_blocked_Normalize.insertTerm._\'23selFP21\'23base_1'('Prelude.(,)'(_5445270,_5445288),_5448274,_5448280,_5448286):-!,hnf(_5445288,_5448274,_5448280,_5448286).
'blocked_blocked_Normalize.insertTerm._\'23selFP21\'23base_1'('FAIL'(_5448888),'FAIL'(_5448888),_5448902,_5448902):-nonvar(_5448888).

'Normalize.insertWithAdd'(_5449960,_5449962,_5449964,_5449966,_5449968,_5449970):-freeze(_5449968,'blocked_Normalize.insertWithAdd'(_5449960,_5449962,_5449964,_5449966,_5449968,_5449970)).
'blocked_Normalize.insertWithAdd'(_5450066,_5450084,_5450102,_5452484,_5452490,_5452496):-hnf(_5450102,_5453706,_5452490,_5453730),'blocked_Normalize.insertWithAdd_3'(_5453706,_5450066,_5450084,_5452484,_5453730,_5452496).

'blocked_Normalize.insertWithAdd_3'(_5454126,_5454128,_5454130,_5454132,_5454134,_5454136):-freeze(_5454134,'blocked_blocked_Normalize.insertWithAdd_3'(_5454126,_5454128,_5454130,_5454132,_5454134,_5454136)).
'blocked_blocked_Normalize.insertWithAdd_3'([],_5450066,_5450084,['Prelude.(,)'(_5450066,_5450084)],_5454394,_5454394).
'blocked_blocked_Normalize.insertWithAdd_3'([_5450626|_5450644],_5450066,_5450084,_5455832,_5455838,_5455844):-!,hnf(_5450626,_5457534,_5455838,_5457564),'blocked_blocked_Normalize.insertWithAdd_3_[|]_1'(_5457534,_5450644,_5450066,_5450084,_5455832,_5457564,_5455844).

'blocked_blocked_Normalize.insertWithAdd_3_[|]_1'(_5458070,_5458072,_5458074,_5458076,_5458078,_5458080,_5458082):-freeze(_5458080,'blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1'(_5458070,_5458072,_5458074,_5458076,_5458078,_5458080,_5458082)).
'blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1'('Prelude.(,)'(_5450772,_5450790),_5450644,_5450066,_5450084,_5461366,_5461372,_5461378):-!,makeShare(_5450066,_5458926),makeShare(_5450772,_5458946),hnf('RadExpr._impl\'23\'3D\'3D\'23Prelude.Eq\'23RadExpr.RadExpr\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23',[]),_5458926,_5458946),_5463940,_5461372,_5463888),'blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1_Prelude.(,)_ComplexCase'(_5463940,_5458946,_5450790,_5450644,_5458926,_5450084,_5461366,_5463888,_5461378).

'blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1_Prelude.(,)_ComplexCase'(_5464632,_5464634,_5464636,_5464638,_5464640,_5464642,_5464644,_5464646,_5464648):-freeze(_5464646,freeze(_5464632,'blocked_blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1_Prelude.(,)_ComplexCase'(_5464632,_5464634,_5464636,_5464638,_5464640,_5464642,_5464644,_5464646,_5464648))).
'blocked_blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1_Prelude.(,)_ComplexCase'('Prelude.True',_5458946,_5450790,_5450644,_5458926,_5450084,['Prelude.(,)'(_5458946,'Rational.ratAdd'(_5450790,_5450084))|_5450644],_5465072,_5465072).
'blocked_blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1_Prelude.(,)_ComplexCase'('Prelude.False',_5458946,_5450790,_5450644,_5458926,_5450084,['Prelude.(,)'(_5458946,_5450790)|'Normalize.insertWithAdd'(_5458926,_5450084,_5450644)],_5467198,_5467198):-!.
'blocked_blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1_Prelude.(,)_ComplexCase'('FAIL'(_5469208),_5458946,_5450790,_5450644,_5458926,_5450084,'FAIL'(_5469208),_5469222,_5469222).
'blocked_blocked_blocked_Normalize.insertWithAdd_3_[|]_1'('FAIL'(_5469314),_5450644,_5450066,_5450084,'FAIL'(_5469314),_5469328,_5469328):-nonvar(_5469314).
'blocked_blocked_Normalize.insertWithAdd_3'('FAIL'(_5469408),_5450066,_5450084,'FAIL'(_5469408),_5469422,_5469422):-nonvar(_5469408).

'Normalize.applyCoeffAdd'(_5470496,_5470498,_5470500,_5470502,_5470504):-freeze(_5470502,'blocked_Normalize.applyCoeffAdd'(_5470496,_5470498,_5470500,_5470502,_5470504)).
'blocked_Normalize.applyCoeffAdd'(_5470592,_5470610,_5476322,_5476328,_5476334):-makeShare(_5470592,_5473554),makeShare(_5470610,_5473574),hnf('Prelude.&&'('Rational.ratEq'(_5473554,'Normalize.rOne'),'Normalize.isLitOne'(_5473574)),_5477638,_5476328,_5477604),'blocked_Normalize.applyCoeffAdd_ComplexCase'(_5477638,_5473554,_5473574,_5476322,_5477604,_5476334).

'blocked_Normalize.applyCoeffAdd_ComplexCase'(_5478090,_5478092,_5478094,_5478096,_5478098,_5478100):-freeze(_5478098,freeze(_5478090,'blocked_blocked_Normalize.applyCoeffAdd_ComplexCase'(_5478090,_5478092,_5478094,_5478096,_5478098,_5478100))).
'blocked_blocked_Normalize.applyCoeffAdd_ComplexCase'('Prelude.True',_5473554,_5473574,'RadExpr.Lit'('Normalize.rOne'),_5478500,_5478500).
'blocked_blocked_Normalize.applyCoeffAdd_ComplexCase'('Prelude.False',_5473554,_5473574,_5481630,_5481636,_5481642):-!,makeShare(_5473574,_5479862),hnf('Normalize.isLitOne'(_5479862),_5484094,_5481636,_5484060),'blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase'(_5484094,_5473554,_5479862,_5481630,_5484060,_5481642).

'blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase'(_5484756,_5484758,_5484760,_5484762,_5484764,_5484766):-freeze(_5484764,freeze(_5484756,'blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase'(_5484756,_5484758,_5484760,_5484762,_5484764,_5484766))).
'blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5473554,_5479862,'RadExpr.Lit'(_5473554),_5485166,_5485166).
'blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5473554,_5479862,_5488512,_5488518,_5488524):-!,makeShare(_5473554,_5486586),hnf('Rational.ratEq'(_5486586,'Normalize.rOne'),_5492200,_5488518,_5492166),'blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5492200,_5486586,_5479862,_5488512,_5492166,_5488524).

'blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5493066,_5493068,_5493070,_5493072,_5493074,_5493076):-freeze(_5493074,freeze(_5493066,'blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5493066,_5493068,_5493070,_5493072,_5493074,_5493076))).
'blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5486586,_5479862,_5493470,_5493476,_5493482):-hnf(_5479862,_5493470,_5493476,_5493482).
'blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5486586,_5479862,_5496846,_5496852,_5496858):-!,makeShare(_5486586,_5494896),hnf('Rational.ratEq'(_5494896,'Normalize.rNegOne'),_5501758,_5496852,_5501724),'blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5501758,_5494896,_5479862,_5496846,_5501724,_5496858).

'blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5502828,_5502830,_5502832,_5502834,_5502836,_5502838):-freeze(_5502836,freeze(_5502828,'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5502828,_5502830,_5502832,_5502834,_5502836,_5502838))).
'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5494896,_5479862,'RadExpr.Neg'(_5479862),_5503238,_5503238).
'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5494896,_5479862,_5506436,_5506442,_5506448):-!,hnf('Prelude.otherwise',_5512572,_5506442,_5512538),'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5512572,_5494896,_5479862,_5506436,_5512538,_5506448).

'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5513834,_5513836,_5513838,_5513840,_5513842,_5513844):-freeze(_5513842,freeze(_5513834,'blocked_blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5513834,_5513836,_5513838,_5513840,_5513842,_5513844))).
'blocked_blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5494896,_5479862,'RadExpr.Mul'('RadExpr.Lit'(_5494896),_5479862),_5514244,_5514244).
'blocked_blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5494896,_5479862,_5516378,_5516384,_5516390):-!,hnf(reportFailure4PAKCS('Normalize.applyCoeffAdd',['Prelude.False']),_5516378,_5516384,_5516390).
'blocked_blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5518564),_5494896,_5479862,'FAIL'(_5518564),_5518578,_5518578).
'blocked_blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5518646),_5494896,_5479862,'FAIL'(_5518646),_5518660,_5518660).
'blocked_blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5518728),_5486586,_5479862,'FAIL'(_5518728),_5518742,_5518742).
'blocked_blocked_blocked_Normalize.applyCoeffAdd_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5518810),_5473554,_5479862,'FAIL'(_5518810),_5518824,_5518824).
'blocked_blocked_Normalize.applyCoeffAdd_ComplexCase'('FAIL'(_5518892),_5473554,_5473574,'FAIL'(_5518892),_5518906,_5518906).

'Normalize.isLitOne'(_5519786,_5519788,_5519790,_5519792):-freeze(_5519790,'blocked_Normalize.isLitOne'(_5519786,_5519788,_5519790,_5519792)).
'blocked_Normalize.isLitOne'(_5519872,_5520398,_5520404,_5520410):-makeShare(_5519900,_5520516),hnf('Prelude.cond'(letrec4PAKCS(_5520516,'Prelude.False'),'Normalize.isLitOne._\'23caseor0'(_5519872,_5520516)),_5520398,_5520404,_5520410).

'Normalize.buildAddN'(_5522954,_5522956,_5522958,_5522960):-freeze(_5522958,'blocked_Normalize.buildAddN'(_5522954,_5522956,_5522958,_5522960)).
'blocked_Normalize.buildAddN'(_5523040,_5524060,_5524066,_5524072):-hnf(_5523040,_5525122,_5524066,_5525134),'blocked_Normalize.buildAddN_1'(_5525122,_5524060,_5525134,_5524072).

'blocked_Normalize.buildAddN_1'(_5525496,_5525498,_5525500,_5525502):-freeze(_5525500,freeze(_5525496,'blocked_blocked_Normalize.buildAddN_1'(_5525496,_5525498,_5525500,_5525502))).
'blocked_blocked_Normalize.buildAddN_1'([],'RadExpr.Lit'('Normalize.rZero'),_5525750,_5525750).
'blocked_blocked_Normalize.buildAddN_1'([_5523340|_5523358],_5527368,_5527374,_5527380):-!,makeShare(_5523358,_5526552),hnf(_5526552,_5528910,_5527374,_5528934),'blocked_blocked_Normalize.buildAddN_1_[|]_2'(_5528910,_5523340,_5528910,_5527368,_5528934,_5527380).

'blocked_blocked_Normalize.buildAddN_1_[|]_2'(_5529426,_5529428,_5529430,_5529432,_5529434,_5529436):-freeze(_5529434,freeze(_5529426,'blocked_blocked_blocked_Normalize.buildAddN_1_[|]_2'(_5529426,_5529428,_5529430,_5529432,_5529434,_5529436))).
'blocked_blocked_blocked_Normalize.buildAddN_1_[|]_2'([],_5523340,_5526552,_5529694,_5529700,_5529706):-hnf(_5523340,_5529694,_5529700,_5529706).
'blocked_blocked_blocked_Normalize.buildAddN_1_[|]_2'([_5523572|_5523590],_5523340,_5526552,_5530460,_5530466,_5530472):-!,hnf('Prelude.foldl'('RadExpr.Add',_5523340,_5526552),_5530460,_5530466,_5530472).
'blocked_blocked_blocked_Normalize.buildAddN_1_[|]_2'('FAIL'(_5531652),_5523340,_5526552,'FAIL'(_5531652),_5531666,_5531666).
'blocked_blocked_Normalize.buildAddN_1'('FAIL'(_5531734),'FAIL'(_5531734),_5531748,_5531748).

'Normalize.sortCommutative'(_5532878,_5532880,_5532882,_5532884):-freeze(_5532882,'blocked_Normalize.sortCommutative'(_5532878,_5532880,_5532882,_5532884)).
'blocked_Normalize.sortCommutative'(_5532964,_5539282,_5539288,_5539294):-makeShare(_5532964,_5537370),hnf(_5537370,_5540560,_5539288,_5540578),'blocked_Normalize.sortCommutative_1'(_5540560,_5540560,_5539282,_5540578,_5539294).

'blocked_Normalize.sortCommutative_1'(_5540996,_5540998,_5541000,_5541002,_5541004):-freeze(_5541002,freeze(_5540996,'blocked_blocked_Normalize.sortCommutative_1'(_5540996,_5540998,_5541000,_5541002,_5541004))).
'blocked_blocked_Normalize.sortCommutative_1'('RadExpr.Add'(_5533080,_5533098),_5537370,_5541396,_5541402,_5541408):-makeShare(_5533138,_5541614),makeShare(_5533156,_5541634),hnf('Prelude.cond'(letrec4PAKCS(_5541614,'Normalize.flattenAddS'(_5537370)),'Prelude.cond'(letrec4PAKCS(_5541634,'Prelude.apply'('Data.List.sort'(partcall(1,'RadExpr._inst\'23Prelude.Ord\'23RadExpr.RadExpr\'230\'23\'23',[partcall(1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',[])])),'Prelude.map'(partcall(1,'Normalize.sortCommutative',[]),_5541614))),'Normalize.buildAddN'(_5541634))),_5541396,_5541402,_5541408).
'blocked_blocked_Normalize.sortCommutative_1'('RadExpr.Mul'(_5534560,_5534578),_5537370,_5546234,_5546240,_5546246):-makeShare(_5534618,_5546452),makeShare(_5534636,_5546472),hnf('Prelude.cond'(letrec4PAKCS(_5546452,'Normalize.flattenMulS'(_5537370)),'Prelude.cond'(letrec4PAKCS(_5546472,'Prelude.apply'('Data.List.sort'(partcall(1,'RadExpr._inst\'23Prelude.Ord\'23RadExpr.RadExpr\'230\'23\'23',[partcall(1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',[])])),'Prelude.map'(partcall(1,'Normalize.sortCommutative',[]),_5546452))),'Normalize.buildMulS'(_5546472))),_5546234,_5546240,_5546246).
'blocked_blocked_Normalize.sortCommutative_1'('RadExpr.Neg'(_5536040),_5537370,'RadExpr.Neg'('Normalize.sortCommutative'(_5536040)),_5551070,_5551070).
'blocked_blocked_Normalize.sortCommutative_1'('RadExpr.Inv'(_5536318),_5537370,'RadExpr.Inv'('Normalize.sortCommutative'(_5536318)),_5552304,_5552304).
'blocked_blocked_Normalize.sortCommutative_1'('RadExpr.Root'(_5536596,_5536614),_5537370,'RadExpr.Root'(_5536596,'Normalize.sortCommutative'(_5536614)),_5553558,_5553558).
'blocked_blocked_Normalize.sortCommutative_1'('RadExpr.Pow'(_5536968,_5536986),_5537370,'RadExpr.Pow'('Normalize.sortCommutative'(_5536968),_5536986),_5554976,_5554976).
'blocked_blocked_Normalize.sortCommutative_1'('RadExpr.Lit'(_5537340),_5537370,_5556374,_5556380,_5556386):-!,hnf(_5537370,_5556374,_5556380,_5556386).
'blocked_blocked_Normalize.sortCommutative_1'('FAIL'(_5556912),_5537370,'FAIL'(_5556912),_5556926,_5556926).

'Normalize.flattenAddS'(_5557912,_5557914,_5557916,_5557918):-freeze(_5557916,'blocked_Normalize.flattenAddS'(_5557912,_5557914,_5557916,_5557918)).
'blocked_Normalize.flattenAddS'(_5557998,_5558678,_5558684,_5558690):-makeShare(_5558026,_5558824),makeShare(_5557998,_5558844),hnf('Prelude.cond'(letrec4PAKCS(_5558824,[_5558844]),'Normalize.flattenAddS._\'23caseor0'(_5558844,_5558824)),_5558678,_5558684,_5558690).

'Normalize.flattenMulS'(_5561716,_5561718,_5561720,_5561722):-freeze(_5561720,'blocked_Normalize.flattenMulS'(_5561716,_5561718,_5561720,_5561722)).
'blocked_Normalize.flattenMulS'(_5561802,_5562482,_5562488,_5562494):-makeShare(_5561830,_5562628),makeShare(_5561802,_5562648),hnf('Prelude.cond'(letrec4PAKCS(_5562628,[_5562648]),'Normalize.flattenMulS._\'23caseor0'(_5562648,_5562628)),_5562482,_5562488,_5562494).

'Normalize.buildMulS'(_5565444,_5565446,_5565448,_5565450):-freeze(_5565448,'blocked_Normalize.buildMulS'(_5565444,_5565446,_5565448,_5565450)).
'blocked_Normalize.buildMulS'(_5565530,_5566550,_5566556,_5566562):-hnf(_5565530,_5567612,_5566556,_5567624),'blocked_Normalize.buildMulS_1'(_5567612,_5566550,_5567624,_5566562).

'blocked_Normalize.buildMulS_1'(_5567986,_5567988,_5567990,_5567992):-freeze(_5567990,freeze(_5567986,'blocked_blocked_Normalize.buildMulS_1'(_5567986,_5567988,_5567990,_5567992))).
'blocked_blocked_Normalize.buildMulS_1'([],'RadExpr.Lit'('Normalize.rOne'),_5568240,_5568240).
'blocked_blocked_Normalize.buildMulS_1'([_5565830|_5565848],_5569852,_5569858,_5569864):-!,makeShare(_5565848,_5569036),hnf(_5569036,_5571394,_5569858,_5571418),'blocked_blocked_Normalize.buildMulS_1_[|]_2'(_5571394,_5565830,_5571394,_5569852,_5571418,_5569864).

'blocked_blocked_Normalize.buildMulS_1_[|]_2'(_5571910,_5571912,_5571914,_5571916,_5571918,_5571920):-freeze(_5571918,freeze(_5571910,'blocked_blocked_blocked_Normalize.buildMulS_1_[|]_2'(_5571910,_5571912,_5571914,_5571916,_5571918,_5571920))).
'blocked_blocked_blocked_Normalize.buildMulS_1_[|]_2'([],_5565830,_5569036,_5572178,_5572184,_5572190):-hnf(_5565830,_5572178,_5572184,_5572190).
'blocked_blocked_blocked_Normalize.buildMulS_1_[|]_2'([_5566062|_5566080],_5565830,_5569036,_5572944,_5572950,_5572956):-!,hnf('Prelude.foldl'('RadExpr.Mul',_5565830,_5569036),_5572944,_5572950,_5572956).
'blocked_blocked_blocked_Normalize.buildMulS_1_[|]_2'('FAIL'(_5574136),_5565830,_5569036,'FAIL'(_5574136),_5574150,_5574150).
'blocked_blocked_Normalize.buildMulS_1'('FAIL'(_5574218),'FAIL'(_5574218),_5574232,_5574232).

'Normalize.distribute'(_5575172,_5575174,_5575176,_5575178):-freeze(_5575176,'blocked_Normalize.distribute'(_5575172,_5575174,_5575176,_5575178)).
'blocked_Normalize.distribute'(_5575258,_5580706,_5580712,_5580718):-makeShare(_5575258,_5579056),hnf(_5579056,_5581804,_5580712,_5581822),'blocked_Normalize.distribute_1'(_5581804,_5581804,_5580706,_5581822,_5580718).

'blocked_Normalize.distribute_1'(_5582210,_5582212,_5582214,_5582216,_5582218):-freeze(_5582216,freeze(_5582210,'blocked_blocked_Normalize.distribute_1'(_5582210,_5582212,_5582214,_5582216,_5582218))).
'blocked_blocked_Normalize.distribute_1'('RadExpr.Mul'(_5575374,_5575392),_5579056,_5582610,_5582616,_5582622):-makeShare(_5575432,_5583128),makeShare(_5575692,_5583148),makeShare(_5575374,_5583168),makeShare(_5575392,_5583188),hnf('Prelude.cond'(letrec4PAKCS(_5583128,'Prelude.cond'(letrec4PAKCS(_5583148,'Normalize.distribute._\'23caseor0'('Prelude.&&'('Normalize.isTinySum'(_5583168),'Prelude.not'('Normalize.isSum'(_5583188))),_5583168,_5583188)),'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5583188,_5583168,_5583148))),'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5583168,_5583188,_5583128)),_5582610,_5582616,_5582622).
'blocked_blocked_Normalize.distribute_1'('RadExpr.Neg'(_5577270),_5579056,'RadExpr.Neg'('Normalize.distribute'(_5577270)),_5588946,_5588946).
'blocked_blocked_Normalize.distribute_1'('RadExpr.Add'(_5577548,_5577566),_5579056,'RadExpr.Add'('Normalize.distribute'(_5577548),'Normalize.distribute'(_5577566)),_5590128,_5590128).
'blocked_blocked_Normalize.distribute_1'('RadExpr.Inv'(_5578004),_5579056,'RadExpr.Inv'('Normalize.distribute'(_5578004)),_5591702,_5591702).
'blocked_blocked_Normalize.distribute_1'('RadExpr.Root'(_5578282,_5578300),_5579056,'RadExpr.Root'(_5578282,'Normalize.distribute'(_5578300)),_5592896,_5592896).
'blocked_blocked_Normalize.distribute_1'('RadExpr.Pow'(_5578654,_5578672),_5579056,'RadExpr.Pow'('Normalize.distribute'(_5578654),_5578672),_5594254,_5594254).
'blocked_blocked_Normalize.distribute_1'('RadExpr.Lit'(_5579026),_5579056,_5595592,_5595598,_5595604):-!,hnf(_5579056,_5595592,_5595598,_5595604).
'blocked_blocked_Normalize.distribute_1'('FAIL'(_5596100),_5579056,'FAIL'(_5596100),_5596114,_5596114).

'Normalize.distribute._\'23lambda31'(_5597508,_5597510,_5597512,_5597514,_5597516):-freeze(_5597514,'blocked_Normalize.distribute._\'23lambda31'(_5597508,_5597510,_5597512,_5597514,_5597516)).
'blocked_Normalize.distribute._\'23lambda31'(_5597604,_5597622,'RadExpr.Mul'('RadExpr.Lit'(_5597604),_5597622),_5597882,_5597882).

'Normalize.distribute._\'23lambda33'(_5600156,_5600158,_5600160,_5600162,_5600164):-freeze(_5600162,'blocked_Normalize.distribute._\'23lambda33'(_5600156,_5600158,_5600160,_5600162,_5600164)).
'blocked_Normalize.distribute._\'23lambda33'(_5600252,_5600270,'RadExpr.Mul'(_5600270,'RadExpr.Lit'(_5600252)),_5600530,_5600530).

'Normalize.distribute._\'23lambda35'(_5602804,_5602806,_5602808,_5602810,_5602812):-freeze(_5602810,'blocked_Normalize.distribute._\'23lambda35'(_5602804,_5602806,_5602808,_5602810,_5602812)).
'blocked_Normalize.distribute._\'23lambda35'(_5602900,_5602918,_5603172,_5603178,_5603184):-hnf('Normalize.distribute'('RadExpr.Mul'(_5602918,_5602900)),_5603172,_5603178,_5603184).

'Normalize.distribute._\'23lambda36'(_5605524,_5605526,_5605528,_5605530,_5605532):-freeze(_5605530,'blocked_Normalize.distribute._\'23lambda36'(_5605524,_5605526,_5605528,_5605530,_5605532)).
'blocked_Normalize.distribute._\'23lambda36'(_5605620,_5605638,_5605892,_5605898,_5605904):-hnf('Normalize.distribute'('RadExpr.Mul'(_5605620,_5605638)),_5605892,_5605898,_5605904).

'Normalize.isSum'(_5607608,_5607610,_5607612,_5607614):-freeze(_5607612,'blocked_Normalize.isSum'(_5607608,_5607610,_5607612,_5607614)).
'blocked_Normalize.isSum'(_5607694,_5608220,_5608226,_5608232):-makeShare(_5607722,_5608338),hnf('Prelude.cond'(letrec4PAKCS(_5608338,'Prelude.False'),'Normalize.isSum._\'23caseor0'(_5607694,_5608338)),_5608220,_5608226,_5608232).

'Normalize.isTinySum'(_5610740,_5610742,_5610744,_5610746):-freeze(_5610744,'blocked_Normalize.isTinySum'(_5610740,_5610742,_5610744,_5610746)).
'blocked_Normalize.isTinySum'(_5610826,_5611402,_5611408,_5611414):-makeShare(_5610826,_5611498),hnf('Prelude.&&'('Normalize.isSum'(_5611498),'Prelude._impl\'23\'3C\'3D\'23Prelude.Ord\'23Prelude.Int\'23'('Prelude.length'('Normalize.flattenS'(_5611498)),2)),_5611402,_5611408,_5611414).

'Normalize.flattenS'(_5614252,_5614254,_5614256,_5614258):-freeze(_5614256,'blocked_Normalize.flattenS'(_5614252,_5614254,_5614256,_5614258)).
'blocked_Normalize.flattenS'(_5614338,_5615018,_5615024,_5615030):-makeShare(_5614366,_5615164),makeShare(_5614338,_5615184),hnf('Prelude.cond'(letrec4PAKCS(_5615164,[_5615184]),'Normalize.flattenS._\'23caseor0'(_5615184,_5615164)),_5615018,_5615024,_5615030).

'Normalize.rebuildAddD'(_5618020,_5618022,_5618024,_5618026):-freeze(_5618024,'blocked_Normalize.rebuildAddD'(_5618020,_5618022,_5618024,_5618026)).
'blocked_Normalize.rebuildAddD'(_5618106,_5622680,_5622686,_5622692):-hnf(_5618106,_5623814,_5622686,_5623826),'blocked_Normalize.rebuildAddD_1'(_5623814,_5622680,_5623826,_5622692).

'blocked_Normalize.rebuildAddD_1'(_5624200,_5624202,_5624204,_5624206):-freeze(_5624204,freeze(_5624200,'blocked_blocked_Normalize.rebuildAddD_1'(_5624200,_5624202,_5624204,_5624206))).
'blocked_blocked_Normalize.rebuildAddD_1'([],_5624448,_5624454,_5624460):-hnf('Prelude.error'(['^r','^e','^b','^u','^i','^l','^d','^A','^d','^d','^D',^:,'^ ','^e','^m','^p','^t','^y','^ ','^l','^i','^s','^t']),_5624448,_5624454,_5624460).
'blocked_blocked_Normalize.rebuildAddD_1'([_5621948|_5621966],_5630678,_5630684,_5630690):-!,makeShare(_5621966,_5629850),hnf(_5629850,_5632292,_5630684,_5632316),'blocked_blocked_Normalize.rebuildAddD_1_[|]_2'(_5632292,_5621948,_5632292,_5630678,_5632316,_5630690).

'blocked_blocked_Normalize.rebuildAddD_1_[|]_2'(_5632820,_5632822,_5632824,_5632826,_5632828,_5632830):-freeze(_5632828,freeze(_5632820,'blocked_blocked_blocked_Normalize.rebuildAddD_1_[|]_2'(_5632820,_5632822,_5632824,_5632826,_5632828,_5632830))).
'blocked_blocked_blocked_Normalize.rebuildAddD_1_[|]_2'([],_5621948,_5629850,_5633088,_5633094,_5633100):-hnf(_5621948,_5633088,_5633094,_5633100).
'blocked_blocked_blocked_Normalize.rebuildAddD_1_[|]_2'([_5622180|_5622198],_5621948,_5629850,_5633866,_5633872,_5633878):-!,hnf('Prelude.foldl'('RadExpr.Add',_5621948,_5629850),_5633866,_5633872,_5633878).
'blocked_blocked_blocked_Normalize.rebuildAddD_1_[|]_2'('FAIL'(_5635070),_5621948,_5629850,'FAIL'(_5635070),_5635084,_5635084).
'blocked_blocked_Normalize.rebuildAddD_1'('FAIL'(_5635152),'FAIL'(_5635152),_5635166,_5635166).

'Normalize.fixN._\'23caseor0'(_5636286,_5636288,_5636290,_5636292,_5636294,_5636296,_5636298,_5636300):-freeze(_5636298,'blocked_Normalize.fixN._\'23caseor0'(_5636286,_5636288,_5636290,_5636292,_5636294,_5636296,_5636298,_5636300)).
'blocked_Normalize.fixN._\'23caseor0'(_5636412,_5636430,_5636448,_5636466,_5636484,_5637302,_5637308,_5637314):-hnf(_5636412,_5638648,_5637308,_5638684),'blocked_Normalize.fixN._\'23caseor0_1'(_5638648,_5636430,_5636448,_5636466,_5636484,_5637302,_5638684,_5637314).

'blocked_Normalize.fixN._\'23caseor0_1'(_5639120,_5639122,_5639124,_5639126,_5639128,_5639130,_5639132,_5639134):-freeze(_5639132,freeze(_5639120,'blocked_blocked_Normalize.fixN._\'23caseor0_1'(_5639120,_5639122,_5639124,_5639126,_5639128,_5639130,_5639132,_5639134))).
'blocked_blocked_Normalize.fixN._\'23caseor0_1'('Prelude.True',_5636430,_5636448,_5636466,_5636484,_5639544,_5639550,_5639556):-hnf(_5636430,_5639544,_5639550,_5639556).
'blocked_blocked_Normalize.fixN._\'23caseor0_1'('Prelude.False',_5636430,_5636448,_5636466,_5636484,_5640536,_5640542,_5640548):-!,hnf('Normalize.fixN'('Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_5636448,1),_5636466,_5636484),_5640536,_5640542,_5640548).
'blocked_blocked_Normalize.fixN._\'23caseor0_1'('FAIL'(_5642144),_5636430,_5636448,_5636466,_5636484,'FAIL'(_5642144),_5642158,_5642158).

'Normalize.flattenArith._\'23caseor0'(_5643614,_5643616,_5643618,_5643620,_5643622):-freeze(_5643620,'blocked_Normalize.flattenArith._\'23caseor0'(_5643614,_5643616,_5643618,_5643620,_5643622)).
'blocked_Normalize.flattenArith._\'23caseor0'(_5643710,_5643728,_5644994,_5645000,_5645006):-hnf(_5643710,_5646604,_5645000,_5646622),'blocked_Normalize.flattenArith._\'23caseor0_1'(_5646604,_5643728,_5644994,_5646622,_5645006).

'blocked_Normalize.flattenArith._\'23caseor0_1'(_5647082,_5647084,_5647086,_5647088,_5647090):-freeze(_5647088,freeze(_5647082,'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'(_5647082,_5647084,_5647086,_5647088,_5647090))).
'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'('RadExpr.Neg'(_5643844),_5643728,_5647474,_5647480,_5647486):-hnf('Normalize.flattenArith'(_5643844),_5647474,_5647480,_5647486).
'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'('RadExpr.Lit'(_5644038),_5643728,_5648586,_5648592,_5648598):-hnf(_5643728,_5648586,_5648592,_5648598).
'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'('RadExpr.Add'(_5644148,_5644166),_5643728,_5649464,_5649470,_5649476):-hnf(_5643728,_5649464,_5649470,_5649476).
'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'('RadExpr.Mul'(_5644282,_5644300),_5643728,_5650408,_5650414,_5650420):-hnf(_5643728,_5650408,_5650414,_5650420).
'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'('RadExpr.Inv'(_5644416),_5643728,_5651344,_5651350,_5651356):-hnf(_5643728,_5651344,_5651350,_5651356).
'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'('RadExpr.Root'(_5644526,_5644544),_5643728,_5652234,_5652240,_5652246):-hnf(_5643728,_5652234,_5652240,_5652246).
'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'('RadExpr.Pow'(_5644660,_5644678),_5643728,_5653178,_5653184,_5653190):-!,hnf(_5643728,_5653178,_5653184,_5653190).
'blocked_blocked_Normalize.flattenArith._\'23caseor0_1'('FAIL'(_5653836),_5643728,'FAIL'(_5653836),_5653850,_5653850).

'Normalize.flattenArith._\'23caseor0._\'23caseor0'(_5655690,_5655692,_5655694,_5655696,_5655698):-freeze(_5655696,'blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0'(_5655690,_5655692,_5655694,_5655696,_5655698)).
'blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0'(_5655786,_5655804,_5657142,_5657148,_5657154):-hnf(_5655786,_5659184,_5657148,_5659202),'blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'(_5659184,_5655804,_5657142,_5659202,_5657154).

'blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'(_5659734,_5659736,_5659738,_5659740,_5659742):-freeze(_5659740,freeze(_5659734,'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'(_5659734,_5659736,_5659738,_5659740,_5659742))).
'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5655920),_5655804,_5660126,_5660132,_5660138):-hnf('Normalize.flattenArith'(_5655920),_5660126,_5660132,_5660138).
'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5656114),_5655804,_5661310,_5661316,_5661322):-hnf(_5655804,_5661310,_5661316,_5661322).
'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5656224),_5655804,_5662252,_5662258,_5662264):-hnf(_5655804,_5662252,_5662258,_5662264).
'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5656334,_5656352),_5655804,_5663202,_5663208,_5663214):-hnf(_5655804,_5663202,_5663208,_5663214).
'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5656468,_5656486),_5655804,_5664218,_5664224,_5664230):-hnf(_5655804,_5664218,_5664224,_5664230).
'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5656602,_5656620),_5655804,_5665246,_5665252,_5665258):-hnf(_5655804,_5665246,_5665252,_5665258).
'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5656736,_5656754),_5655804,_5666262,_5666268,_5666274):-!,hnf(_5655804,_5666262,_5666268,_5666274).
'blocked_blocked_Normalize.flattenArith._\'23caseor0._\'23caseor0_1'('FAIL'(_5666992),_5655804,'FAIL'(_5666992),_5667006,_5667006).

'Normalize.foldConstants._\'23caseor0'(_5668476,_5668478,_5668480,_5668482,_5668484):-freeze(_5668482,'blocked_Normalize.foldConstants._\'23caseor0'(_5668476,_5668478,_5668480,_5668482,_5668484)).
'blocked_Normalize.foldConstants._\'23caseor0'(_5668572,_5668590,_5669946,_5669952,_5669958):-hnf(_5668572,_5671592,_5669952,_5671610),'blocked_Normalize.foldConstants._\'23caseor0_1'(_5671592,_5668590,_5669946,_5671610,_5669958).

'blocked_Normalize.foldConstants._\'23caseor0_1'(_5672076,_5672078,_5672080,_5672082,_5672084):-freeze(_5672082,freeze(_5672076,'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'(_5672076,_5672078,_5672080,_5672082,_5672084))).
'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'('RadExpr.Lit'(_5668706),_5668590,'RadExpr.Lit'('Rational.ratNeg'(_5668706)),_5672474,_5672474).
'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'('RadExpr.Neg'(_5668984),_5668590,_5673702,_5673708,_5673714):-hnf(_5668984,_5673702,_5673708,_5673714).
'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'('RadExpr.Add'(_5669094,_5669112),_5668590,_5674586,_5674592,_5674598):-hnf(_5668590,_5674586,_5674592,_5674598).
'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'('RadExpr.Mul'(_5669228,_5669246),_5668590,_5675536,_5675542,_5675548):-hnf(_5668590,_5675536,_5675542,_5675548).
'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'('RadExpr.Inv'(_5669362),_5668590,_5676478,_5676484,_5676490):-hnf(_5668590,_5676478,_5676484,_5676490).
'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'('RadExpr.Root'(_5669472,_5669490),_5668590,_5677374,_5677380,_5677386):-hnf(_5668590,_5677374,_5677380,_5677386).
'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'('RadExpr.Pow'(_5669606,_5669624),_5668590,_5678324,_5678330,_5678336):-!,hnf(_5668590,_5678324,_5678330,_5678336).
'blocked_blocked_Normalize.foldConstants._\'23caseor0_1'('FAIL'(_5678988),_5668590,'FAIL'(_5678988),_5679002,_5679002).

'Normalize.foldConstants._\'23caseor0._\'23caseor0'(_5680880,_5680882,_5680884,_5680886,_5680888):-freeze(_5680886,'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0'(_5680880,_5680882,_5680884,_5680886,_5680888)).
'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0'(_5680976,_5680994,_5683116,_5683122,_5683128):-hnf(_5680976,_5685194,_5683122,_5685212),'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'(_5685194,_5680994,_5683116,_5685212,_5683128).

'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'(_5685750,_5685752,_5685754,_5685756,_5685758):-freeze(_5685756,freeze(_5685750,'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'(_5685750,_5685752,_5685754,_5685756,_5685758))).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5681110),_5680994,_5687800,_5687806,_5687812):-makeShare(_5681110,_5686228),hnf('Prelude.not'('Rational.ratEq'(_5686228,'Normalize.rZero')),_5690700,_5687806,_5690666),'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5690700,_5686228,_5680994,_5687800,_5690666,_5687812).

'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5691416,_5691418,_5691420,_5691422,_5691424,_5691426):-freeze(_5691424,freeze(_5691416,'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5691416,_5691418,_5691420,_5691422,_5691424,_5691426))).
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_5686228,_5680994,'RadExpr.Lit'('Rational.ratDiv'('Normalize.rOne',_5686228)),_5691826,_5691826).
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_5686228,_5680994,'RadExpr.Inv'('RadExpr.Lit'(_5686228)),_5693538,_5693538):-!.
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_5694730),_5686228,_5680994,'FAIL'(_5694730),_5694744,_5694744).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5682082),_5680994,_5695108,_5695114,_5695120):-hnf(_5682082,_5695108,_5695114,_5695120).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5682192),_5680994,_5696056,_5696062,_5696068):-hnf(_5680994,_5696056,_5696062,_5696068).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5682302,_5682320),_5680994,_5697012,_5697018,_5697024):-hnf(_5680994,_5697012,_5697018,_5697024).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5682436,_5682454),_5680994,_5698034,_5698040,_5698046):-hnf(_5680994,_5698034,_5698040,_5698046).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5682570,_5682588),_5680994,_5699068,_5699074,_5699080):-hnf(_5680994,_5699068,_5699074,_5699080).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5682704,_5682722),_5680994,_5700090,_5700096,_5700102):-!,hnf(_5680994,_5700090,_5700096,_5700102).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0_1'('FAIL'(_5700826),_5680994,'FAIL'(_5700826),_5700840,_5700840).

'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0'(_5703126,_5703128,_5703130,_5703132,_5703134,_5703136):-freeze(_5703134,'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0'(_5703126,_5703128,_5703130,_5703132,_5703134,_5703136)).
'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0'(_5703232,_5703250,_5703268,_5705750,_5705756,_5705762):-hnf(_5703232,_5708268,_5705756,_5708292),'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5708268,_5703250,_5703268,_5705750,_5708292,_5705762).

'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5708910,_5708912,_5708914,_5708916,_5708918,_5708920):-freeze(_5708918,freeze(_5708910,'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5708910,_5708912,_5708914,_5708916,_5708918,_5708920))).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5703384),_5703250,_5703268,_5711168,_5711174,_5711180):-makeShare(_5703384,_5709430),hnf('Rational.ratEq'(_5709430,'Normalize.rZero'),_5714514,_5711174,_5714474),'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5714514,_5709430,_5703250,_5703268,_5711168,_5714474,_5711180).

'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5715304,_5715306,_5715308,_5715310,_5715312,_5715314,_5715316):-freeze(_5715314,freeze(_5715304,'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5715304,_5715306,_5715308,_5715310,_5715312,_5715314,_5715316))).
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_5709430,_5703250,_5703268,'RadExpr.Lit'('Normalize.rZero'),_5715724,_5715724).
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_5709430,_5703250,_5703268,_5719092,_5719098,_5719104):-!,makeShare(_5709430,_5717384),hnf('Rational.ratEq'(_5717384,'Normalize.rOne'),_5723658,_5719098,_5723618),'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_5723658,_5717384,_5703250,_5703268,_5719092,_5723618,_5719104).

'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_5724670,_5724672,_5724674,_5724676,_5724678,_5724680,_5724682):-freeze(_5724680,freeze(_5724670,'blocked_blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_5724670,_5724672,_5724674,_5724676,_5724678,_5724680,_5724682))).
'blocked_blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5717384,_5703250,_5703268,'RadExpr.Lit'('Normalize.rOne'),_5725090,_5725090).
'blocked_blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5717384,_5703250,_5703268,'RadExpr.Root'(_5703250,'RadExpr.Lit'(_5717384)),_5726860,_5726860):-!.
'blocked_blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5728512),_5717384,_5703250,_5703268,'FAIL'(_5728512),_5728526,_5728526).
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_5728602),_5709430,_5703250,_5703268,'FAIL'(_5728602),_5728616,_5728616).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5704644),_5703250,_5703268,_5728988,_5728994,_5729000):-hnf(_5703268,_5728988,_5728994,_5729000).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5704754,_5704772),_5703250,_5703268,_5730090,_5730096,_5730102):-hnf(_5703268,_5730090,_5730096,_5730102).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5704888,_5704906),_5703250,_5703268,_5731258,_5731264,_5731270):-hnf(_5703268,_5731258,_5731264,_5731270).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5705022),_5703250,_5703268,_5732418,_5732424,_5732430):-hnf(_5703268,_5732418,_5732424,_5732430).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5705132,_5705150),_5703250,_5703268,_5733532,_5733538,_5733544):-hnf(_5703268,_5733532,_5733538,_5733544).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5705266,_5705284),_5703250,_5703268,_5734700,_5734706,_5734712):-!,hnf(_5703268,_5734700,_5734706,_5734712).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_5735582),_5703250,_5703268,'FAIL'(_5735582),_5735596,_5735596).

'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5738298,_5738300,_5738302,_5738304,_5738306,_5738308):-freeze(_5738306,'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5738298,_5738300,_5738302,_5738304,_5738306,_5738308)).
'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5738404,_5738422,_5738440,_5739746,_5739752,_5739758):-hnf(_5738404,_5742696,_5739752,_5742720),'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5742696,_5738422,_5738440,_5739746,_5742720,_5739758).

'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5743410,_5743412,_5743414,_5743416,_5743418,_5743420):-freeze(_5743418,freeze(_5743410,'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5743410,_5743412,_5743414,_5743416,_5743418,_5743420))).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('Prelude.True',_5738422,_5738440,'RadExpr.Lit'('Normalize.rOne'),_5743820,_5743820).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('Prelude.False',_5738422,_5738440,_5746910,_5746916,_5746922):-!,makeShare(_5738440,_5745300),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5745300,1),_5750742,_5746916,_5750708),'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_5750742,_5738422,_5745300,_5746910,_5750708,_5746922).

'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_5751632,_5751634,_5751636,_5751638,_5751640,_5751642):-freeze(_5751640,freeze(_5751632,'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_5751632,_5751634,_5751636,_5751638,_5751640,_5751642))).
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_5738422,_5745300,_5752036,_5752042,_5752048):-hnf(_5738422,_5752036,_5752042,_5752048).
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_5738422,_5745300,'RadExpr.Pow'(_5738422,_5745300),_5753360,_5753360):-!.
'blocked_blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_5754636),_5738422,_5745300,'FAIL'(_5754636),_5754650,_5754650).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_5754718),_5738422,_5738440,'FAIL'(_5754718),_5754732,_5754732).

'Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5757842,_5757844,_5757846,_5757848,_5757850,_5757852):-freeze(_5757850,'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5757842,_5757844,_5757846,_5757848,_5757850,_5757852)).
'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5757948,_5757966,_5757984,_5759698,_5759704,_5759710):-hnf(_5757948,_5763080,_5759704,_5763104),'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5763080,_5757966,_5757984,_5759698,_5763104,_5759710).

'blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5763866,_5763868,_5763870,_5763872,_5763874,_5763876):-freeze(_5763874,freeze(_5763866,'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5763866,_5763868,_5763870,_5763872,_5763874,_5763876))).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5758100),_5757966,_5757984,'RadExpr.Lit'('Rational.ratPow'(_5758100,_5757966)),_5764274,_5764274).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5758448),_5757966,_5757984,_5765968,_5765974,_5765980):-hnf(_5757984,_5765968,_5765974,_5765980).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5758558,_5758576),_5757966,_5757984,_5767214,_5767220,_5767226):-hnf(_5757984,_5767214,_5767220,_5767226).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5758692,_5758710),_5757966,_5757984,_5768526,_5768532,_5768538):-hnf(_5757984,_5768526,_5768532,_5768538).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5758826),_5757966,_5757984,_5769830,_5769836,_5769842):-hnf(_5757984,_5769830,_5769836,_5769842).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5758936,_5758954),_5757966,_5757984,_5771088,_5771094,_5771100):-hnf(_5757984,_5771088,_5771094,_5771100).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5759070,_5759088),_5757966,_5757984,_5772400,_5772406,_5772412):-!,hnf(_5757984,_5772400,_5772406,_5772412).
'blocked_blocked_Normalize.foldConstants._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_5773426),_5757966,_5757984,'FAIL'(_5773426),_5773440,_5773440).

'Normalize.foldConstantsAdd._\'23caseor0'(_5775032,_5775034,_5775036,_5775038,_5775040,_5775042,_5775044):-freeze(_5775042,'blocked_Normalize.foldConstantsAdd._\'23caseor0'(_5775032,_5775034,_5775036,_5775038,_5775040,_5775042,_5775044)).
'blocked_Normalize.foldConstantsAdd._\'23caseor0'(_5775148,_5775166,_5775184,_5775202,_5777684,_5777690,_5777696):-hnf(_5775148,_5779454,_5777690,_5779484),'blocked_Normalize.foldConstantsAdd._\'23caseor0_1'(_5779454,_5775166,_5775184,_5775202,_5777684,_5779484,_5777696).

'blocked_Normalize.foldConstantsAdd._\'23caseor0_1'(_5779984,_5779986,_5779988,_5779990,_5779992,_5779994,_5779996):-freeze(_5779994,freeze(_5779984,'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'(_5779984,_5779986,_5779988,_5779990,_5779992,_5779994,_5779996))).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'('RadExpr.Lit'(_5775318),_5775166,_5775184,_5775202,_5781738,_5781744,_5781750):-hnf('Rational.ratEq'(_5775318,'Normalize.rZero'),_5784342,_5781744,_5784296),'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5784342,_5775318,_5775166,_5775184,_5775202,_5781738,_5784296,_5781750).

'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5784996,_5784998,_5785000,_5785002,_5785004,_5785006,_5785008,_5785010):-freeze(_5785008,freeze(_5784996,'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5784996,_5784998,_5785000,_5785002,_5785004,_5785006,_5785008,_5785010))).
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_5775318,_5775166,_5775184,_5775202,_5785420,_5785426,_5785432):-hnf(_5775184,_5785420,_5785426,_5785432).
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_5775318,_5775166,_5775184,_5775202,'RadExpr.Add'(_5775184,_5775166),_5786682,_5786682):-!.
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_5787896),_5775318,_5775166,_5775184,_5775202,'FAIL'(_5787896),_5787910,_5787910).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'('RadExpr.Neg'(_5775940),_5775166,_5775184,_5775202,_5790698,_5790704,_5790710):-makeShare(_5775184,_5788416),makeShare(_5775940,_5788436),hnf('RadExpr._impl\'23\'3D\'3D\'23Prelude.Eq\'23RadExpr.RadExpr\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23',[]),_5788416,_5788436),_5793302,_5790704,_5793256),'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Neg_ComplexCase'(_5793302,_5788436,_5775166,_5788416,_5775202,_5790698,_5793256,_5790710).

'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Neg_ComplexCase'(_5793980,_5793982,_5793984,_5793986,_5793988,_5793990,_5793992,_5793994):-freeze(_5793992,freeze(_5793980,'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Neg_ComplexCase'(_5793980,_5793982,_5793984,_5793986,_5793988,_5793990,_5793992,_5793994))).
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Neg_ComplexCase'('Prelude.True',_5788436,_5775166,_5788416,_5775202,'RadExpr.Lit'('Normalize.rZero'),_5794410,_5794410).
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Neg_ComplexCase'('Prelude.False',_5788436,_5775166,_5788416,_5775202,'RadExpr.Add'(_5788416,'RadExpr.Neg'(_5788436)),_5795930,_5795930):-!.
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1_RadExpr.Neg_ComplexCase'('FAIL'(_5797320),_5788436,_5775166,_5788416,_5775202,'FAIL'(_5797320),_5797334,_5797334).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'('RadExpr.Add'(_5776814,_5776832),_5775166,_5775184,_5775202,_5797722,_5797728,_5797734):-hnf(_5775202,_5797722,_5797728,_5797734).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'('RadExpr.Mul'(_5776948,_5776966),_5775166,_5775184,_5775202,_5798838,_5798844,_5798850):-hnf(_5775202,_5798838,_5798844,_5798850).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'('RadExpr.Inv'(_5777082),_5775166,_5775184,_5775202,_5799946,_5799952,_5799958):-hnf(_5775202,_5799946,_5799952,_5799958).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'('RadExpr.Root'(_5777192,_5777210),_5775166,_5775184,_5775202,_5801008,_5801014,_5801020):-hnf(_5775202,_5801008,_5801014,_5801020).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'('RadExpr.Pow'(_5777326,_5777344),_5775166,_5775184,_5775202,_5802124,_5802130,_5802136):-!,hnf(_5775202,_5802124,_5802130,_5802136).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0_1'('FAIL'(_5802954),_5775166,_5775184,_5775202,'FAIL'(_5802954),_5802968,_5802968).

'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0'(_5805384,_5805386,_5805388,_5805390,_5805392,_5805394):-freeze(_5805392,'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0'(_5805384,_5805386,_5805388,_5805390,_5805392,_5805394)).
'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0'(_5805490,_5805508,_5805526,_5806336,_5806342,_5806348):-hnf(_5805490,_5808962,_5806342,_5808986),'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5808962,_5805508,_5805526,_5806336,_5808986,_5806348).

'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5809622,_5809624,_5809626,_5809628,_5809630,_5809632):-freeze(_5809630,freeze(_5809622,'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5809622,_5809624,_5809626,_5809628,_5809630,_5809632))).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('Prelude.True',_5805508,_5805526,_5810026,_5810032,_5810038):-hnf(_5805526,_5810026,_5810032,_5810038).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('Prelude.False',_5805508,_5805526,'RadExpr.Add'(_5805508,_5805526),_5811092,_5811092):-!.
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_5812110),_5805508,_5805526,'FAIL'(_5812110),_5812124,_5812124).

'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5814940,_5814942,_5814944,_5814946,_5814948,_5814950,_5814952,_5814954):-freeze(_5814952,'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5814940,_5814942,_5814944,_5814946,_5814948,_5814950,_5814952,_5814954)).
'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5815066,_5815084,_5815102,_5815120,_5815138,_5817310,_5817316,_5817322):-hnf(_5815066,_5820384,_5817316,_5820420),'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5820384,_5815084,_5815102,_5815120,_5815138,_5817310,_5820420,_5817322).

'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5821144,_5821146,_5821148,_5821150,_5821152,_5821154,_5821156,_5821158):-freeze(_5821156,freeze(_5821144,'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5821144,_5821146,_5821148,_5821150,_5821152,_5821154,_5821156,_5821158))).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5815254),_5815084,_5815102,_5815120,_5815138,'RadExpr.Lit'('Rational.ratAdd'(_5815084,_5815254)),_5821572,_5821572).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5815602),_5815084,_5815102,_5815120,_5815138,_5824984,_5824990,_5824996):-hnf('Rational.ratEq'(_5815084,'Normalize.rZero'),_5828898,_5824990,_5828846),'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Neg_ComplexCase'(_5828898,_5815602,_5815084,_5815102,_5815120,_5815138,_5824984,_5828846,_5824996).

'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Neg_ComplexCase'(_5829770,_5829772,_5829774,_5829776,_5829778,_5829780,_5829782,_5829784,_5829786):-freeze(_5829784,freeze(_5829770,'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Neg_ComplexCase'(_5829770,_5829772,_5829774,_5829776,_5829778,_5829780,_5829782,_5829784,_5829786))).
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Neg_ComplexCase'('Prelude.True',_5815602,_5815084,_5815102,_5815120,_5815138,_5830204,_5830210,_5830216):-hnf(_5815120,_5830204,_5830210,_5830216).
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Neg_ComplexCase'('Prelude.False',_5815602,_5815084,_5815102,_5815120,_5815138,'RadExpr.Add'(_5815102,_5815120),_5831756,_5831756):-!.
'blocked_blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Neg_ComplexCase'('FAIL'(_5833260),_5815602,_5815084,_5815102,_5815120,_5815138,'FAIL'(_5833260),_5833274,_5833274).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5816224,_5816242),_5815084,_5815102,_5815120,_5815138,_5833670,_5833676,_5833682):-hnf(_5815138,_5833670,_5833676,_5833682).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5816358,_5816376),_5815084,_5815102,_5815120,_5815138,_5835076,_5835082,_5835088):-hnf(_5815138,_5835076,_5835082,_5835088).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5816492),_5815084,_5815102,_5815120,_5815138,_5836474,_5836480,_5836486):-hnf(_5815138,_5836474,_5836480,_5836486).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5816602,_5816620),_5815084,_5815102,_5815120,_5815138,_5837826,_5837832,_5837838):-hnf(_5815138,_5837826,_5837832,_5837838).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5816736,_5816754),_5815084,_5815102,_5815120,_5815138,_5839232,_5839238,_5839244):-!,hnf(_5815138,_5839232,_5839238,_5839244).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_5840352),_5815084,_5815102,_5815120,_5815138,'FAIL'(_5840352),_5840366,_5840366).

'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0'(_5842382,_5842384,_5842386,_5842388,_5842390,_5842392,_5842394,_5842396):-freeze(_5842394,'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0'(_5842382,_5842384,_5842386,_5842388,_5842390,_5842392,_5842394,_5842396)).
'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0'(_5842508,_5842526,_5842544,_5842562,_5842580,_5844956,_5844962,_5844968):-hnf(_5842508,_5847166,_5844962,_5847202),'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'(_5847166,_5842526,_5842544,_5842562,_5842580,_5844956,_5847202,_5844968).

'blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'(_5847782,_5847784,_5847786,_5847788,_5847790,_5847792,_5847794,_5847796):-freeze(_5847794,freeze(_5847782,'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'(_5847782,_5847784,_5847786,_5847788,_5847790,_5847792,_5847794,_5847796))).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5842696),_5842526,_5842544,_5842562,_5842580,_5848204,_5848210,_5848216):-makeShare(_5842730,_5848622),makeShare(_5842696,_5848642),makeShare(_5842544,_5848662),makeShare(_5842562,_5848682),hnf('Prelude.cond'(letrec4PAKCS(_5848622,'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0'('Rational.ratEq'(_5848642,'Normalize.rZero'),_5848662,_5848682)),'Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5842526,_5848642,_5848662,_5848682,_5848622)),_5848204,_5848210,_5848216).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5843904),_5842526,_5842544,_5842562,_5842580,_5853176,_5853182,_5853188):-hnf(_5842580,_5853176,_5853182,_5853188).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5844014,_5844032),_5842526,_5842544,_5842562,_5842580,_5854372,_5854378,_5854384):-hnf(_5842580,_5854372,_5854378,_5854384).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5844148,_5844166),_5842526,_5842544,_5842562,_5842580,_5855634,_5855640,_5855646):-hnf(_5842580,_5855634,_5855640,_5855646).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5844282),_5842526,_5842544,_5842562,_5842580,_5856888,_5856894,_5856900):-hnf(_5842580,_5856888,_5856894,_5856900).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5844392,_5844410),_5842526,_5842544,_5842562,_5842580,_5858096,_5858102,_5858108):-hnf(_5842580,_5858096,_5858102,_5858108).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5844526,_5844544),_5842526,_5842544,_5842562,_5842580,_5859358,_5859364,_5859370):-!,hnf(_5842580,_5859358,_5859364,_5859370).
'blocked_blocked_Normalize.foldConstantsAdd._\'23caseor0._\'23caseor0_1'('FAIL'(_5860334),_5842526,_5842544,_5842562,_5842580,'FAIL'(_5860334),_5860348,_5860348).

'Normalize.foldConstantsMul._\'23caseor0'(_5861956,_5861958,_5861960,_5861962,_5861964,_5861966,_5861968):-freeze(_5861966,'blocked_Normalize.foldConstantsMul._\'23caseor0'(_5861956,_5861958,_5861960,_5861962,_5861964,_5861966,_5861968)).
'blocked_Normalize.foldConstantsMul._\'23caseor0'(_5862072,_5862090,_5862108,_5862126,_5864742,_5864748,_5864754):-hnf(_5862072,_5866512,_5864748,_5866542),'blocked_Normalize.foldConstantsMul._\'23caseor0_1'(_5866512,_5862090,_5862108,_5862126,_5864742,_5866542,_5864754).

'blocked_Normalize.foldConstantsMul._\'23caseor0_1'(_5867042,_5867044,_5867046,_5867048,_5867050,_5867052,_5867054):-freeze(_5867052,freeze(_5867042,'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'(_5867042,_5867044,_5867046,_5867048,_5867050,_5867052,_5867054))).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'('RadExpr.Lit'(_5862242),_5862090,_5862108,_5862126,_5869522,_5869528,_5869534):-makeShare(_5862242,_5867664),hnf('Rational.ratEq'(_5867664,'Normalize.rZero'),_5872126,_5869528,_5872080),'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5872126,_5867664,_5862090,_5862108,_5862126,_5869522,_5872080,_5869534).

'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5872792,_5872794,_5872796,_5872798,_5872800,_5872802,_5872804,_5872806):-freeze(_5872804,freeze(_5872792,'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_5872792,_5872794,_5872796,_5872798,_5872800,_5872802,_5872804,_5872806))).
'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_5867664,_5862090,_5862108,_5862126,'RadExpr.Lit'('Normalize.rZero'),_5873222,_5873222).
'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_5867664,_5862090,_5862108,_5862126,_5876750,_5876756,_5876762):-!,makeShare(_5867664,_5874922),hnf('Rational.ratEq'(_5874922,'Normalize.rOne'),_5880574,_5876756,_5880528),'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_5880574,_5874922,_5862090,_5862108,_5862126,_5876750,_5880528,_5876762).

'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_5881462,_5881464,_5881466,_5881468,_5881470,_5881472,_5881474,_5881476):-freeze(_5881474,freeze(_5881462,'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_5881462,_5881464,_5881466,_5881468,_5881470,_5881472,_5881474,_5881476))).
'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5874922,_5862090,_5862108,_5862126,_5881886,_5881892,_5881898):-hnf(_5862090,_5881886,_5881892,_5881898).
'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5874922,_5862090,_5862108,_5862126,_5885112,_5885118,_5885124):-!,hnf('Rational.ratEq'(_5874922,'Normalize.rNegOne'),_5890160,_5885118,_5890114),'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5890160,_5874922,_5862090,_5862108,_5862126,_5885112,_5890114,_5885124).

'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5891240,_5891242,_5891244,_5891246,_5891248,_5891250,_5891252,_5891254):-freeze(_5891252,freeze(_5891240,'blocked_blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5891240,_5891242,_5891244,_5891246,_5891248,_5891250,_5891252,_5891254))).
'blocked_blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5874922,_5862090,_5862108,_5862126,'RadExpr.Neg'(_5862090),_5891670,_5891670).
'blocked_blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5874922,_5862090,_5862108,_5862126,'RadExpr.Mul'(_5862090,_5862108),_5893492,_5893492):-!.
'blocked_blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5895114),_5874922,_5862090,_5862108,_5862126,'FAIL'(_5895114),_5895128,_5895128).
'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5895212),_5874922,_5862090,_5862108,_5862126,'FAIL'(_5895212),_5895226,_5895226).
'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_5895310),_5867664,_5862090,_5862108,_5862126,'FAIL'(_5895310),_5895324,_5895324).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'('RadExpr.Neg'(_5863762),_5862090,_5862108,_5862126,_5895704,_5895710,_5895716):-hnf(_5862126,_5895704,_5895710,_5895716).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'('RadExpr.Add'(_5863872,_5863890),_5862090,_5862108,_5862126,_5896754,_5896760,_5896766):-hnf(_5862126,_5896754,_5896760,_5896766).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'('RadExpr.Mul'(_5864006,_5864024),_5862090,_5862108,_5862126,_5897870,_5897876,_5897882):-hnf(_5862126,_5897870,_5897876,_5897882).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'('RadExpr.Inv'(_5864140),_5862090,_5862108,_5862126,_5898978,_5898984,_5898990):-hnf(_5862126,_5898978,_5898984,_5898990).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'('RadExpr.Root'(_5864250,_5864268),_5862090,_5862108,_5862126,_5900040,_5900046,_5900052):-hnf(_5862126,_5900040,_5900046,_5900052).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'('RadExpr.Pow'(_5864384,_5864402),_5862090,_5862108,_5862126,_5901156,_5901162,_5901168):-!,hnf(_5862126,_5901156,_5901162,_5901168).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0_1'('FAIL'(_5901986),_5862090,_5862108,_5862126,'FAIL'(_5901986),_5902000,_5902000).

'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0'(_5904416,_5904418,_5904420,_5904422,_5904424,_5904426,_5904428):-freeze(_5904426,'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0'(_5904416,_5904418,_5904420,_5904422,_5904424,_5904426,_5904428)).
'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0'(_5904532,_5904550,_5904568,_5904586,_5906294,_5906300,_5906306):-hnf(_5904532,_5908928,_5906300,_5908958),'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5908928,_5904550,_5904568,_5904586,_5906294,_5908958,_5906306).

'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5909602,_5909604,_5909606,_5909608,_5909610,_5909612,_5909614):-freeze(_5909612,freeze(_5909602,'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5909602,_5909604,_5909606,_5909608,_5909610,_5909612,_5909614))).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1'('Prelude.True',_5904550,_5904568,_5904586,'RadExpr.Lit'('Normalize.rZero'),_5910022,_5910022).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1'('Prelude.False',_5904550,_5904568,_5904586,_5913314,_5913320,_5913326):-!,makeShare(_5904550,_5911600),hnf('Rational.ratEq'(_5911600,'Normalize.rOne'),_5916836,_5913320,_5916796),'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_5916836,_5911600,_5904568,_5904586,_5913314,_5916796,_5913326).

'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_5917674,_5917676,_5917678,_5917680,_5917682,_5917684,_5917686):-freeze(_5917684,freeze(_5917674,'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_5917674,_5917676,_5917678,_5917680,_5917682,_5917684,_5917686))).
'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_5911600,_5904568,_5904586,_5918088,_5918094,_5918100):-hnf(_5904586,_5918088,_5918094,_5918100).
'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_5911600,_5904568,_5904586,_5921078,_5921084,_5921090):-!,hnf('Rational.ratEq'(_5911600,'Normalize.rNegOne'),_5925824,_5921084,_5925784),'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5925824,_5911600,_5904568,_5904586,_5921078,_5925784,_5921090).

'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5926854,_5926856,_5926858,_5926860,_5926862,_5926864,_5926866):-freeze(_5926864,freeze(_5926854,'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5926854,_5926856,_5926858,_5926860,_5926862,_5926864,_5926866))).
'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5911600,_5904568,_5904586,'RadExpr.Neg'(_5904586),_5927274,_5927274).
'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5911600,_5904568,_5904586,'RadExpr.Mul'(_5904568,_5904586),_5928974,_5928974):-!.
'blocked_blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5930474),_5911600,_5904568,_5904586,'FAIL'(_5930474),_5930488,_5930488).
'blocked_blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_5930564),_5911600,_5904568,_5904586,'FAIL'(_5930564),_5930578,_5930578).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_5930654),_5904550,_5904568,_5904586,'FAIL'(_5930654),_5930668,_5930668).

'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5933492,_5933494,_5933496,_5933498,_5933500,_5933502):-freeze(_5933500,'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5933492,_5933494,_5933496,_5933498,_5933500,_5933502)).
'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5933598,_5933616,_5933634,_5935294,_5935300,_5935306):-hnf(_5933598,_5938352,_5935300,_5938376),'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5938352,_5933616,_5933634,_5935294,_5938376,_5935306).

'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5939084,_5939086,_5939088,_5939090,_5939092,_5939094):-freeze(_5939092,freeze(_5939084,'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_5939084,_5939086,_5939088,_5939090,_5939092,_5939094))).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5933750),_5933616,_5933634,'RadExpr.Lit'('Rational.ratMul'(_5933616,_5933750)),_5939492,_5939492).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5934098),_5933616,_5933634,_5941132,_5941138,_5941144):-hnf(_5933634,_5941132,_5941138,_5941144).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5934208,_5934226),_5933616,_5933634,_5942324,_5942330,_5942336):-hnf(_5933634,_5942324,_5942330,_5942336).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5934342,_5934360),_5933616,_5933634,_5943582,_5943588,_5943594):-hnf(_5933634,_5943582,_5943588,_5943594).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5934476),_5933616,_5933634,_5944832,_5944838,_5944844):-hnf(_5933634,_5944832,_5944838,_5944844).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5934586,_5934604),_5933616,_5933634,_5946036,_5946042,_5946048):-hnf(_5933634,_5946036,_5946042,_5946048).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5934720,_5934738),_5933616,_5933634,_5947294,_5947300,_5947306):-!,hnf(_5933634,_5947294,_5947300,_5947306).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_5948266),_5933616,_5933634,'FAIL'(_5948266),_5948280,_5948280).

'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0'(_5950280,_5950282,_5950284,_5950286,_5950288,_5950290,_5950292,_5950294):-freeze(_5950292,'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0'(_5950280,_5950282,_5950284,_5950286,_5950288,_5950290,_5950292,_5950294)).
'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0'(_5950406,_5950424,_5950442,_5950460,_5950478,_5952784,_5952790,_5952796):-hnf(_5950406,_5954994,_5952790,_5955030),'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'(_5954994,_5950424,_5950442,_5950460,_5950478,_5952784,_5955030,_5952796).

'blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'(_5955610,_5955612,_5955614,_5955616,_5955618,_5955620,_5955622,_5955624):-freeze(_5955622,freeze(_5955610,'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'(_5955610,_5955612,_5955614,_5955616,_5955618,_5955620,_5955622,_5955624))).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5950594),_5950424,_5950442,_5950460,_5950478,_5956032,_5956038,_5956044):-makeShare(_5950628,_5956388),makeShare(_5950594,_5956408),hnf('Prelude.cond'(letrec4PAKCS(_5956388,'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0'('Rational.ratEq'(_5956408,'Normalize.rZero'),_5956408,_5950424,_5950442)),'Normalize.foldConstantsMul._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_5950460,_5956408,_5956388)),_5956032,_5956038,_5956044).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5951732),_5950424,_5950442,_5950460,_5950478,_5960388,_5960394,_5960400):-hnf(_5950478,_5960388,_5960394,_5960400).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5951842,_5951860),_5950424,_5950442,_5950460,_5950478,_5961584,_5961590,_5961596):-hnf(_5950478,_5961584,_5961590,_5961596).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5951976,_5951994),_5950424,_5950442,_5950460,_5950478,_5962846,_5962852,_5962858):-hnf(_5950478,_5962846,_5962852,_5962858).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5952110),_5950424,_5950442,_5950460,_5950478,_5964100,_5964106,_5964112):-hnf(_5950478,_5964100,_5964106,_5964112).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5952220,_5952238),_5950424,_5950442,_5950460,_5950478,_5965308,_5965314,_5965320):-hnf(_5950478,_5965308,_5965314,_5965320).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5952354,_5952372),_5950424,_5950442,_5950460,_5950478,_5966570,_5966576,_5966582):-!,hnf(_5950478,_5966570,_5966576,_5966582).
'blocked_blocked_Normalize.foldConstantsMul._\'23caseor0._\'23caseor0_1'('FAIL'(_5967546),_5950424,_5950442,_5950460,_5950478,'FAIL'(_5967546),_5967560,_5967560).

'Normalize.simplifyPowers._\'23caseor0._\'23caseor0'(_5969500,_5969502,_5969504,_5969506,_5969508,_5969510,_5969512,_5969514):-freeze(_5969512,'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0'(_5969500,_5969502,_5969504,_5969506,_5969508,_5969510,_5969512,_5969514)).
'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0'(_5969626,_5969644,_5969662,_5969680,_5969698,_5972772,_5972778,_5972784):-hnf(_5969626,_5974910,_5972778,_5974946),'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'(_5974910,_5969644,_5969662,_5969680,_5969698,_5972772,_5974946,_5972784).

'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'(_5975514,_5975516,_5975518,_5975520,_5975522,_5975524,_5975526,_5975528):-freeze(_5975526,freeze(_5975514,'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'(_5975514,_5975516,_5975518,_5975520,_5975522,_5975524,_5975526,_5975528))).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_5969814,_5969832),_5969644,_5969662,_5969680,_5969698,_5978156,_5978162,_5978168):-hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5969814,2),_5981170,_5978162,_5981112),'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'(_5981170,_5969814,_5969832,_5969644,_5969662,_5969680,_5969698,_5978156,_5981112,_5978168).

'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'(_5981894,_5981896,_5981898,_5981900,_5981902,_5981904,_5981906,_5981908,_5981910,_5981912):-freeze(_5981910,freeze(_5981894,'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'(_5981894,_5981896,_5981898,_5981900,_5981902,_5981904,_5981906,_5981908,_5981910,_5981912))).
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'('Prelude.True',_5969814,_5969832,_5969644,_5969662,_5969680,_5969698,_5985216,_5985222,_5985228):-makeShare(_5969644,_5982486),makeShare(_5969832,_5982506),hnf('RadExpr._impl\'23\'3D\'3D\'23Prelude.Eq\'23RadExpr.RadExpr\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23',[]),_5982486,_5982506),_5989434,_5985222,_5989376),'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase_Prelude.True_ComplexCase'(_5989434,_5969814,_5982506,_5982486,_5969662,_5969680,_5969698,_5985216,_5989376,_5985228).

'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase_Prelude.True_ComplexCase'(_5990380,_5990382,_5990384,_5990386,_5990388,_5990390,_5990392,_5990394,_5990396,_5990398):-freeze(_5990396,freeze(_5990380,'blocked_blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase_Prelude.True_ComplexCase'(_5990380,_5990382,_5990384,_5990386,_5990388,_5990390,_5990392,_5990394,_5990396,_5990398))).
'blocked_blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase_Prelude.True_ComplexCase'('Prelude.True',_5969814,_5982506,_5982486,_5969662,_5969680,_5969698,_5990824,_5990830,_5990836):-hnf('Normalize.simplifyPowers'(_5982486),_5990824,_5990830,_5990836).
'blocked_blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase_Prelude.True_ComplexCase'('Prelude.False',_5969814,_5982506,_5982486,_5969662,_5969680,_5969698,'RadExpr.Mul'('Normalize.simplifyPowers'('RadExpr.Root'(2,_5982486)),'Normalize.simplifyPowers'('RadExpr.Root'(2,_5982506))),_5992752,_5992752):-!.
'blocked_blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase_Prelude.True_ComplexCase'('FAIL'(_5995410),_5969814,_5982506,_5982486,_5969662,_5969680,_5969698,'FAIL'(_5995410),_5995424,_5995424).
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'('Prelude.False',_5969814,_5969832,_5969644,_5969662,_5969680,_5969698,'RadExpr.Mul'('Normalize.simplifyPowers'(_5969662),'Normalize.simplifyPowers'(_5969680)),_5995840,_5995840):-!.
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'('FAIL'(_5997776),_5969814,_5969832,_5969644,_5969662,_5969680,_5969698,'FAIL'(_5997776),_5997790,_5997790).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_5971756),_5969644,_5969662,_5969680,_5969698,_5998186,_5998192,_5998198):-hnf(_5969698,_5998186,_5998192,_5998198).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_5971866),_5969644,_5969662,_5969680,_5969698,_5999362,_5999368,_5999374):-hnf(_5969698,_5999362,_5999368,_5999374).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_5971976,_5971994),_5969644,_5969662,_5969680,_5969698,_6000546,_6000552,_6000558):-hnf(_5969698,_6000546,_6000552,_6000558).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_5972110,_5972128),_5969644,_5969662,_5969680,_5969698,_6001796,_6001802,_6001808):-hnf(_5969698,_6001796,_6001802,_6001808).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_5972244),_5969644,_5969662,_5969680,_5969698,_6003038,_6003044,_6003050):-hnf(_5969698,_6003038,_6003044,_6003050).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_5972354,_5972372),_5969644,_5969662,_5969680,_5969698,_6004222,_6004228,_6004234):-!,hnf(_5969698,_6004222,_6004228,_6004234).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0_1'('FAIL'(_6005186),_5969644,_5969662,_5969680,_5969698,'FAIL'(_6005186),_6005200,_6005200).

'Normalize.simplifyPowers._\'23caseor0'(_6006732,_6006734,_6006736,_6006738,_6006740,_6006742,_6006744):-freeze(_6006742,'blocked_Normalize.simplifyPowers._\'23caseor0'(_6006732,_6006734,_6006736,_6006738,_6006740,_6006742,_6006744)).
'blocked_Normalize.simplifyPowers._\'23caseor0'(_6006848,_6006866,_6006884,_6006902,_6009790,_6009796,_6009802):-hnf(_6006848,_6011488,_6009796,_6011518),'blocked_Normalize.simplifyPowers._\'23caseor0_1'(_6011488,_6006866,_6006884,_6006902,_6009790,_6011518,_6009802).

'blocked_Normalize.simplifyPowers._\'23caseor0_1'(_6012006,_6012008,_6012010,_6012012,_6012014,_6012016,_6012018):-freeze(_6012016,freeze(_6012006,'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'(_6012006,_6012008,_6012010,_6012012,_6012014,_6012016,_6012018))).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'('RadExpr.Root'(_6007018,_6007036),_6006866,_6006884,_6006902,_6014566,_6014572,_6014578):-hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6007018,2),_6017134,_6014572,_6017082),'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1_RadExpr.Root_ComplexCase'(_6017134,_6007018,_6007036,_6006866,_6006884,_6006902,_6014566,_6017082,_6014578).

'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1_RadExpr.Root_ComplexCase'(_6017784,_6017786,_6017788,_6017790,_6017792,_6017794,_6017796,_6017798,_6017800):-freeze(_6017798,freeze(_6017784,'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1_RadExpr.Root_ComplexCase'(_6017784,_6017786,_6017788,_6017790,_6017792,_6017794,_6017796,_6017798,_6017800))).
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1_RadExpr.Root_ComplexCase'('Prelude.True',_6007018,_6007036,_6006866,_6006884,_6006902,_6018218,_6018224,_6018230):-makeShare(_6007328,_6018570),makeShare(_6006866,_6018590),makeShare(_6006884,_6018610),hnf('Prelude.cond'(letrec4PAKCS(_6018570,'RadExpr.Mul'('Normalize.simplifyPowers'(_6018590),'Normalize.simplifyPowers'(_6018610))),'Normalize.simplifyPowers._\'23caseor0._\'23caseor0'(_6018610,_6007036,_6018590,_6018610,_6018570)),_6018218,_6018224,_6018230).
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1_RadExpr.Root_ComplexCase'('Prelude.False',_6007018,_6007036,_6006866,_6006884,_6006902,'RadExpr.Mul'('Normalize.simplifyPowers'(_6006866),'Normalize.simplifyPowers'(_6006884)),_6022596,_6022596):-!.
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1_RadExpr.Root_ComplexCase'('FAIL'(_6024386),_6007018,_6007036,_6006866,_6006884,_6006902,'FAIL'(_6024386),_6024400,_6024400).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'('RadExpr.Lit'(_6008846),_6006866,_6006884,_6006902,_6024788,_6024794,_6024800):-hnf(_6006902,_6024788,_6024794,_6024800).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'('RadExpr.Neg'(_6008956),_6006866,_6006884,_6006902,_6025818,_6025824,_6025830):-hnf(_6006902,_6025818,_6025824,_6025830).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'('RadExpr.Add'(_6009066,_6009084),_6006866,_6006884,_6006902,_6026856,_6026862,_6026868):-hnf(_6006902,_6026856,_6026862,_6026868).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'('RadExpr.Mul'(_6009200,_6009218),_6006866,_6006884,_6006902,_6027960,_6027966,_6027972):-hnf(_6006902,_6027960,_6027966,_6027972).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'('RadExpr.Inv'(_6009334),_6006866,_6006884,_6006902,_6029056,_6029062,_6029068):-hnf(_6006902,_6029056,_6029062,_6029068).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'('RadExpr.Pow'(_6009444,_6009462),_6006866,_6006884,_6006902,_6030094,_6030100,_6030106):-!,hnf(_6006902,_6030094,_6030100,_6030106).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0_1'('FAIL'(_6030912),_6006866,_6006884,_6006902,'FAIL'(_6030912),_6030926,_6030926).

'Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0'(_6033266,_6033268,_6033270,_6033272,_6033274,_6033276):-freeze(_6033274,'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0'(_6033266,_6033268,_6033270,_6033272,_6033274,_6033276)).
'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0'(_6033372,_6033390,_6033408,_6035958,_6035964,_6035970):-hnf(_6033372,_6038512,_6035964,_6038536),'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6038512,_6033390,_6033408,_6035958,_6038536,_6035970).

'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6039160,_6039162,_6039164,_6039166,_6039168,_6039170):-freeze(_6039168,freeze(_6039160,'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6039160,_6039162,_6039164,_6039166,_6039168,_6039170))).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6033524,_6033542),_6033390,_6033408,_6039570,_6039576,_6039582):-hnf('Normalize.simplifyPowers'('RadExpr.Pow'(_6033524,'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6033542,_6033390))),_6039570,_6039576,_6039582).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6034050,_6034068),_6033390,_6033408,_6043974,_6043980,_6043986):-makeShare(_6033390,_6042020),makeShare(_6034050,_6042040),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6042020,_6042040),_6047392,_6043980,_6047346),'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'(_6047392,_6042040,_6034068,_6042020,_6033408,_6043974,_6047346,_6043986).

'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'(_6048208,_6048210,_6048212,_6048214,_6048216,_6048218,_6048220,_6048222):-freeze(_6048220,freeze(_6048208,'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'(_6048208,_6048210,_6048212,_6048214,_6048216,_6048218,_6048220,_6048222))).
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'('Prelude.True',_6042040,_6034068,_6042020,_6033408,_6048632,_6048638,_6048644):-hnf('Normalize.simplifyPowers'(_6034068),_6048632,_6048638,_6048644).
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'('Prelude.False',_6042040,_6034068,_6042020,_6033408,'RadExpr.Pow'('Normalize.simplifyPowers'('RadExpr.Root'(_6042040,_6034068)),_6042020),_6050286,_6050286):-!.
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Root_ComplexCase'('FAIL'(_6052190),_6042040,_6034068,_6042020,_6033408,'FAIL'(_6052190),_6052204,_6052204).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6035004),_6033390,_6033408,_6052584,_6052590,_6052596):-hnf(_6033408,_6052584,_6052590,_6052596).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6035114),_6033390,_6033408,_6053684,_6053690,_6053696):-hnf(_6033408,_6053684,_6053690,_6053696).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6035224,_6035242),_6033390,_6033408,_6054792,_6054798,_6054804):-hnf(_6033408,_6054792,_6054798,_6054804).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6035358,_6035376),_6033390,_6033408,_6055966,_6055972,_6055978):-hnf(_6033408,_6055966,_6055972,_6055978).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6035492),_6033390,_6033408,_6057132,_6057138,_6057144):-!,hnf(_6033408,_6057132,_6057138,_6057144).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_6057954),_6033390,_6033408,'FAIL'(_6057954),_6057968,_6057968).

'Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6060708,_6060710,_6060712,_6060714,_6060716,_6060718):-freeze(_6060716,'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6060708,_6060710,_6060712,_6060714,_6060716,_6060718)).
'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6060814,_6060832,_6060850,_6063472,_6063478,_6063484):-hnf(_6060814,_6066458,_6063478,_6066482),'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6066458,_6060832,_6060850,_6063472,_6066482,_6063484).

'blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6067178,_6067180,_6067182,_6067184,_6067186,_6067188):-freeze(_6067186,freeze(_6067178,'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6067178,_6067180,_6067182,_6067184,_6067186,_6067188))).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6060966,_6060984),_6060832,_6060850,_6069814,_6069820,_6069826):-makeShare(_6060984,_6067788),makeShare(_6060832,_6067808),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6067788,_6067808),_6073634,_6069820,_6073588),'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Pow_ComplexCase'(_6073634,_6060966,_6067788,_6067808,_6060850,_6069814,_6073588,_6069826).

'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Pow_ComplexCase'(_6074516,_6074518,_6074520,_6074522,_6074524,_6074526,_6074528,_6074530):-freeze(_6074528,freeze(_6074516,'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Pow_ComplexCase'(_6074516,_6074518,_6074520,_6074522,_6074524,_6074526,_6074528,_6074530))).
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Pow_ComplexCase'('Prelude.True',_6060966,_6067788,_6067808,_6060850,_6074940,_6074946,_6074952):-hnf('Normalize.simplifyPowers'(_6060966),_6074940,_6074946,_6074952).
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Pow_ComplexCase'('Prelude.False',_6060966,_6067788,_6067808,_6060850,'RadExpr.Root'(_6067808,'Normalize.simplifyPowers'('RadExpr.Pow'(_6060966,_6067788))),_6076660,_6076660):-!.
'blocked_blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Pow_ComplexCase'('FAIL'(_6078630),_6060966,_6067788,_6067808,_6060850,'FAIL'(_6078630),_6078644,_6078644).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6061920,_6061938),_6060832,_6060850,'RadExpr.Root'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6060832,_6061920),'Normalize.simplifyPowers'(_6061938)),_6079050,_6079050).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6062446),_6060832,_6060850,_6081334,_6081340,_6081346):-hnf(_6060850,_6081334,_6081340,_6081346).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6062556),_6060832,_6060850,_6082506,_6082512,_6082518):-hnf(_6060850,_6082506,_6082512,_6082518).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6062666,_6062684),_6060832,_6060850,_6083686,_6083692,_6083698):-hnf(_6060850,_6083686,_6083692,_6083698).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6062800,_6062818),_6060832,_6060850,_6084932,_6084938,_6084944):-hnf(_6060850,_6084932,_6084938,_6084944).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6062934),_6060832,_6060850,_6086170,_6086176,_6086182):-!,hnf(_6060850,_6086170,_6086176,_6086182).
'blocked_blocked_Normalize.simplifyPowers._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_6087064),_6060832,_6060850,'FAIL'(_6087064),_6087078,_6087078).

'Normalize.extractPerfectPowers._\'23caseor0'(_6088822,_6088824,_6088826,_6088828,_6088830,_6088832):-freeze(_6088830,'blocked_Normalize.extractPerfectPowers._\'23caseor0'(_6088822,_6088824,_6088826,_6088828,_6088830,_6088832)).
'blocked_Normalize.extractPerfectPowers._\'23caseor0'(_6088928,_6088946,_6088964,_6090348,_6090354,_6090360):-hnf(_6088928,_6092254,_6090354,_6092278),'blocked_Normalize.extractPerfectPowers._\'23caseor0_1'(_6092254,_6088946,_6088964,_6090348,_6092278,_6090360).

'blocked_Normalize.extractPerfectPowers._\'23caseor0_1'(_6092794,_6092796,_6092798,_6092800,_6092802,_6092804):-freeze(_6092802,freeze(_6092794,'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'(_6092794,_6092796,_6092798,_6092800,_6092802,_6092804))).
'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'('RadExpr.Lit'(_6089080),_6088946,_6088964,_6093196,_6093202,_6093208):-hnf('Normalize.extractPerfectPowersLit'(_6088946,_6089080),_6093196,_6093202,_6093208).
'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'('RadExpr.Neg'(_6089344),_6088946,_6088964,_6094600,_6094606,_6094612):-hnf(_6088964,_6094600,_6094606,_6094612).
'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'('RadExpr.Add'(_6089454,_6089472),_6088946,_6088964,_6095600,_6095606,_6095612):-hnf(_6088964,_6095600,_6095606,_6095612).
'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'('RadExpr.Mul'(_6089588,_6089606),_6088946,_6088964,_6096666,_6096672,_6096678):-hnf(_6088964,_6096666,_6096672,_6096678).
'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'('RadExpr.Inv'(_6089722),_6088946,_6088964,_6097724,_6097730,_6097736):-hnf(_6088964,_6097724,_6097730,_6097736).
'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'('RadExpr.Root'(_6089832,_6089850),_6088946,_6088964,_6098736,_6098742,_6098748):-hnf(_6088964,_6098736,_6098742,_6098748).
'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'('RadExpr.Pow'(_6089966,_6089984),_6088946,_6088964,_6099802,_6099808,_6099814):-!,hnf(_6088964,_6099802,_6099808,_6099814).
'blocked_blocked_Normalize.extractPerfectPowers._\'23caseor0_1'('FAIL'(_6100582),_6088946,_6088964,'FAIL'(_6100582),_6100596,_6100596).

'Normalize.extractPerfectPowersLit._\'23caseor0'(_6102454,_6102456,_6102458,_6102460,_6102462,_6102464,_6102466,_6102468,_6102470):-freeze(_6102468,'blocked_Normalize.extractPerfectPowersLit._\'23caseor0'(_6102454,_6102456,_6102458,_6102460,_6102462,_6102464,_6102466,_6102468,_6102470)).
'blocked_Normalize.extractPerfectPowersLit._\'23caseor0'(_6102590,_6102608,_6102626,_6102644,_6102662,_6102680,_6106836,_6106842,_6106848):-hnf(_6102590,_6108874,_6106842,_6108916),'blocked_Normalize.extractPerfectPowersLit._\'23caseor0_1'(_6108874,_6102608,_6102626,_6102644,_6102662,_6102680,_6106836,_6108916,_6106848).

'blocked_Normalize.extractPerfectPowersLit._\'23caseor0_1'(_6109474,_6109476,_6109478,_6109480,_6109482,_6109484,_6109486,_6109488,_6109490):-freeze(_6109488,freeze(_6109474,'blocked_blocked_Normalize.extractPerfectPowersLit._\'23caseor0_1'(_6109474,_6109476,_6109478,_6109480,_6109482,_6109484,_6109486,_6109488,_6109490))).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23caseor0_1'('Prelude.True',_6102608,_6102626,_6102644,_6102662,_6102680,_6109908,_6109914,_6109920):-hnf('Normalize.extractPerfectPowersLit.buildExtracted.145'(_6102680,'Rational.ratDiv'(_6102626,_6102644),_6102608),_6109908,_6109914,_6109920).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23caseor0_1'('Prelude.False',_6102608,_6102626,_6102644,_6102662,_6102680,_6112058,_6112064,_6112070):-!,makeShare(_6103270,_6113520),makeShare(_6102662,_6113540),makeShare(_6102680,_6113560),makeShare(_6103288,_6113580),makeShare(_6103306,_6113600),makeShare(_6103324,_6113620),makeShare(_6103342,_6113640),hnf('Prelude.cond'(letrec4PAKCS(_6113520,'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6102608,'Normalize.intPow'(_6113540,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_6113560,1)))),'Prelude.cond'(letrec4PAKCS(_6113580,'Rational.ratDiv'(_6102626,'Rational.ratMul'(_6102644,'Rational.fromInt'(_6113540)))),'Prelude.cond'(letrec4PAKCS(_6113600,'Normalize.extractNthPower'(_6113560,_6113520)),'Prelude.cond'(letrec4PAKCS(_6113620,'Normalize.extractPerfectPowersLit._\'23selFP4\'23numOut2'(_6113600)),'Prelude.cond'(letrec4PAKCS(_6113640,'Normalize.extractPerfectPowersLit._\'23selFP5\'23numIn2'(_6113600)),'Normalize.extractPerfectPowersLit.buildExtracted.145'(_6113560,'Rational.ratMul'(_6113580,_6113620),_6113640)))))),_6112058,_6112064,_6112070).
'blocked_blocked_Normalize.extractPerfectPowersLit._\'23caseor0_1'('FAIL'(_6122976),_6102608,_6102626,_6102644,_6102662,_6102680,'FAIL'(_6122976),_6122990,_6122990).

'Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0'(_6125594,_6125596,_6125598,_6125600,_6125602,_6125604,_6125606,_6125608,_6125610):-freeze(_6125608,'blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0'(_6125594,_6125596,_6125598,_6125600,_6125602,_6125604,_6125606,_6125608,_6125610)).
'blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0'(_6125730,_6125748,_6125766,_6125784,_6125802,_6125820,_6127932,_6127938,_6127944):-hnf(_6125730,_6130654,_6127938,_6130696),'blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1'(_6130654,_6125748,_6125766,_6125784,_6125802,_6125820,_6127932,_6130696,_6127944).

'blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1'(_6131368,_6131370,_6131372,_6131374,_6131376,_6131378,_6131380,_6131382,_6131384):-freeze(_6131382,freeze(_6131368,'blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1'(_6131368,_6131370,_6131372,_6131374,_6131376,_6131378,_6131380,_6131382,_6131384))).
'blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1'('Prelude.True',_6125748,_6125766,_6125784,_6125802,_6125820,'RadExpr.Lit'('Normalize.rOne'),_6131808,_6131808).
'blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1'('Prelude.False',_6125748,_6125766,_6125784,_6125802,_6125820,_6133876,_6133882,_6133888):-!,hnf(_6125748,_6137378,_6133882,_6137414),'blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1'(_6137378,_6125766,_6125784,_6125802,_6125820,_6133876,_6137414,_6133888).

'blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1'(_6138240,_6138242,_6138244,_6138246,_6138248,_6138250,_6138252,_6138254):-freeze(_6138252,freeze(_6138240,'blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1'(_6138240,_6138242,_6138244,_6138246,_6138248,_6138250,_6138252,_6138254))).
'blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1'('Prelude.True',_6125766,_6125784,_6125802,_6125820,'RadExpr.Root'(_6125802,'RadExpr.Lit'('Rational.fromInt'(_6125820))),_6138670,_6138670).
'blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1'('Prelude.False',_6125766,_6125784,_6125802,_6125820,_6141344,_6141350,_6141356):-!,hnf(_6125766,_6145702,_6141350,_6145732),'blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1_Prelude.False_1'(_6145702,_6125784,_6125802,_6125820,_6141344,_6145732,_6141356).

'blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1_Prelude.False_1'(_6146694,_6146696,_6146698,_6146700,_6146702,_6146704,_6146706):-freeze(_6146704,freeze(_6146694,'blocked_blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1_Prelude.False_1'(_6146694,_6146696,_6146698,_6146700,_6146702,_6146704,_6146706))).
'blocked_blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1_Prelude.False_1'('Prelude.True',_6125784,_6125802,_6125820,'RadExpr.Lit'(_6125784),_6147114,_6147114).
'blocked_blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1_Prelude.False_1'('Prelude.False',_6125784,_6125802,_6125820,'RadExpr.Mul'('RadExpr.Lit'(_6125784),'RadExpr.Root'(_6125802,'RadExpr.Lit'('Rational.fromInt'(_6125820)))),_6148706,_6148706):-!.
'blocked_blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1_Prelude.False_1'('FAIL'(_6150954),_6125784,_6125802,_6125820,'FAIL'(_6150954),_6150968,_6150968).
'blocked_blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1_Prelude.False_1'('FAIL'(_6151044),_6125766,_6125784,_6125802,_6125820,'FAIL'(_6151044),_6151058,_6151058).
'blocked_blocked_Normalize.extractPerfectPowersLit.buildExtracted.145._\'23caseor0_1'('FAIL'(_6151142),_6125748,_6125766,_6125784,_6125802,_6125820,'FAIL'(_6151142),_6151156,_6151156).

'Normalize.collectCoefficients._\'23caseor0'(_6152886,_6152888,_6152890,_6152892,_6152894):-freeze(_6152892,'blocked_Normalize.collectCoefficients._\'23caseor0'(_6152886,_6152888,_6152890,_6152892,_6152894)).
'blocked_Normalize.collectCoefficients._\'23caseor0'(_6152982,_6153000,_6154392,_6154398,_6154404):-hnf(_6152982,_6156254,_6154398,_6156272),'blocked_Normalize.collectCoefficients._\'23caseor0_1'(_6156254,_6153000,_6154392,_6156272,_6154404).

'blocked_Normalize.collectCoefficients._\'23caseor0_1'(_6156774,_6156776,_6156778,_6156780,_6156782):-freeze(_6156780,freeze(_6156774,'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'(_6156774,_6156776,_6156778,_6156780,_6156782))).
'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'('RadExpr.Lit'(_6153116),_6153000,'RadExpr.Lit'('Rational.ratNeg'(_6153116)),_6157172,_6157172).
'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'('RadExpr.Neg'(_6153394),_6153000,_6158436,_6158442,_6158448):-hnf(_6153000,_6158436,_6158442,_6158448).
'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'('RadExpr.Add'(_6153504,_6153522),_6153000,_6159356,_6159362,_6159368):-hnf(_6153000,_6159356,_6159362,_6159368).
'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'('RadExpr.Mul'(_6153638,_6153656),_6153000,_6160342,_6160348,_6160354):-hnf(_6153000,_6160342,_6160348,_6160354).
'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'('RadExpr.Inv'(_6153772),_6153000,_6161320,_6161326,_6161332):-hnf(_6153000,_6161320,_6161326,_6161332).
'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'('RadExpr.Root'(_6153882,_6153900),_6153000,_6162252,_6162258,_6162264):-hnf(_6153000,_6162252,_6162258,_6162264).
'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'('RadExpr.Pow'(_6154016,_6154034),_6153000,_6163238,_6163244,_6163250):-!,hnf(_6153000,_6163238,_6163244,_6163250).
'blocked_blocked_Normalize.collectCoefficients._\'23caseor0_1'('FAIL'(_6163938),_6153000,'FAIL'(_6163938),_6163952,_6163952).

'Normalize.flattenMulN._\'23caseor0'(_6165346,_6165348,_6165350,_6165352,_6165354):-freeze(_6165352,'blocked_Normalize.flattenMulN._\'23caseor0'(_6165346,_6165348,_6165350,_6165352,_6165354)).
'blocked_Normalize.flattenMulN._\'23caseor0'(_6165442,_6165460,_6166958,_6166964,_6166970):-hnf(_6165442,_6168532,_6166964,_6168550),'blocked_Normalize.flattenMulN._\'23caseor0_1'(_6168532,_6165460,_6166958,_6168550,_6166970).

'blocked_Normalize.flattenMulN._\'23caseor0_1'(_6169004,_6169006,_6169008,_6169010,_6169012):-freeze(_6169010,freeze(_6169004,'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'(_6169004,_6169006,_6169008,_6169010,_6169012))).
'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'('RadExpr.Mul'(_6165576,_6165594),_6165460,_6169404,_6169410,_6169416):-hnf('Prelude.++'('Normalize.flattenMulN'(_6165576),'Normalize.flattenMulN'(_6165594)),_6169404,_6169410,_6169416).
'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'('RadExpr.Lit'(_6166032),_6165460,_6171144,_6171150,_6171156):-hnf(_6165460,_6171144,_6171150,_6171156).
'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'('RadExpr.Neg'(_6166142),_6165460,_6172008,_6172014,_6172020):-hnf(_6165460,_6172008,_6172014,_6172020).
'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'('RadExpr.Add'(_6166252,_6166270),_6165460,_6172880,_6172886,_6172892):-hnf(_6165460,_6172880,_6172886,_6172892).
'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'('RadExpr.Inv'(_6166386),_6165460,_6173810,_6173816,_6173822):-hnf(_6165460,_6173810,_6173816,_6173822).
'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'('RadExpr.Root'(_6166496,_6166514),_6165460,_6174694,_6174700,_6174706):-hnf(_6165460,_6174694,_6174700,_6174706).
'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'('RadExpr.Pow'(_6166630,_6166648),_6165460,_6175632,_6175638,_6175644):-!,hnf(_6165460,_6175632,_6175638,_6175644).
'blocked_blocked_Normalize.flattenMulN._\'23caseor0_1'('FAIL'(_6176284),_6165460,'FAIL'(_6176284),_6176298,_6176298).

'Normalize.partitionLitsN._\'23caseor0._\'23caseor0'(_6178214,_6178216,_6178218,_6178220,_6178222,_6178224,_6178226,_6178228):-freeze(_6178226,'blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0'(_6178214,_6178216,_6178218,_6178220,_6178222,_6178224,_6178226,_6178228)).
'blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0'(_6178340,_6178358,_6178376,_6178394,_6178412,_6180904,_6180910,_6180916):-hnf(_6178340,_6183042,_6180910,_6183078),'blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'(_6183042,_6178358,_6178376,_6178394,_6178412,_6180904,_6183078,_6180916).

'blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'(_6183646,_6183648,_6183650,_6183652,_6183654,_6183656,_6183658,_6183660):-freeze(_6183658,freeze(_6183646,'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'(_6183646,_6183648,_6183650,_6183652,_6183654,_6183656,_6183658,_6183660))).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6178528),_6178358,_6178376,_6178394,_6178412,_6186150,_6186156,_6186162):-makeShare(_6178528,_6184320),hnf('Prelude.not'('Rational.ratEq'(_6184320,'Normalize.rZero')),_6189128,_6186156,_6189076),'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6189128,_6184320,_6178358,_6178376,_6178394,_6178412,_6186150,_6189076,_6186162).

'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6189856,_6189858,_6189860,_6189862,_6189864,_6189866,_6189868,_6189870,_6189872):-freeze(_6189870,freeze(_6189856,'blocked_blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6189856,_6189858,_6189860,_6189862,_6189864,_6189866,_6189868,_6189870,_6189872))).
'blocked_blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_6184320,_6178358,_6178376,_6178394,_6178412,'Prelude.(,)'(['Rational.ratDiv'('Normalize.rOne',_6184320)|_6178358],_6178394),_6190296,_6190296).
'blocked_blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_6184320,_6178358,_6178376,_6178394,_6178412,'Prelude.(,)'(_6178358,[_6178376|_6178394]),_6192552,_6192552):-!.
'blocked_blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_6194112),_6184320,_6178358,_6178376,_6178394,_6178412,'FAIL'(_6194112),_6194126,_6194126).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6179864),_6178358,_6178376,_6178394,_6178412,_6194514,_6194520,_6194526):-hnf(_6178412,_6194514,_6194520,_6194526).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6179974,_6179992),_6178358,_6178376,_6178394,_6178412,_6195698,_6195704,_6195710):-hnf(_6178412,_6195698,_6195704,_6195710).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6180108,_6180126),_6178358,_6178376,_6178394,_6178412,_6196948,_6196954,_6196960):-hnf(_6178412,_6196948,_6196954,_6196960).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6180242),_6178358,_6178376,_6178394,_6178412,_6198190,_6198196,_6198202):-hnf(_6178412,_6198190,_6198196,_6198202).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6180352,_6180370),_6178358,_6178376,_6178394,_6178412,_6199386,_6199392,_6199398):-hnf(_6178412,_6199386,_6199392,_6199398).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6180486,_6180504),_6178358,_6178376,_6178394,_6178412,_6200636,_6200642,_6200648):-!,hnf(_6178412,_6200636,_6200642,_6200648).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0._\'23caseor0_1'('FAIL'(_6201600),_6178358,_6178376,_6178394,_6178412,'FAIL'(_6201600),_6201614,_6201614).

'Normalize.partitionLitsN._\'23caseor0'(_6203146,_6203148,_6203150,_6203152,_6203154,_6203156,_6203158,_6203160):-freeze(_6203158,'blocked_Normalize.partitionLitsN._\'23caseor0'(_6203146,_6203148,_6203150,_6203152,_6203154,_6203156,_6203158,_6203160)).
'blocked_Normalize.partitionLitsN._\'23caseor0'(_6203272,_6203290,_6203308,_6203326,_6203344,_6205860,_6205866,_6205872):-hnf(_6203272,_6207566,_6205866,_6207602),'blocked_Normalize.partitionLitsN._\'23caseor0_1'(_6207566,_6203290,_6203308,_6203326,_6203344,_6205860,_6207602,_6205872).

'blocked_Normalize.partitionLitsN._\'23caseor0_1'(_6208098,_6208100,_6208102,_6208104,_6208106,_6208108,_6208110,_6208112):-freeze(_6208110,freeze(_6208098,'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'(_6208098,_6208100,_6208102,_6208104,_6208106,_6208108,_6208110,_6208112))).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'('RadExpr.Lit'(_6203460),_6203290,_6203308,_6203326,_6203344,'Prelude.(,)'([_6203460|_6203290],_6203326),_6208526,_6208526).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'('RadExpr.Inv'(_6203878),_6203290,_6203308,_6203326,_6203344,_6210098,_6210104,_6210110):-makeShare(_6203912,_6210514),makeShare(_6203290,_6210534),makeShare(_6203308,_6210554),makeShare(_6203326,_6210574),hnf('Prelude.cond'(letrec4PAKCS(_6210514,'Prelude.(,)'(_6210534,[_6210554|_6210574])),'Normalize.partitionLitsN._\'23caseor0._\'23caseor0'(_6203878,_6210534,_6210554,_6210574,_6210514)),_6210098,_6210104,_6210110).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'('RadExpr.Neg'(_6205002),_6203290,_6203308,_6203326,_6203344,_6214228,_6214234,_6214240):-hnf(_6203344,_6214228,_6214234,_6214240).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'('RadExpr.Add'(_6205112,_6205130),_6203290,_6203308,_6203326,_6203344,_6215340,_6215346,_6215352):-hnf(_6203344,_6215340,_6215346,_6215352).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'('RadExpr.Mul'(_6205246,_6205264),_6203290,_6203308,_6203326,_6203344,_6216518,_6216524,_6216530):-hnf(_6203344,_6216518,_6216524,_6216530).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'('RadExpr.Root'(_6205380,_6205398),_6203290,_6203308,_6203326,_6203344,_6217708,_6217714,_6217720):-hnf(_6203344,_6217708,_6217714,_6217720).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'('RadExpr.Pow'(_6205514,_6205532),_6203290,_6203308,_6203326,_6203344,_6218886,_6218892,_6218898):-!,hnf(_6203344,_6218886,_6218892,_6218898).
'blocked_blocked_Normalize.partitionLitsN._\'23caseor0_1'('FAIL'(_6219778),_6203290,_6203308,_6203326,_6203344,'FAIL'(_6219778),_6219792,_6219792).

'Normalize.applyCoeffMul._\'23caseor0'(_6221286,_6221288,_6221290,_6221292,_6221294,_6221296):-freeze(_6221294,'blocked_Normalize.applyCoeffMul._\'23caseor0'(_6221286,_6221288,_6221290,_6221292,_6221294,_6221296)).
'blocked_Normalize.applyCoeffMul._\'23caseor0'(_6221392,_6221410,_6221428,_6222854,_6222860,_6222866):-hnf(_6221392,_6224508,_6222860,_6224532),'blocked_Normalize.applyCoeffMul._\'23caseor0_1'(_6224508,_6221410,_6221428,_6222854,_6224532,_6222866).

'blocked_Normalize.applyCoeffMul._\'23caseor0_1'(_6225006,_6225008,_6225010,_6225012,_6225014,_6225016):-freeze(_6225014,freeze(_6225006,'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'(_6225006,_6225008,_6225010,_6225012,_6225014,_6225016))).
'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'('RadExpr.Lit'(_6221544),_6221410,_6221428,'RadExpr.Lit'('Rational.ratMul'(_6221410,_6221544)),_6225414,_6225414).
'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'('RadExpr.Neg'(_6221892),_6221410,_6221428,_6226820,_6226826,_6226832):-hnf(_6221428,_6226820,_6226826,_6226832).
'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'('RadExpr.Add'(_6222002,_6222020),_6221410,_6221428,_6227778,_6227784,_6227790):-hnf(_6221428,_6227778,_6227784,_6227790).
'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'('RadExpr.Mul'(_6222136,_6222154),_6221410,_6221428,_6228802,_6228808,_6228814):-hnf(_6221428,_6228802,_6228808,_6228814).
'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'('RadExpr.Inv'(_6222270),_6221410,_6221428,_6229818,_6229824,_6229830):-hnf(_6221428,_6229818,_6229824,_6229830).
'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'('RadExpr.Root'(_6222380,_6222398),_6221410,_6221428,_6230788,_6230794,_6230800):-hnf(_6221428,_6230788,_6230794,_6230800).
'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'('RadExpr.Pow'(_6222514,_6222532),_6221410,_6221428,_6231812,_6231818,_6231824):-!,hnf(_6221428,_6231812,_6231818,_6231824).
'blocked_blocked_Normalize.applyCoeffMul._\'23caseor0_1'('FAIL'(_6232550),_6221410,_6221428,'FAIL'(_6232550),_6232564,_6232564).

'Normalize.flattenAddN._\'23caseor0'(_6233966,_6233968,_6233970,_6233972,_6233974):-freeze(_6233972,'blocked_Normalize.flattenAddN._\'23caseor0'(_6233966,_6233968,_6233970,_6233972,_6233974)).
'blocked_Normalize.flattenAddN._\'23caseor0'(_6234062,_6234080,_6235578,_6235584,_6235590):-hnf(_6234062,_6237152,_6235584,_6237170),'blocked_Normalize.flattenAddN._\'23caseor0_1'(_6237152,_6234080,_6235578,_6237170,_6235590).

'blocked_Normalize.flattenAddN._\'23caseor0_1'(_6237624,_6237626,_6237628,_6237630,_6237632):-freeze(_6237630,freeze(_6237624,'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'(_6237624,_6237626,_6237628,_6237630,_6237632))).
'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'('RadExpr.Add'(_6234196,_6234214),_6234080,_6238024,_6238030,_6238036):-hnf('Prelude.++'('Normalize.flattenAddN'(_6234196),'Normalize.flattenAddN'(_6234214)),_6238024,_6238030,_6238036).
'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'('RadExpr.Lit'(_6234652),_6234080,_6239764,_6239770,_6239776):-hnf(_6234080,_6239764,_6239770,_6239776).
'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'('RadExpr.Neg'(_6234762),_6234080,_6240628,_6240634,_6240640):-hnf(_6234080,_6240628,_6240634,_6240640).
'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'('RadExpr.Mul'(_6234872,_6234890),_6234080,_6241500,_6241506,_6241512):-hnf(_6234080,_6241500,_6241506,_6241512).
'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'('RadExpr.Inv'(_6235006),_6234080,_6242430,_6242436,_6242442):-hnf(_6234080,_6242430,_6242436,_6242442).
'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'('RadExpr.Root'(_6235116,_6235134),_6234080,_6243314,_6243320,_6243326):-hnf(_6234080,_6243314,_6243320,_6243326).
'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'('RadExpr.Pow'(_6235250,_6235268),_6234080,_6244252,_6244258,_6244264):-!,hnf(_6234080,_6244252,_6244258,_6244264).
'blocked_blocked_Normalize.flattenAddN._\'23caseor0_1'('FAIL'(_6244904),_6234080,'FAIL'(_6244904),_6244918,_6244918).

'Normalize.splitCoeff._\'23caseor0._\'23caseor0'(_6246682,_6246684,_6246686,_6246688,_6246690,_6246692):-freeze(_6246690,'blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0'(_6246682,_6246684,_6246686,_6246688,_6246690,_6246692)).
'blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0'(_6246788,_6246806,_6246824,_6248220,_6248226,_6248232):-hnf(_6246788,_6250198,_6248226,_6250222),'blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'(_6250198,_6246806,_6246824,_6248220,_6250222,_6248232).

'blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'(_6250750,_6250752,_6250754,_6250756,_6250758,_6250760):-freeze(_6250758,freeze(_6250750,'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'(_6250750,_6250752,_6250754,_6250756,_6250758,_6250760))).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6246940),_6246806,_6246824,'Prelude.(,)'(_6246940,_6246806),_6251158,_6251158).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6247204),_6246806,_6246824,_6252404,_6252410,_6252416):-hnf(_6246824,_6252404,_6252410,_6252416).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6247314,_6247332),_6246806,_6246824,_6253416,_6253422,_6253428):-hnf(_6246824,_6253416,_6253422,_6253428).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6247448,_6247466),_6246806,_6246824,_6254494,_6254500,_6254506):-hnf(_6246824,_6254494,_6254500,_6254506).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6247582),_6246806,_6246824,_6255564,_6255570,_6255576):-hnf(_6246824,_6255564,_6255570,_6255576).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6247692,_6247710),_6246806,_6246824,_6256588,_6256594,_6256600):-hnf(_6246824,_6256588,_6256594,_6256600).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6247826,_6247844),_6246806,_6246824,_6257666,_6257672,_6257678):-!,hnf(_6246824,_6257666,_6257672,_6257678).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0._\'23caseor0_1'('FAIL'(_6258458),_6246806,_6246824,'FAIL'(_6258458),_6258472,_6258472).

'Normalize.splitCoeff._\'23caseor0'(_6259836,_6259838,_6259840,_6259842,_6259844,_6259846):-freeze(_6259844,'blocked_Normalize.splitCoeff._\'23caseor0'(_6259836,_6259838,_6259840,_6259842,_6259844,_6259846)).
'blocked_Normalize.splitCoeff._\'23caseor0'(_6259942,_6259960,_6259978,_6263630,_6263636,_6263642):-hnf(_6259942,_6265176,_6263636,_6265200),'blocked_Normalize.splitCoeff._\'23caseor0_1'(_6265176,_6259960,_6259978,_6263630,_6265200,_6263642).

'blocked_Normalize.splitCoeff._\'23caseor0_1'(_6265656,_6265658,_6265660,_6265662,_6265664,_6265666):-freeze(_6265664,freeze(_6265656,'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'(_6265656,_6265658,_6265660,_6265662,_6265664,_6265666))).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'('RadExpr.Mul'(_6260094,_6260112),_6259960,_6259978,_6266066,_6266072,_6266078):-makeShare(_6260152,_6266290),hnf('Prelude.cond'(letrec4PAKCS(_6266290,'Prelude.(,)'('Normalize.rOne',_6259960)),'Normalize.splitCoeff._\'23caseor0._\'23caseor0'(_6260094,_6260112,_6266290)),_6266066,_6266072,_6266078).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'('RadExpr.Neg'(_6260962),_6259960,_6259978,_6268940,_6268946,_6268952):-makeShare(_6260996,_6269278),makeShare(_6261014,_6269298),makeShare(_6261032,_6269318),hnf('Prelude.cond'(letrec4PAKCS(_6269278,'Normalize.splitCoeff'(_6260962)),'Prelude.cond'(letrec4PAKCS(_6269298,'Normalize.splitCoeff._\'23selFP17\'23c'(_6269278)),'Prelude.cond'(letrec4PAKCS(_6269318,'Normalize.splitCoeff._\'23selFP18\'23b'(_6269278)),'Prelude.(,)'('Rational.ratNeg'(_6269298),_6269318)))),_6268940,_6268946,_6268952).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'('RadExpr.Lit'(_6262568),_6259960,_6259978,'Prelude.(,)'(_6262568,'RadExpr.Lit'('Normalize.rOne')),_6273932,_6273932).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'('RadExpr.Add'(_6262930,_6262948),_6259960,_6259978,_6275378,_6275384,_6275390):-hnf(_6259978,_6275378,_6275384,_6275390).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'('RadExpr.Inv'(_6263064),_6259960,_6259978,_6276376,_6276382,_6276388):-hnf(_6259978,_6276376,_6276382,_6276388).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'('RadExpr.Root'(_6263174,_6263192),_6259960,_6259978,_6277328,_6277334,_6277340):-hnf(_6259978,_6277328,_6277334,_6277340).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'('RadExpr.Pow'(_6263308,_6263326),_6259960,_6259978,_6278334,_6278340,_6278346):-!,hnf(_6259978,_6278334,_6278340,_6278346).
'blocked_blocked_Normalize.splitCoeff._\'23caseor0_1'('FAIL'(_6279054),_6259960,_6259978,'FAIL'(_6279054),_6279068,_6279068).

'Normalize.isLitOne._\'23caseor0'(_6280356,_6280358,_6280360,_6280362,_6280364):-freeze(_6280362,'blocked_Normalize.isLitOne._\'23caseor0'(_6280356,_6280358,_6280360,_6280362,_6280364)).
'blocked_Normalize.isLitOne._\'23caseor0'(_6280452,_6280470,_6281796,_6281802,_6281808):-hnf(_6280452,_6283262,_6281802,_6283280),'blocked_Normalize.isLitOne._\'23caseor0_1'(_6283262,_6280470,_6281796,_6283280,_6281808).

'blocked_Normalize.isLitOne._\'23caseor0_1'(_6283716,_6283718,_6283720,_6283722,_6283724):-freeze(_6283722,freeze(_6283716,'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'(_6283716,_6283718,_6283720,_6283722,_6283724))).
'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'('RadExpr.Lit'(_6280586),_6280470,_6284108,_6284114,_6284120):-hnf('Rational.ratEq'(_6280586,'Normalize.rOne'),_6284108,_6284114,_6284120).
'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'('RadExpr.Neg'(_6280864),_6280470,_6285340,_6285346,_6285352):-hnf(_6280470,_6285340,_6285346,_6285352).
'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'('RadExpr.Add'(_6280974,_6280992),_6280470,_6286194,_6286200,_6286206):-hnf(_6280470,_6286194,_6286200,_6286206).
'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'('RadExpr.Mul'(_6281108,_6281126),_6280470,_6287114,_6287120,_6287126):-hnf(_6280470,_6287114,_6287120,_6287126).
'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'('RadExpr.Inv'(_6281242),_6280470,_6288026,_6288032,_6288038):-hnf(_6280470,_6288026,_6288032,_6288038).
'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'('RadExpr.Root'(_6281352,_6281370),_6280470,_6288892,_6288898,_6288904):-hnf(_6280470,_6288892,_6288898,_6288904).
'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'('RadExpr.Pow'(_6281486,_6281504),_6280470,_6289812,_6289818,_6289824):-!,hnf(_6280470,_6289812,_6289818,_6289824).
'blocked_blocked_Normalize.isLitOne._\'23caseor0_1'('FAIL'(_6290446),_6280470,'FAIL'(_6290446),_6290460,_6290460).

'Normalize.flattenAddS._\'23caseor0'(_6291854,_6291856,_6291858,_6291860,_6291862):-freeze(_6291860,'blocked_Normalize.flattenAddS._\'23caseor0'(_6291854,_6291856,_6291858,_6291860,_6291862)).
'blocked_Normalize.flattenAddS._\'23caseor0'(_6291950,_6291968,_6293466,_6293472,_6293478):-hnf(_6291950,_6295040,_6293472,_6295058),'blocked_Normalize.flattenAddS._\'23caseor0_1'(_6295040,_6291968,_6293466,_6295058,_6293478).

'blocked_Normalize.flattenAddS._\'23caseor0_1'(_6295512,_6295514,_6295516,_6295518,_6295520):-freeze(_6295518,freeze(_6295512,'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'(_6295512,_6295514,_6295516,_6295518,_6295520))).
'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'('RadExpr.Add'(_6292084,_6292102),_6291968,_6295912,_6295918,_6295924):-hnf('Prelude.++'('Normalize.flattenAddS'(_6292084),'Normalize.flattenAddS'(_6292102)),_6295912,_6295918,_6295924).
'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'('RadExpr.Lit'(_6292540),_6291968,_6297652,_6297658,_6297664):-hnf(_6291968,_6297652,_6297658,_6297664).
'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'('RadExpr.Neg'(_6292650),_6291968,_6298516,_6298522,_6298528):-hnf(_6291968,_6298516,_6298522,_6298528).
'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'('RadExpr.Mul'(_6292760,_6292778),_6291968,_6299388,_6299394,_6299400):-hnf(_6291968,_6299388,_6299394,_6299400).
'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'('RadExpr.Inv'(_6292894),_6291968,_6300318,_6300324,_6300330):-hnf(_6291968,_6300318,_6300324,_6300330).
'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'('RadExpr.Root'(_6293004,_6293022),_6291968,_6301202,_6301208,_6301214):-hnf(_6291968,_6301202,_6301208,_6301214).
'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'('RadExpr.Pow'(_6293138,_6293156),_6291968,_6302140,_6302146,_6302152):-!,hnf(_6291968,_6302140,_6302146,_6302152).
'blocked_blocked_Normalize.flattenAddS._\'23caseor0_1'('FAIL'(_6302792),_6291968,'FAIL'(_6302792),_6302806,_6302806).

'Normalize.flattenMulS._\'23caseor0'(_6304200,_6304202,_6304204,_6304206,_6304208):-freeze(_6304206,'blocked_Normalize.flattenMulS._\'23caseor0'(_6304200,_6304202,_6304204,_6304206,_6304208)).
'blocked_Normalize.flattenMulS._\'23caseor0'(_6304296,_6304314,_6305812,_6305818,_6305824):-hnf(_6304296,_6307386,_6305818,_6307404),'blocked_Normalize.flattenMulS._\'23caseor0_1'(_6307386,_6304314,_6305812,_6307404,_6305824).

'blocked_Normalize.flattenMulS._\'23caseor0_1'(_6307858,_6307860,_6307862,_6307864,_6307866):-freeze(_6307864,freeze(_6307858,'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'(_6307858,_6307860,_6307862,_6307864,_6307866))).
'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'('RadExpr.Mul'(_6304430,_6304448),_6304314,_6308258,_6308264,_6308270):-hnf('Prelude.++'('Normalize.flattenMulS'(_6304430),'Normalize.flattenMulS'(_6304448)),_6308258,_6308264,_6308270).
'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'('RadExpr.Lit'(_6304886),_6304314,_6309998,_6310004,_6310010):-hnf(_6304314,_6309998,_6310004,_6310010).
'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'('RadExpr.Neg'(_6304996),_6304314,_6310862,_6310868,_6310874):-hnf(_6304314,_6310862,_6310868,_6310874).
'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'('RadExpr.Add'(_6305106,_6305124),_6304314,_6311734,_6311740,_6311746):-hnf(_6304314,_6311734,_6311740,_6311746).
'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'('RadExpr.Inv'(_6305240),_6304314,_6312664,_6312670,_6312676):-hnf(_6304314,_6312664,_6312670,_6312676).
'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'('RadExpr.Root'(_6305350,_6305368),_6304314,_6313548,_6313554,_6313560):-hnf(_6304314,_6313548,_6313554,_6313560).
'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'('RadExpr.Pow'(_6305484,_6305502),_6304314,_6314486,_6314492,_6314498):-!,hnf(_6304314,_6314486,_6314492,_6314498).
'blocked_blocked_Normalize.flattenMulS._\'23caseor0_1'('FAIL'(_6315138),_6304314,'FAIL'(_6315138),_6315152,_6315152).

'Normalize.distribute._\'23caseor0._\'23caseor0'(_6316916,_6316918,_6316920,_6316922,_6316924,_6316926):-freeze(_6316924,'blocked_Normalize.distribute._\'23caseor0._\'23caseor0'(_6316916,_6316918,_6316920,_6316922,_6316924,_6316926)).
'blocked_Normalize.distribute._\'23caseor0._\'23caseor0'(_6317022,_6317040,_6317058,_6318622,_6318628,_6318634):-hnf(_6317022,_6320600,_6318628,_6320624),'blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'(_6320600,_6317040,_6317058,_6318622,_6320624,_6318634).

'blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'(_6321152,_6321154,_6321156,_6321158,_6321160,_6321162):-freeze(_6321160,freeze(_6321152,'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'(_6321152,_6321154,_6321156,_6321158,_6321160,_6321162))).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6317174),_6317040,_6317058,'RadExpr.Neg'('Normalize.distribute'('RadExpr.Mul'(_6317040,_6317174))),_6321560,_6321560).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6317606),_6317040,_6317058,_6323226,_6323232,_6323238):-hnf(_6317058,_6323226,_6323232,_6323238).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6317716,_6317734),_6317040,_6317058,_6324238,_6324244,_6324250):-hnf(_6317058,_6324238,_6324244,_6324250).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6317850,_6317868),_6317040,_6317058,_6325316,_6325322,_6325328):-hnf(_6317058,_6325316,_6325322,_6325328).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6317984),_6317040,_6317058,_6326386,_6326392,_6326398):-hnf(_6317058,_6326386,_6326392,_6326398).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6318094,_6318112),_6317040,_6317058,_6327410,_6327416,_6327422):-hnf(_6317058,_6327410,_6327416,_6327422).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6318228,_6318246),_6317040,_6317058,_6328488,_6328494,_6328500):-!,hnf(_6317058,_6328488,_6328494,_6328500).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0_1'('FAIL'(_6329280),_6317040,_6317058,'FAIL'(_6329280),_6329294,_6329294).

'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0'(_6331474,_6331476,_6331478,_6331480,_6331482,_6331484):-freeze(_6331482,'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0'(_6331474,_6331476,_6331478,_6331480,_6331482,_6331484)).
'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0'(_6331580,_6331598,_6331616,_6333252,_6333258,_6333264):-hnf(_6331580,_6335662,_6333258,_6335686),'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6335662,_6331598,_6331616,_6333252,_6335686,_6333264).

'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6336286,_6336288,_6336290,_6336292,_6336294,_6336296):-freeze(_6336294,freeze(_6336286,'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6336286,_6336288,_6336290,_6336292,_6336294,_6336296))).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6331732),_6331598,_6331616,'RadExpr.Neg'('Normalize.distribute'('RadExpr.Mul'(_6331732,_6331598))),_6336694,_6336694).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6332164),_6331598,_6331616,_6338432,_6338438,_6338444):-hnf(_6331616,_6338432,_6338438,_6338444).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6332274,_6332292),_6331598,_6331616,_6339516,_6339522,_6339528):-hnf(_6331616,_6339516,_6339522,_6339528).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6332408,_6332426),_6331598,_6331616,_6340666,_6340672,_6340678):-hnf(_6331616,_6340666,_6340672,_6340678).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6332542),_6331598,_6331616,_6341808,_6341814,_6341820):-hnf(_6331616,_6341808,_6341814,_6341820).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6332652,_6332670),_6331598,_6331616,_6342904,_6342910,_6342916):-hnf(_6331616,_6342904,_6342910,_6342916).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6332786,_6332804),_6331598,_6331616,_6344054,_6344060,_6344066):-!,hnf(_6331616,_6344054,_6344060,_6344066).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_6344918),_6331598,_6331616,'FAIL'(_6344918),_6344932,_6344932).

'Normalize.distribute._\'23caseor0'(_6346296,_6346298,_6346300,_6346302,_6346304,_6346306):-freeze(_6346304,'blocked_Normalize.distribute._\'23caseor0'(_6346296,_6346298,_6346300,_6346302,_6346304,_6346306)).
'blocked_Normalize.distribute._\'23caseor0'(_6346402,_6346420,_6346438,_6350766,_6350772,_6350778):-hnf(_6346402,_6352312,_6350772,_6352336),'blocked_Normalize.distribute._\'23caseor0_1'(_6352312,_6346420,_6346438,_6350766,_6352336,_6350778).

'blocked_Normalize.distribute._\'23caseor0_1'(_6352792,_6352794,_6352796,_6352798,_6352800,_6352802):-freeze(_6352800,freeze(_6352792,'blocked_blocked_Normalize.distribute._\'23caseor0_1'(_6352792,_6352794,_6352796,_6352798,_6352800,_6352802))).
'blocked_blocked_Normalize.distribute._\'23caseor0_1'('Prelude.True',_6346420,_6346438,_6353196,_6353202,_6353208):-makeShare(_6346564,_6353372),hnf('Prelude.cond'(letrec4PAKCS(_6353372,'Normalize.flattenS'(_6346420)),'Normalize.rebuildAddD'('Prelude.map'(partcall(1,'Normalize.distribute._\'23lambda35',[_6346438]),_6353372))),_6353196,_6353202,_6353208).
'blocked_blocked_Normalize.distribute._\'23caseor0_1'('Prelude.False',_6346420,_6346438,_6359438,_6359444,_6359450):-!,makeShare(_6346420,_6356764),makeShare(_6346438,_6356784),hnf('Prelude.&&'('Prelude.not'('Normalize.isSum'(_6356764)),'Normalize.isTinySum'(_6356784)),_6361866,_6359444,_6361832),'blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase'(_6361866,_6356764,_6356784,_6359438,_6361832,_6359450).

'blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase'(_6362534,_6362536,_6362538,_6362540,_6362542,_6362544):-freeze(_6362542,freeze(_6362534,'blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase'(_6362534,_6362536,_6362538,_6362540,_6362542,_6362544))).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_6356764,_6356784,_6362938,_6362944,_6362950):-makeShare(_6347902,_6363114),hnf('Prelude.cond'(letrec4PAKCS(_6363114,'Normalize.flattenS'(_6356784)),'Normalize.rebuildAddD'('Prelude.map'(partcall(1,'Normalize.distribute._\'23lambda36',[_6356764]),_6363114))),_6362938,_6362944,_6362950).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_6356764,_6356784,_6368282,_6368288,_6368294):-!,makeShare(_6356764,_6366460),makeShare(_6356784,_6366480),hnf('Prelude.(,)'(_6366460,_6366480),_6371934,_6368288,_6371900),'blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6371934,_6366460,_6366480,_6368282,_6371900,_6368294).

'blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6372806,_6372808,_6372810,_6372812,_6372814,_6372816):-freeze(_6372814,freeze(_6372806,'blocked_blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6372806,_6372808,_6372810,_6372812,_6372814,_6372816))).
'blocked_blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.(,)'(_6348978,_6348996),_6366460,_6366480,_6373202,_6373208,_6373214):-!,makeShare(_6349036,_6373684),makeShare(_6349296,_6373704),makeShare(_6366460,_6373724),makeShare(_6366480,_6373744),hnf('Prelude.cond'(letrec4PAKCS(_6373684,'Prelude.cond'(letrec4PAKCS(_6373704,'RadExpr.Mul'('Normalize.distribute'(_6373724),'Normalize.distribute'(_6373744))),'Normalize.distribute._\'23caseor0._\'23caseor0'(_6348996,_6373724,_6373704))),'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0'(_6348978,_6373744,_6373684)),_6373202,_6373208,_6373214).
'blocked_blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6378764),_6366460,_6366480,'FAIL'(_6378764),_6378778,_6378778).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_6378846),_6356764,_6356784,'FAIL'(_6378846),_6378860,_6378860).
'blocked_blocked_Normalize.distribute._\'23caseor0_1'('FAIL'(_6378928),_6346420,_6346438,'FAIL'(_6378928),_6378942,_6378942).

'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6381938,_6381940,_6381942,_6381944,_6381946,_6381948):-freeze(_6381946,'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6381938,_6381940,_6381942,_6381944,_6381946,_6381948)).
'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6382044,_6382062,_6382080,_6383944,_6383950,_6383956):-hnf(_6382044,_6387218,_6383950,_6387242),'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6387218,_6382062,_6382080,_6383944,_6387242,_6383956).

'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6387986,_6387988,_6387990,_6387992,_6387994,_6387996):-freeze(_6387994,freeze(_6387986,'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6387986,_6387988,_6387990,_6387992,_6387994,_6387996))).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6382196),_6382062,_6382080,'RadExpr.Neg'('Normalize.distribute'('RadExpr.Mul'(_6382196,'RadExpr.Lit'(_6382062)))),_6388394,_6388394).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6382712),_6382062,_6382080,_6390452,_6390458,_6390464):-hnf(_6382080,_6390452,_6390458,_6390464).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6382822,_6382840),_6382062,_6382080,_6391680,_6391686,_6391692):-hnf(_6382080,_6391680,_6391686,_6391692).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6382956,_6382974),_6382062,_6382080,_6392974,_6392980,_6392986):-hnf(_6382080,_6392974,_6392980,_6392986).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6383090),_6382062,_6382080,_6394260,_6394266,_6394272):-hnf(_6382080,_6394260,_6394266,_6394272).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6383200,_6383218),_6382062,_6382080,_6395500,_6395506,_6395512):-hnf(_6382080,_6395500,_6395506,_6395512).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6383334,_6383352),_6382062,_6382080,_6396794,_6396800,_6396806):-!,hnf(_6382080,_6396794,_6396800,_6396806).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_6397802),_6382062,_6382080,'FAIL'(_6397802),_6397816,_6397816).

'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6400404,_6400406,_6400408,_6400410,_6400412,_6400414):-freeze(_6400412,'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6400404,_6400406,_6400408,_6400410,_6400412,_6400414)).
'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6400510,_6400528,_6400546,_6403842,_6403848,_6403854):-hnf(_6400510,_6406684,_6403848,_6406708),'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6406684,_6400528,_6400546,_6403842,_6406708,_6403854).

'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6407380,_6407382,_6407384,_6407386,_6407388,_6407390):-freeze(_6407388,freeze(_6407380,'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6407380,_6407382,_6407384,_6407386,_6407388,_6407390))).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6400662),_6400528,_6400546,_6409900,_6409906,_6409912):-makeShare(_6400528,_6408158),hnf('Normalize.isSum'(_6408158),_6413570,_6409906,_6413530),'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6413570,_6400662,_6408158,_6400546,_6409900,_6413530,_6409912).

'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6414414,_6414416,_6414418,_6414420,_6414422,_6414424,_6414426):-freeze(_6414424,freeze(_6414414,'blocked_blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6414414,_6414416,_6414418,_6414420,_6414422,_6414424,_6414426))).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_6400662,_6408158,_6400546,_6414828,_6414834,_6414840):-makeShare(_6400878,_6415012),hnf('Prelude.cond'(letrec4PAKCS(_6415012,'Normalize.flattenS'(_6408158)),'Normalize.rebuildAddD'('Prelude.map'(partcall(1,'Normalize.distribute._\'23lambda33',[_6400662]),_6415012))),_6414828,_6414834,_6414840).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_6400662,_6408158,_6400546,_6418148,_6418154,_6418160):-!,makeShare(_6401712,_6418396),makeShare(_6408158,_6418416),makeShare(_6400662,_6418436),hnf('Prelude.cond'(letrec4PAKCS(_6418396,'RadExpr.Mul'('Normalize.distribute'(_6418416),'RadExpr.Lit'(_6418436))),'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6418416,_6418436,_6418396)),_6418148,_6418154,_6418160).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_6422100),_6400662,_6408158,_6400546,'FAIL'(_6422100),_6422114,_6422114).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6402682),_6400528,_6400546,_6422486,_6422492,_6422498):-hnf(_6400546,_6422486,_6422492,_6422498).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6402792,_6402810),_6400528,_6400546,_6423642,_6423648,_6423654):-hnf(_6400546,_6423642,_6423648,_6423654).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6402926,_6402944),_6400528,_6400546,_6424864,_6424870,_6424876):-hnf(_6400546,_6424864,_6424870,_6424876).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6403060),_6400528,_6400546,_6426078,_6426084,_6426090):-hnf(_6400546,_6426078,_6426084,_6426090).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6403170,_6403188),_6400528,_6400546,_6427246,_6427252,_6427258):-hnf(_6400546,_6427246,_6427252,_6427258).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6403304,_6403322),_6400528,_6400546,_6428468,_6428474,_6428480):-!,hnf(_6400546,_6428468,_6428474,_6428480).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_6429404),_6400528,_6400546,'FAIL'(_6429404),_6429418,_6429418).

'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6433230,_6433232,_6433234,_6433236,_6433238,_6433240):-freeze(_6433238,'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6433230,_6433232,_6433234,_6433236,_6433238,_6433240)).
'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6433336,_6433354,_6433372,_6435380,_6435386,_6435392):-hnf(_6433336,_6439518,_6435386,_6439542),'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6439518,_6433354,_6433372,_6435380,_6439542,_6435392).

'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6440430,_6440432,_6440434,_6440436,_6440438,_6440440):-freeze(_6440438,freeze(_6440430,'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6440430,_6440432,_6440434,_6440436,_6440438,_6440440))).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6433488),_6433354,_6433372,'RadExpr.Neg'('Normalize.distribute'('RadExpr.Mul'('RadExpr.Lit'(_6433354),_6433488))),_6440838,_6440838).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6434004),_6433354,_6433372,_6443040,_6443046,_6443052):-hnf(_6433372,_6443040,_6443046,_6443052).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6434114,_6434132),_6433354,_6433372,_6444412,_6444418,_6444424):-hnf(_6433372,_6444412,_6444418,_6444424).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6434248,_6434266),_6433354,_6433372,_6445850,_6445856,_6445862):-hnf(_6433372,_6445850,_6445856,_6445862).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6434382),_6433354,_6433372,_6447280,_6447286,_6447292):-hnf(_6433372,_6447280,_6447286,_6447292).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6434492,_6434510),_6433354,_6433372,_6448664,_6448670,_6448676):-hnf(_6433372,_6448664,_6448670,_6448676).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6434626,_6434644),_6433354,_6433372,_6450102,_6450108,_6450114):-!,hnf(_6433372,_6450102,_6450108,_6450114).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_6451254),_6433354,_6433372,'FAIL'(_6451254),_6451268,_6451268).

'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6454672,_6454674,_6454676,_6454678,_6454680,_6454682):-freeze(_6454680,'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6454672,_6454674,_6454676,_6454678,_6454680,_6454682)).
'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6454778,_6454796,_6454814,_6458254,_6458260,_6458266):-hnf(_6454778,_6461960,_6458260,_6461984),'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6461960,_6454796,_6454814,_6458254,_6461984,_6458266).

'blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6462800,_6462802,_6462804,_6462806,_6462808,_6462810):-freeze(_6462808,freeze(_6462800,'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_6462800,_6462802,_6462804,_6462806,_6462808,_6462810))).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_6454930),_6454796,_6454814,_6465464,_6465470,_6465476):-makeShare(_6454796,_6463578),hnf('Normalize.isSum'(_6463578),_6469998,_6465470,_6469958),'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6469998,_6454930,_6463578,_6454814,_6465464,_6469958,_6465476).

'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6470986,_6470988,_6470990,_6470992,_6470994,_6470996,_6470998):-freeze(_6470996,freeze(_6470986,'blocked_blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_6470986,_6470988,_6470990,_6470992,_6470994,_6470996,_6470998))).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_6454930,_6463578,_6454814,_6471400,_6471406,_6471412):-makeShare(_6455146,_6471584),hnf('Prelude.cond'(letrec4PAKCS(_6471584,'Normalize.flattenS'(_6463578)),'Normalize.rebuildAddD'('Prelude.map'(partcall(1,'Normalize.distribute._\'23lambda31',[_6454930]),_6471584))),_6471400,_6471406,_6471412).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_6454930,_6463578,_6454814,_6474864,_6474870,_6474876):-!,makeShare(_6455980,_6475112),makeShare(_6454930,_6475132),makeShare(_6463578,_6475152),hnf('Prelude.cond'(letrec4PAKCS(_6475112,'RadExpr.Mul'('RadExpr.Lit'(_6475132),'Normalize.distribute'(_6475152))),'Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_6475152,_6475132,_6475112)),_6474864,_6474870,_6474876).
'blocked_blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_6479104),_6454930,_6463578,_6454814,'FAIL'(_6479104),_6479118,_6479118).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_6456950),_6454796,_6454814,_6479490,_6479496,_6479502):-hnf(_6454814,_6479490,_6479496,_6479502).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_6457060,_6457078),_6454796,_6454814,_6480790,_6480796,_6480802):-hnf(_6454814,_6480790,_6480796,_6480802).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_6457194,_6457212),_6454796,_6454814,_6482156,_6482162,_6482168):-hnf(_6454814,_6482156,_6482162,_6482168).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_6457328),_6454796,_6454814,_6483514,_6483520,_6483526):-hnf(_6454814,_6483514,_6483520,_6483526).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_6457438,_6457456),_6454796,_6454814,_6484826,_6484832,_6484838):-hnf(_6454814,_6484826,_6484832,_6484838).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_6457572,_6457590),_6454796,_6454814,_6486192,_6486198,_6486204):-!,hnf(_6454814,_6486192,_6486198,_6486204).
'blocked_blocked_Normalize.distribute._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_6487272),_6454796,_6454814,'FAIL'(_6487272),_6487286,_6487286).

'Normalize.isSum._\'23caseor0'(_6488460,_6488462,_6488464,_6488466,_6488468):-freeze(_6488466,'blocked_Normalize.isSum._\'23caseor0'(_6488460,_6488462,_6488464,_6488466,_6488468)).
'blocked_Normalize.isSum._\'23caseor0'(_6488556,_6488574,_6489812,_6489818,_6489824):-hnf(_6488556,_6491170,_6489818,_6491188),'blocked_Normalize.isSum._\'23caseor0_1'(_6491170,_6488574,_6489812,_6491188,_6489824).

'blocked_Normalize.isSum._\'23caseor0_1'(_6491606,_6491608,_6491610,_6491612,_6491614):-freeze(_6491612,freeze(_6491606,'blocked_blocked_Normalize.isSum._\'23caseor0_1'(_6491606,_6491608,_6491610,_6491612,_6491614))).
'blocked_blocked_Normalize.isSum._\'23caseor0_1'('RadExpr.Add'(_6488690,_6488708),_6488574,'Prelude.True',_6492012,_6492012).
'blocked_blocked_Normalize.isSum._\'23caseor0_1'('RadExpr.Neg'(_6488838),_6488574,_6492970,_6492976,_6492982):-hnf('Normalize.isSum'(_6488838),_6492970,_6492976,_6492982).
'blocked_blocked_Normalize.isSum._\'23caseor0_1'('RadExpr.Lit'(_6489032),_6488574,_6493998,_6494004,_6494010):-hnf(_6488574,_6493998,_6494004,_6494010).
'blocked_blocked_Normalize.isSum._\'23caseor0_1'('RadExpr.Mul'(_6489142,_6489160),_6488574,_6494834,_6494840,_6494846):-hnf(_6488574,_6494834,_6494840,_6494846).
'blocked_blocked_Normalize.isSum._\'23caseor0_1'('RadExpr.Inv'(_6489276),_6488574,_6495728,_6495734,_6495740):-hnf(_6488574,_6495728,_6495734,_6495740).
'blocked_blocked_Normalize.isSum._\'23caseor0_1'('RadExpr.Root'(_6489386,_6489404),_6488574,_6496576,_6496582,_6496588):-hnf(_6488574,_6496576,_6496582,_6496588).
'blocked_blocked_Normalize.isSum._\'23caseor0_1'('RadExpr.Pow'(_6489520,_6489538),_6488574,_6497478,_6497484,_6497490):-!,hnf(_6488574,_6497478,_6497484,_6497490).
'blocked_blocked_Normalize.isSum._\'23caseor0_1'('FAIL'(_6498094),_6488574,'FAIL'(_6498094),_6498108,_6498108).

'Normalize.flattenS._\'23caseor0'(_6499388,_6499390,_6499392,_6499394,_6499396):-freeze(_6499394,'blocked_Normalize.flattenS._\'23caseor0'(_6499388,_6499390,_6499392,_6499394,_6499396)).
'blocked_Normalize.flattenS._\'23caseor0'(_6499484,_6499502,_6501234,_6501240,_6501246):-hnf(_6499484,_6502700,_6501240,_6502718),'blocked_Normalize.flattenS._\'23caseor0_1'(_6502700,_6499502,_6501234,_6502718,_6501246).

'blocked_Normalize.flattenS._\'23caseor0_1'(_6503154,_6503156,_6503158,_6503160,_6503162):-freeze(_6503160,freeze(_6503154,'blocked_blocked_Normalize.flattenS._\'23caseor0_1'(_6503154,_6503156,_6503158,_6503160,_6503162))).
'blocked_blocked_Normalize.flattenS._\'23caseor0_1'('RadExpr.Add'(_6499618,_6499636),_6499502,_6503554,_6503560,_6503566):-hnf('Prelude.++'('Normalize.flattenS'(_6499618),'Normalize.flattenS'(_6499636)),_6503554,_6503560,_6503566).
'blocked_blocked_Normalize.flattenS._\'23caseor0_1'('RadExpr.Neg'(_6500074),_6499502,_6505240,_6505246,_6505252):-hnf('Prelude.map'('RadExpr.Neg','Normalize.flattenS'(_6500074)),_6505240,_6505246,_6505252).
'blocked_blocked_Normalize.flattenS._\'23caseor0_1'('RadExpr.Lit'(_6500436),_6499502,_6506654,_6506660,_6506666):-hnf(_6499502,_6506654,_6506660,_6506666).
'blocked_blocked_Normalize.flattenS._\'23caseor0_1'('RadExpr.Mul'(_6500546,_6500564),_6499502,_6507508,_6507514,_6507520):-hnf(_6499502,_6507508,_6507514,_6507520).
'blocked_blocked_Normalize.flattenS._\'23caseor0_1'('RadExpr.Inv'(_6500680),_6499502,_6508420,_6508426,_6508432):-hnf(_6499502,_6508420,_6508426,_6508432).
'blocked_blocked_Normalize.flattenS._\'23caseor0_1'('RadExpr.Root'(_6500790,_6500808),_6499502,_6509286,_6509292,_6509298):-hnf(_6499502,_6509286,_6509292,_6509298).
'blocked_blocked_Normalize.flattenS._\'23caseor0_1'('RadExpr.Pow'(_6500924,_6500942),_6499502,_6510206,_6510212,_6510218):-!,hnf(_6499502,_6510206,_6510212,_6510218).
'blocked_blocked_Normalize.flattenS._\'23caseor0_1'('FAIL'(_6510840),_6499502,'FAIL'(_6510840),_6510854,_6510854).

:-costCenters(['']).




%%%%% Number of shared variables: 186

