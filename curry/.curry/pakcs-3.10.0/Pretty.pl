%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').
:-importModule('RadExpr').
:-importModule('Rational').

:-curryModule('Pretty').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Pretty.precAdd','Pretty.precAdd',0,'Pretty.precAdd',nofix,notype).
functiontype('Pretty.precMul','Pretty.precMul',0,'Pretty.precMul',nofix,notype).
functiontype('Pretty.precNeg','Pretty.precNeg',0,'Pretty.precNeg',nofix,notype).
functiontype('Pretty.precPow','Pretty.precPow',0,'Pretty.precPow',nofix,notype).
functiontype('Pretty.pretty',pretty,0,'Pretty.pretty',nofix,notype).
functiontype('Pretty.prettyPrec',prettyPrec,2,'Pretty.prettyPrec',nofix,notype).
functiontype('Pretty.prettyNeg','Pretty.prettyNeg',2,'Pretty.prettyNeg',nofix,notype).
functiontype('Pretty.negTerm','Pretty.negTerm',1,'Pretty.negTerm',nofix,notype).
functiontype('Pretty.prettyRadicand','Pretty.prettyRadicand',1,'Pretty.prettyRadicand',nofix,notype).
functiontype('Pretty.flattenAddBasic','Pretty.flattenAddBasic',1,'Pretty.flattenAddBasic',nofix,notype).
functiontype('Pretty.flattenMulBasic','Pretty.flattenMulBasic',1,'Pretty.flattenMulBasic',nofix,notype).
functiontype('Pretty.rebuildMul','Pretty.rebuildMul',1,'Pretty.rebuildMul',nofix,notype).
functiontype('Pretty.renderTermsBasic','Pretty.renderTermsBasic',1,'Pretty.renderTermsBasic',nofix,notype).
functiontype('Pretty.renderRest','Pretty.renderRest',1,'Pretty.renderRest',nofix,notype).
functiontype('Pretty.renderFactorsBasic','Pretty.renderFactorsBasic',1,'Pretty.renderFactorsBasic',nofix,notype).
functiontype('Pretty.prettyRat','Pretty.prettyRat',1,'Pretty.prettyRat',nofix,notype).
functiontype('Pretty.absInt','Pretty.absInt',1,'Pretty.absInt',nofix,notype).
functiontype('Pretty.parensIf','Pretty.parensIf',2,'Pretty.parensIf',nofix,notype).
functiontype('Pretty.joinWith','Pretty.joinWith',2,'Pretty.joinWith',nofix,notype).
functiontype('Pretty.prettyPrec._\'23caseor0','Pretty.prettyPrec._#caseor0',4,'Pretty.prettyPrec._\'23caseor0',nofix,notype).
functiontype('Pretty.prettyPrec._\'23caseor0._\'23caseor0','Pretty.prettyPrec._#caseor0._#caseor0',2,'Pretty.prettyPrec._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Pretty.prettyNeg._\'23caseor0._\'23caseor0','Pretty.prettyNeg._#caseor0._#caseor0',4,'Pretty.prettyNeg._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Pretty.prettyNeg._\'23caseor0','Pretty.prettyNeg._#caseor0',4,'Pretty.prettyNeg._\'23caseor0',nofix,notype).
functiontype('Pretty.prettyRadicand._\'23caseor0','Pretty.prettyRadicand._#caseor0',3,'Pretty.prettyRadicand._\'23caseor0',nofix,notype).
functiontype('Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0','Pretty.flattenAddBasic._#caseor0._#caseor0._#caseor0',4,'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Pretty.flattenAddBasic._\'23caseor0._\'23caseor0','Pretty.flattenAddBasic._#caseor0._#caseor0',2,'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','Pretty.flattenAddBasic._#caseor0._#caseor0._#caseor0._#caseor0',3,'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Pretty.flattenAddBasic._\'23caseor0','Pretty.flattenAddBasic._#caseor0',3,'Pretty.flattenAddBasic._\'23caseor0',nofix,notype).
functiontype('Pretty.flattenMulBasic._\'23caseor0','Pretty.flattenMulBasic._#caseor0',2,'Pretty.flattenMulBasic._\'23caseor0',nofix,notype).
functiontype('Pretty.renderTermsBasic._\'23caseor0','Pretty.renderTermsBasic._#caseor0',2,'Pretty.renderTermsBasic._\'23caseor0',nofix,notype).
functiontype('Pretty.renderFactorsBasic._\'23caseor0','Pretty.renderFactorsBasic._#caseor0',3,'Pretty.renderFactorsBasic._\'23caseor0',nofix,notype).
functiontype('Pretty.prettyRat._\'23caseor0','Pretty.prettyRat._#caseor0',3,'Pretty.prettyRat._\'23caseor0',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Pretty.precAdd'(_7676830,_7676832,_7676834):-freeze(_7676832,'blocked_Pretty.precAdd'(_7676830,_7676832,_7676834)).
'blocked_Pretty.precAdd'(1,_7676910,_7676910).

'Pretty.precMul'(_7677780,_7677782,_7677784):-freeze(_7677782,'blocked_Pretty.precMul'(_7677780,_7677782,_7677784)).
'blocked_Pretty.precMul'(2,_7677860,_7677860).

'Pretty.precNeg'(_7678730,_7678732,_7678734):-freeze(_7678732,'blocked_Pretty.precNeg'(_7678730,_7678732,_7678734)).
'blocked_Pretty.precNeg'(3,_7678810,_7678810).

'Pretty.precPow'(_7679680,_7679682,_7679684):-freeze(_7679682,'blocked_Pretty.precPow'(_7679680,_7679682,_7679684)).
'blocked_Pretty.precPow'(4,_7679760,_7679760).

'Pretty.pretty'(_7680592,_7680594,_7680596):-freeze(_7680594,'blocked_Pretty.pretty'(_7680592,_7680594,_7680596)).
'blocked_Pretty.pretty'(_7680750,_7680756,_7680762):-hnf(partcall(1,'Pretty.prettyPrec',[0]),_7680750,_7680756,_7680762).

'Pretty.prettyPrec'(_7681970,_7681972,_7681974,_7681976,_7681978):-freeze(_7681976,'blocked_Pretty.prettyPrec'(_7681970,_7681972,_7681974,_7681976,_7681978)).
'blocked_Pretty.prettyPrec'(_7682066,_7682084,_7697028,_7697034,_7697040):-makeShare(_7682084,_7693420),hnf(_7693420,_7698026,_7697034,_7698050),'blocked_Pretty.prettyPrec_2'(_7698026,_7682066,_7698026,_7697028,_7698050,_7697040).

'blocked_Pretty.prettyPrec_2'(_7698428,_7698430,_7698432,_7698434,_7698436,_7698438):-freeze(_7698436,freeze(_7698428,'blocked_blocked_Pretty.prettyPrec_2'(_7698428,_7698430,_7698432,_7698434,_7698436,_7698438))).
'blocked_blocked_Pretty.prettyPrec_2'('RadExpr.Lit'(_7682200),_7682066,_7693420,_7698830,_7698836,_7698842):-hnf('Pretty.prettyRat'(_7682200),_7698830,_7698836,_7698842).
'blocked_blocked_Pretty.prettyPrec_2'('RadExpr.Neg'(_7682394),_7682066,_7693420,_7699878,_7699884,_7699890):-hnf('Pretty.prettyNeg'(_7682066,_7682394),_7699878,_7699884,_7699890).
'blocked_blocked_Pretty.prettyPrec_2'('RadExpr.Add'(_7682658,_7682676),_7682066,_7693420,_7701038,_7701044,_7701050):-hnf('Pretty.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7682066),'Pretty.precAdd'),'Pretty.renderTermsBasic'('Pretty.flattenAddBasic'(_7693420))),_7701038,_7701044,_7701050).
'blocked_blocked_Pretty.prettyPrec_2'('RadExpr.Mul'(_7683450,_7683468),_7682066,_7693420,_7703728,_7703734,_7703740):-makeShare(_7683508,_7704050),makeShare(_7682066,_7704070),hnf('Prelude.cond'(letrec4PAKCS(_7704050,'Pretty.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7704070),'Pretty.precMul'),'Pretty.renderFactorsBasic'('Pretty.flattenMulBasic'(_7693420)))),'Pretty.prettyPrec._\'23caseor0'(_7683468,_7704070,_7683450,_7704050)),_7703728,_7703734,_7703740).
'blocked_blocked_Pretty.prettyPrec_2'('RadExpr.Inv'(_7684878),_7682066,_7693420,_7708232,_7708238,_7708244):-hnf('Pretty.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7682066),'Pretty.precMul'),'Prelude.++'(['^1',^/],'Pretty.prettyPrec'('Pretty.precPow',_7684878))),_7708232,_7708238,_7708244).
'blocked_blocked_Pretty.prettyPrec_2'('RadExpr.Root'(_7686122,_7686140),_7682066,_7693420,_7714038,_7714044,_7714050):-makeShare(_7686122,_7711878),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7711878,2),_7715908,_7714044,_7715862),'blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase'(_7715908,_7711878,_7686140,_7682066,_7693420,_7714038,_7715862,_7714050).

'blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase'(_7716454,_7716456,_7716458,_7716460,_7716462,_7716464,_7716466,_7716468):-freeze(_7716466,freeze(_7716454,'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase'(_7716454,_7716456,_7716458,_7716460,_7716462,_7716464,_7716466,_7716468))).
'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase'('Prelude.True',_7711878,_7686140,_7682066,_7693420,_7716878,_7716884,_7716890):-makeShare(_7686432,_7717074),makeShare(_7686140,_7717094),hnf('Prelude.cond'(letrec4PAKCS(_7717074,'Prelude.++'(['^s','^q','^r','^t'],'Pretty.prettyRadicand'(_7717094))),'Pretty.prettyPrec._\'23caseor0._\'23caseor0'(_7717094,_7717074)),_7716878,_7716884,_7716890).
'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase'('Prelude.False',_7711878,_7686140,_7682066,_7693420,_7723024,_7723030,_7723036):-!,makeShare(_7711878,_7721152),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7721152,3),_7726128,_7723030,_7726082),'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase_Prelude.False_ComplexCase'(_7726128,_7721152,_7686140,_7682066,_7693420,_7723024,_7726082,_7723036).

'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase_Prelude.False_ComplexCase'(_7726896,_7726898,_7726900,_7726902,_7726904,_7726906,_7726908,_7726910):-freeze(_7726908,freeze(_7726896,'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase_Prelude.False_ComplexCase'(_7726896,_7726898,_7726900,_7726902,_7726904,_7726906,_7726908,_7726910))).
'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_7721152,_7686140,_7682066,_7693420,_7727320,_7727326,_7727332):-hnf('Prelude.++'(['^c','^b','^r','^t'],'Pretty.prettyRadicand'(_7686140)),_7727320,_7727326,_7727332).
'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_7721152,_7686140,_7682066,_7693420,_7730042,_7730048,_7730054):-!,hnf('Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_7721152),'Prelude.++'(['^r','^t'],'Pretty.prettyRadicand'(_7686140))),_7730042,_7730048,_7730054).
'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_7733018),_7721152,_7686140,_7682066,_7693420,'FAIL'(_7733018),_7733032,_7733032).
'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Root_ComplexCase'('FAIL'(_7733116),_7711878,_7686140,_7682066,_7693420,'FAIL'(_7733116),_7733130,_7733130).
'blocked_blocked_Pretty.prettyPrec_2'('RadExpr.Pow'(_7690058,_7690076),_7682066,_7693420,_7736022,_7736028,_7736034):-!,makeShare(_7690076,_7733872),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7733872,0),_7737862,_7736028,_7737816),'blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase'(_7737862,_7690058,_7733872,_7682066,_7693420,_7736022,_7737816,_7736034).

'blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase'(_7738420,_7738422,_7738424,_7738426,_7738428,_7738430,_7738432,_7738434):-freeze(_7738432,freeze(_7738420,'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase'(_7738420,_7738422,_7738424,_7738426,_7738428,_7738430,_7738432,_7738434))).
'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase'('Prelude.True',_7690058,_7733872,_7682066,_7693420,['^1'],_7738850,_7738850).
'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase'('Prelude.False',_7690058,_7733872,_7682066,_7693420,_7742948,_7742954,_7742960):-!,makeShare(_7733872,_7740498),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_7740498),0),_7746016,_7742954,_7745970),'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'(_7746016,_7690058,_7740498,_7682066,_7693420,_7742948,_7745970,_7742960).

'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'(_7746778,_7746780,_7746782,_7746784,_7746786,_7746788,_7746790,_7746792):-freeze(_7746790,freeze(_7746778,'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'(_7746778,_7746780,_7746782,_7746784,_7746786,_7746788,_7746790,_7746792))).
'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_7690058,_7740498,_7682066,_7693420,_7747202,_7747208,_7747214):-hnf('Pretty.prettyPrec'(_7682066,'RadExpr.Inv'('RadExpr.Pow'(_7690058,'Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_7740498)))),_7747202,_7747208,_7747214).
'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_7690058,_7740498,_7682066,_7693420,_7751996,_7752002,_7752008):-!,makeShare(_7740498,_7749970),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7749970,1),_7756288,_7752002,_7756242),'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7756288,_7690058,_7749970,_7682066,_7693420,_7751996,_7756242,_7752008).

'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7757254,_7757256,_7757258,_7757260,_7757262,_7757264,_7757266,_7757268):-freeze(_7757266,freeze(_7757254,'blocked_blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7757254,_7757256,_7757258,_7757260,_7757262,_7757264,_7757266,_7757268))).
'blocked_blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_7690058,_7749970,_7682066,_7693420,_7757678,_7757684,_7757690):-hnf('Pretty.prettyPrec'(_7682066,_7690058),_7757678,_7757684,_7757690).
'blocked_blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_7690058,_7749970,_7682066,_7693420,_7759532,_7759538,_7759544):-!,hnf('Pretty.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7682066),'Pretty.precPow'),'Prelude.++'('Pretty.prettyPrec'('Pretty.precPow',_7690058),'Prelude.++'([^^],'Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_7749970)))),_7759532,_7759538,_7759544).
'blocked_blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_7763968),_7690058,_7749970,_7682066,_7693420,'FAIL'(_7763968),_7763982,_7763982).
'blocked_blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_7764066),_7690058,_7740498,_7682066,_7693420,'FAIL'(_7764066),_7764080,_7764080).
'blocked_blocked_blocked_Pretty.prettyPrec_2_RadExpr.Pow_ComplexCase'('FAIL'(_7764164),_7690058,_7733872,_7682066,_7693420,'FAIL'(_7764164),_7764178,_7764178).
'blocked_blocked_Pretty.prettyPrec_2'('FAIL'(_7764262),_7682066,_7693420,'FAIL'(_7764262),_7764276,_7764276).

'Pretty.prettyNeg'(_7765080,_7765082,_7765084,_7765086,_7765088):-freeze(_7765086,'blocked_Pretty.prettyNeg'(_7765080,_7765082,_7765084,_7765086,_7765088)).
'blocked_Pretty.prettyNeg'(_7765176,_7765194,_7766826,_7766832,_7766838):-makeShare(_7765222,_7767112),makeShare(_7765176,_7767132),makeShare(_7765194,_7767152),hnf('Prelude.cond'(letrec4PAKCS(_7767112,'Pretty.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7767132),'Pretty.precNeg'),'Prelude.++'([^-],'Pretty.prettyPrec'('Pretty.precNeg',_7767152)))),'Pretty.prettyNeg._\'23caseor0'(_7767152,_7767152,_7767132,_7767112)),_7766826,_7766832,_7766838).

'Pretty.negTerm'(_7772114,_7772116,_7772118,_7772120):-freeze(_7772118,'blocked_Pretty.negTerm'(_7772114,_7772116,_7772118,_7772120)).
'blocked_Pretty.negTerm'(_7772200,_7772768,_7772774,_7772780):-hnf(_7772200,_7773650,_7772774,_7773662),'blocked_Pretty.negTerm_1'(_7773650,_7772768,_7773662,_7772780).

'blocked_Pretty.negTerm_1'(_7773988,_7773990,_7773992,_7773994):-freeze(_7773992,'blocked_blocked_Pretty.negTerm_1'(_7773988,_7773990,_7773992,_7773994)).
'blocked_blocked_Pretty.negTerm_1'('Prelude.(,)'(_7772316,_7772334),'Prelude.(,)'('Prelude.not'(_7772316),_7772334),_7774364,_7774364):-!.
'blocked_blocked_Pretty.negTerm_1'('FAIL'(_7775240),'FAIL'(_7775240),_7775254,_7775254):-nonvar(_7775240).

'Pretty.prettyRadicand'(_7776236,_7776238,_7776240,_7776242):-freeze(_7776240,'blocked_Pretty.prettyRadicand'(_7776236,_7776238,_7776240,_7776242)).
'blocked_Pretty.prettyRadicand'(_7776322,_7777716,_7777722,_7777728):-makeShare(_7776350,_7777904),makeShare(_7776322,_7777924),hnf('Prelude.cond'(letrec4PAKCS(_7777904,'Prelude.++'(['^('],'Prelude.++'('Prelude.apply'('Pretty.pretty',_7777924),['^)']))),'Pretty.prettyRadicand._\'23caseor0'(_7777924,_7777924,_7777904)),_7777716,_7777722,_7777728).

'Pretty.flattenAddBasic'(_7782136,_7782138,_7782140,_7782142):-freeze(_7782140,'blocked_Pretty.flattenAddBasic'(_7782136,_7782138,_7782140,_7782142)).
'blocked_Pretty.flattenAddBasic'(_7782222,_7783140,_7783146,_7783152):-makeShare(_7782250,_7783316),makeShare(_7782222,_7783336),hnf('Prelude.cond'(letrec4PAKCS(_7783316,['Prelude.(,)'('Prelude.True',_7783336)]),'Pretty.flattenAddBasic._\'23caseor0'(_7783336,_7783336,_7783316)),_7783140,_7783146,_7783152).

'Pretty.flattenMulBasic'(_7786674,_7786676,_7786678,_7786680):-freeze(_7786678,'blocked_Pretty.flattenMulBasic'(_7786674,_7786676,_7786678,_7786680)).
'blocked_Pretty.flattenMulBasic'(_7786760,_7787440,_7787446,_7787452):-makeShare(_7786788,_7787586),makeShare(_7786760,_7787606),hnf('Prelude.cond'(letrec4PAKCS(_7787586,[_7787606]),'Pretty.flattenMulBasic._\'23caseor0'(_7787606,_7787586)),_7787440,_7787446,_7787452).

'Pretty.rebuildMul'(_7790338,_7790340,_7790342,_7790344):-freeze(_7790342,'blocked_Pretty.rebuildMul'(_7790338,_7790340,_7790342,_7790344)).
'blocked_Pretty.rebuildMul'(_7790424,_7791502,_7791508,_7791514):-hnf(_7790424,_7792492,_7791508,_7792504),'blocked_Pretty.rebuildMul_1'(_7792492,_7791502,_7792504,_7791514).

'blocked_Pretty.rebuildMul_1'(_7792854,_7792856,_7792858,_7792860):-freeze(_7792858,freeze(_7792854,'blocked_blocked_Pretty.rebuildMul_1'(_7792854,_7792856,_7792858,_7792860))).
'blocked_blocked_Pretty.rebuildMul_1'([],'RadExpr.Lit'('Rational.fromInt'(1)),_7793108,_7793108).
'blocked_blocked_Pretty.rebuildMul_1'([_7790794|_7790812],_7794790,_7794796,_7794802):-!,makeShare(_7790812,_7793986),hnf(_7793986,_7796260,_7794796,_7796284),'blocked_blocked_Pretty.rebuildMul_1_[|]_2'(_7796260,_7790794,_7796260,_7794790,_7796284,_7794802).

'blocked_blocked_Pretty.rebuildMul_1_[|]_2'(_7796764,_7796766,_7796768,_7796770,_7796772,_7796774):-freeze(_7796772,freeze(_7796764,'blocked_blocked_blocked_Pretty.rebuildMul_1_[|]_2'(_7796764,_7796766,_7796768,_7796770,_7796772,_7796774))).
'blocked_blocked_blocked_Pretty.rebuildMul_1_[|]_2'([],_7790794,_7793986,_7797032,_7797038,_7797044):-hnf(_7790794,_7797032,_7797038,_7797044).
'blocked_blocked_blocked_Pretty.rebuildMul_1_[|]_2'([_7791026|_7791044],_7790794,_7793986,_7797786,_7797792,_7797798):-!,hnf('Prelude.foldl'('RadExpr.Mul',_7790794,_7793986),_7797786,_7797792,_7797798).
'blocked_blocked_blocked_Pretty.rebuildMul_1_[|]_2'('FAIL'(_7798966),_7790794,_7793986,'FAIL'(_7798966),_7798980,_7798980).
'blocked_blocked_Pretty.rebuildMul_1'('FAIL'(_7799048),'FAIL'(_7799048),_7799062,_7799062).

'Pretty.renderTermsBasic'(_7800116,_7800118,_7800120,_7800122):-freeze(_7800120,'blocked_Pretty.renderTermsBasic'(_7800116,_7800118,_7800120,_7800122)).
'blocked_Pretty.renderTermsBasic'(_7800202,_7801894,_7801900,_7801906):-hnf(_7800202,_7803100,_7801900,_7803112),'blocked_Pretty.renderTermsBasic_1'(_7803100,_7801894,_7803112,_7801906).

'blocked_Pretty.renderTermsBasic_1'(_7803498,_7803500,_7803502,_7803504):-freeze(_7803502,freeze(_7803498,'blocked_blocked_Pretty.renderTermsBasic_1'(_7803498,_7803500,_7803502,_7803504))).
'blocked_blocked_Pretty.renderTermsBasic_1'([],['^0'],_7803752,_7803752).
'blocked_blocked_Pretty.renderTermsBasic_1'([_7800572|_7800590],_7804776,_7804782,_7804788):-!,hnf(_7800572,_7806462,_7804782,_7806480),'blocked_blocked_Pretty.renderTermsBasic_1_[|]_1'(_7806462,_7800590,_7804776,_7806480,_7804788).

'blocked_blocked_Pretty.renderTermsBasic_1_[|]_1'(_7806976,_7806978,_7806980,_7806982,_7806984):-freeze(_7806982,freeze(_7806976,'blocked_blocked_blocked_Pretty.renderTermsBasic_1_[|]_1'(_7806976,_7806978,_7806980,_7806982,_7806984))).
'blocked_blocked_blocked_Pretty.renderTermsBasic_1_[|]_1'('Prelude.(,)'(_7800718,_7800736),_7800590,_7807362,_7807368,_7807374):-!,makeShare(_7800776,_7807582),hnf('Prelude.cond'(letrec4PAKCS(_7807582,'Pretty.renderTermsBasic._\'23caseor0'(_7800718,_7800736)),'Prelude.++'(_7807582,'Prelude.apply'('Prelude.concatMap'(partcall(1,'Pretty.renderRest',[])),_7800590))),_7807362,_7807368,_7807374).
'blocked_blocked_blocked_Pretty.renderTermsBasic_1_[|]_1'('FAIL'(_7810348),_7800590,'FAIL'(_7810348),_7810362,_7810362).
'blocked_blocked_Pretty.renderTermsBasic_1'('FAIL'(_7810422),'FAIL'(_7810422),_7810436,_7810436).

'Pretty.renderRest'(_7811262,_7811264,_7811266,_7811268):-freeze(_7811266,'blocked_Pretty.renderRest'(_7811262,_7811264,_7811266,_7811268)).
'blocked_Pretty.renderRest'(_7811348,_7813482,_7813488,_7813494):-hnf(_7811348,_7814472,_7813488,_7814484),'blocked_Pretty.renderRest_1'(_7814472,_7813482,_7814484,_7813494).

'blocked_Pretty.renderRest_1'(_7814828,_7814830,_7814832,_7814834):-freeze(_7814832,'blocked_blocked_Pretty.renderRest_1'(_7814828,_7814830,_7814832,_7814834)).
'blocked_blocked_Pretty.renderRest_1'('Prelude.(,)'(_7811464,_7811482),_7815438,_7815444,_7815450):-!,hnf(_7811464,_7817148,_7815444,_7817166),'blocked_blocked_Pretty.renderRest_1_Prelude.(,)_1'(_7817148,_7811482,_7815438,_7817166,_7815450).

'blocked_blocked_Pretty.renderRest_1_Prelude.(,)_1'(_7817674,_7817676,_7817678,_7817680,_7817682):-freeze(_7817680,freeze(_7817674,'blocked_blocked_blocked_Pretty.renderRest_1_Prelude.(,)_1'(_7817674,_7817676,_7817678,_7817680,_7817682))).
'blocked_blocked_blocked_Pretty.renderRest_1_Prelude.(,)_1'('Prelude.True',_7811482,_7818068,_7818074,_7818080):-hnf('Prelude.++'(['^ ',^+,'^ '],'Pretty.prettyPrec'('Pretty.precAdd',_7811482)),_7818068,_7818074,_7818080).
'blocked_blocked_blocked_Pretty.renderRest_1_Prelude.(,)_1'('Prelude.False',_7811482,_7820264,_7820270,_7820276):-!,hnf('Prelude.++'(['^ ',^-,'^ '],'Pretty.prettyPrec'('Pretty.precMul',_7811482)),_7820264,_7820270,_7820276).
'blocked_blocked_blocked_Pretty.renderRest_1_Prelude.(,)_1'('FAIL'(_7822168),_7811482,'FAIL'(_7822168),_7822182,_7822182).
'blocked_blocked_Pretty.renderRest_1'('FAIL'(_7822242),'FAIL'(_7822242),_7822256,_7822256):-nonvar(_7822242).

'Pretty.renderFactorsBasic'(_7823390,_7823392,_7823394,_7823396):-freeze(_7823394,'blocked_Pretty.renderFactorsBasic'(_7823390,_7823392,_7823394,_7823396)).
'blocked_Pretty.renderFactorsBasic'(_7823476,_7826618,_7826624,_7826630):-makeShare(_7823476,_7825446),hnf(_7825446,_7827896,_7826624,_7827914),'blocked_Pretty.renderFactorsBasic_1'(_7827896,_7827896,_7826618,_7827914,_7826630).

'blocked_Pretty.renderFactorsBasic_1'(_7828332,_7828334,_7828336,_7828338,_7828340):-freeze(_7828338,freeze(_7828332,'blocked_blocked_Pretty.renderFactorsBasic_1'(_7828332,_7828334,_7828336,_7828338,_7828340))).
'blocked_blocked_Pretty.renderFactorsBasic_1'([],_7825446,['^1'],_7828596,_7828596).
'blocked_blocked_Pretty.renderFactorsBasic_1'([_7823846|_7823864],_7825446,_7830606,_7830612,_7830618):-!,makeShare(_7823864,_7829440),hnf(_7829440,_7832372,_7830612,_7832402),'blocked_blocked_Pretty.renderFactorsBasic_1_[|]_2'(_7832372,_7823846,_7832372,_7825446,_7830606,_7832402,_7830618).

'blocked_blocked_Pretty.renderFactorsBasic_1_[|]_2'(_7832938,_7832940,_7832942,_7832944,_7832946,_7832948,_7832950):-freeze(_7832948,freeze(_7832938,'blocked_blocked_blocked_Pretty.renderFactorsBasic_1_[|]_2'(_7832938,_7832940,_7832942,_7832944,_7832946,_7832948,_7832950))).
'blocked_blocked_blocked_Pretty.renderFactorsBasic_1_[|]_2'([],_7823846,_7829440,_7825446,_7833216,_7833222,_7833228):-hnf('Pretty.prettyPrec'('Pretty.precMul',_7823846),_7833216,_7833222,_7833228).
'blocked_blocked_blocked_Pretty.renderFactorsBasic_1_[|]_2'([_7824246|_7824264],_7823846,_7829440,_7825446,_7834496,_7834502,_7834508):-!,makeShare(_7824304,_7834738),hnf('Prelude.cond'(letrec4PAKCS(_7834738,'Pretty.joinWith'([^*],'Prelude.map'(partcall(1,'Pretty.prettyPrec',['Pretty.precPow']),_7825446))),'Pretty.renderFactorsBasic._\'23caseor0'(_7823846,_7829440,_7834738)),_7834496,_7834502,_7834508).
'blocked_blocked_blocked_Pretty.renderFactorsBasic_1_[|]_2'('FAIL'(_7837910),_7823846,_7829440,_7825446,'FAIL'(_7837910),_7837924,_7837924).
'blocked_blocked_Pretty.renderFactorsBasic_1'('FAIL'(_7838000),_7825446,'FAIL'(_7838000),_7838014,_7838014).

'Pretty.prettyRat'(_7838810,_7838812,_7838814,_7838816):-freeze(_7838814,'blocked_Pretty.prettyRat'(_7838810,_7838812,_7838814,_7838816)).
'blocked_Pretty.prettyRat'(_7838896,_7840132,_7840138,_7840144):-makeShare(_7838924,_7840400),makeShare(_7838896,_7840420),makeShare(_7838942,_7840440),hnf('Prelude.cond'(letrec4PAKCS(_7840400,'Rational.numerator'(_7840420)),'Prelude.cond'(letrec4PAKCS(_7840440,'Rational.denominator'(_7840420)),'Pretty.prettyRat._\'23caseor0'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7840440,1),_7840400,_7840440))),_7840132,_7840138,_7840144).

'Pretty.absInt'(_7844560,_7844562,_7844564,_7844566):-freeze(_7844564,'blocked_Pretty.absInt'(_7844560,_7844562,_7844564,_7844566)).
'blocked_Pretty.absInt'(_7844646,_7846822,_7846828,_7846834):-makeShare(_7844646,_7845338),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_7845338),0),_7847764,_7846828,_7847736),'blocked_Pretty.absInt_ComplexCase'(_7847764,_7845338,_7846822,_7847736,_7846834).

'blocked_Pretty.absInt_ComplexCase'(_7848142,_7848144,_7848146,_7848148,_7848150):-freeze(_7848148,freeze(_7848142,'blocked_blocked_Pretty.absInt_ComplexCase'(_7848142,_7848144,_7848146,_7848148,_7848150))).
'blocked_blocked_Pretty.absInt_ComplexCase'('Prelude.True',_7845338,_7848536,_7848542,_7848548):-hnf('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_7845338),_7848536,_7848542,_7848548).
'blocked_blocked_Pretty.absInt_ComplexCase'('Prelude.False',_7845338,_7849720,_7849726,_7849732):-!,hnf(_7845338,_7849720,_7849726,_7849732).
'blocked_blocked_Pretty.absInt_ComplexCase'('FAIL'(_7850180),_7845338,'FAIL'(_7850180),_7850194,_7850194).

'Pretty.parensIf'(_7850952,_7850954,_7850956,_7850958,_7850960):-freeze(_7850958,'blocked_Pretty.parensIf'(_7850952,_7850954,_7850956,_7850958,_7850960)).
'blocked_Pretty.parensIf'(_7851048,_7851066,_7852084,_7852090,_7852096):-hnf(_7851048,_7853010,_7852090,_7853028),'blocked_Pretty.parensIf_1'(_7853010,_7851066,_7852084,_7853028,_7852096).

'blocked_Pretty.parensIf_1'(_7853374,_7853376,_7853378,_7853380,_7853382):-freeze(_7853380,freeze(_7853374,'blocked_blocked_Pretty.parensIf_1'(_7853374,_7853376,_7853378,_7853380,_7853382))).
'blocked_blocked_Pretty.parensIf_1'('Prelude.True',_7851066,_7853768,_7853774,_7853780):-hnf('Prelude.++'(['^('],'Prelude.++'(_7851066,['^)'])),_7853768,_7853774,_7853780).
'blocked_blocked_Pretty.parensIf_1'('Prelude.False',_7851066,_7855552,_7855558,_7855564):-!,hnf(_7851066,_7855552,_7855558,_7855564).
'blocked_blocked_Pretty.parensIf_1'('FAIL'(_7855964),_7851066,'FAIL'(_7855964),_7855978,_7855978).

'Pretty.joinWith'(_7856736,_7856738,_7856740,_7856742,_7856744):-freeze(_7856742,'blocked_Pretty.joinWith'(_7856736,_7856738,_7856740,_7856742,_7856744)).
'blocked_Pretty.joinWith'(_7856832,_7856850,_7858008,_7858014,_7858020):-hnf(_7856850,_7858934,_7858014,_7858952),'blocked_Pretty.joinWith_2'(_7858934,_7856832,_7858008,_7858952,_7858020).

'blocked_Pretty.joinWith_2'(_7859298,_7859300,_7859302,_7859304,_7859306):-freeze(_7859304,freeze(_7859298,'blocked_blocked_Pretty.joinWith_2'(_7859298,_7859300,_7859302,_7859304,_7859306))).
'blocked_blocked_Pretty.joinWith_2'([],_7856832,[],_7859562,_7859562).
'blocked_blocked_Pretty.joinWith_2'([_7857066|_7857084],_7856832,_7861040,_7861046,_7861052):-!,makeShare(_7857084,_7860144),hnf(_7860144,_7862446,_7861046,_7862476),'blocked_blocked_Pretty.joinWith_2_[|]_2'(_7862446,_7857066,_7862446,_7856832,_7861040,_7862476,_7861052).

'blocked_blocked_Pretty.joinWith_2_[|]_2'(_7862952,_7862954,_7862956,_7862958,_7862960,_7862962,_7862964):-freeze(_7862962,freeze(_7862952,'blocked_blocked_blocked_Pretty.joinWith_2_[|]_2'(_7862952,_7862954,_7862956,_7862958,_7862960,_7862962,_7862964))).
'blocked_blocked_blocked_Pretty.joinWith_2_[|]_2'([],_7857066,_7860144,_7856832,_7863230,_7863236,_7863242):-hnf(_7857066,_7863230,_7863236,_7863242).
'blocked_blocked_blocked_Pretty.joinWith_2_[|]_2'([_7857298|_7857316],_7857066,_7860144,_7856832,_7864046,_7864052,_7864058):-!,hnf('Prelude.++'(_7857066,'Prelude.apply'('Prelude.concatMap'(partcall(1,'Prelude.++',[_7856832])),_7860144)),_7864046,_7864052,_7864058).
'blocked_blocked_blocked_Pretty.joinWith_2_[|]_2'('FAIL'(_7865916),_7857066,_7860144,_7856832,'FAIL'(_7865916),_7865930,_7865930).
'blocked_blocked_Pretty.joinWith_2'('FAIL'(_7866006),_7856832,'FAIL'(_7866006),_7866020,_7866020).

'Pretty.prettyPrec._\'23caseor0'(_7867262,_7867264,_7867266,_7867268,_7867270,_7867272,_7867274):-freeze(_7867272,'blocked_Pretty.prettyPrec._\'23caseor0'(_7867262,_7867264,_7867266,_7867268,_7867270,_7867272,_7867274)).
'blocked_Pretty.prettyPrec._\'23caseor0'(_7867378,_7867396,_7867414,_7867432,_7869886,_7869892,_7869898):-hnf(_7867378,_7871332,_7869892,_7871362),'blocked_Pretty.prettyPrec._\'23caseor0_1'(_7871332,_7867396,_7867414,_7867432,_7869886,_7871362,_7869898).

'blocked_Pretty.prettyPrec._\'23caseor0_1'(_7871808,_7871810,_7871812,_7871814,_7871816,_7871818,_7871820):-freeze(_7871818,freeze(_7871808,'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'(_7871808,_7871810,_7871812,_7871814,_7871816,_7871818,_7871820))).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'('RadExpr.Inv'(_7867548),_7867396,_7867414,_7867432,_7872220,_7872226,_7872232):-hnf('Pretty.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7867396),'Pretty.precMul'),'Prelude.++'('Pretty.prettyPrec'('Pretty.precMul',_7867414),'Prelude.++'([^/],'Pretty.prettyPrec'('Pretty.precPow',_7867548)))),_7872220,_7872226,_7872232).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'('RadExpr.Lit'(_7868960),_7867396,_7867414,_7867432,_7876190,_7876196,_7876202):-hnf(_7867432,_7876190,_7876196,_7876202).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'('RadExpr.Neg'(_7869070),_7867396,_7867414,_7867432,_7877178,_7877184,_7877190):-hnf(_7867432,_7877178,_7877184,_7877190).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'('RadExpr.Add'(_7869180,_7869198),_7867396,_7867414,_7867432,_7878174,_7878180,_7878186):-hnf(_7867432,_7878174,_7878180,_7878186).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'('RadExpr.Mul'(_7869314,_7869332),_7867396,_7867414,_7867432,_7879236,_7879242,_7879248):-hnf(_7867432,_7879236,_7879242,_7879248).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'('RadExpr.Root'(_7869448,_7869466),_7867396,_7867414,_7867432,_7880310,_7880316,_7880322):-hnf(_7867432,_7880310,_7880316,_7880322).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'('RadExpr.Pow'(_7869582,_7869600),_7867396,_7867414,_7867432,_7881372,_7881378,_7881384):-!,hnf(_7867432,_7881372,_7881378,_7881384).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0_1'('FAIL'(_7882148),_7867396,_7867414,_7867432,'FAIL'(_7882148),_7882162,_7882162).

'Pretty.prettyPrec._\'23caseor0._\'23caseor0'(_7883828,_7883830,_7883832,_7883834,_7883836):-freeze(_7883834,'blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0'(_7883828,_7883830,_7883832,_7883834,_7883836)).
'blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0'(_7883924,_7883942,_7886798,_7886804,_7886810):-hnf(_7883924,_7888660,_7886804,_7888678),'blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'(_7888660,_7883942,_7886798,_7888678,_7886810).

'blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'(_7889180,_7889182,_7889184,_7889186,_7889188):-freeze(_7889186,freeze(_7889180,'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'(_7889180,_7889182,_7889184,_7889186,_7889188))).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_7884058),_7883942,_7891654,_7891660,_7891666):-makeShare(_7884058,_7889666),hnf('Rational.ratEq'(_7889666,'Rational.fromInt'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1))),_7894338,_7891660,_7894304),'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7894338,_7889666,_7883942,_7891654,_7894304,_7891666).

'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7895018,_7895020,_7895022,_7895024,_7895026,_7895028):-freeze(_7895026,freeze(_7895018,'blocked_blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7895018,_7895020,_7895022,_7895024,_7895026,_7895028))).
'blocked_blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_7889666,_7883942,['^i'],_7895428,_7895428).
'blocked_blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_7889666,_7883942,_7896744,_7896750,_7896756):-!,hnf('Prelude.++'(['^s','^q','^r','^t'],'Pretty.prettyRadicand'('RadExpr.Lit'(_7889666))),_7896744,_7896750,_7896756).
'blocked_blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_7899136),_7889666,_7883942,'FAIL'(_7899136),_7899150,_7899150).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_7885800),_7883942,_7899514,_7899520,_7899526):-hnf(_7883942,_7899514,_7899520,_7899526).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_7885910,_7885928),_7883942,_7900434,_7900440,_7900446):-hnf(_7883942,_7900434,_7900440,_7900446).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_7886044,_7886062),_7883942,_7901420,_7901426,_7901432):-hnf(_7883942,_7901420,_7901426,_7901432).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_7886178),_7883942,_7902398,_7902404,_7902410):-hnf(_7883942,_7902398,_7902404,_7902410).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_7886288,_7886306),_7883942,_7903330,_7903336,_7903342):-hnf(_7883942,_7903330,_7903336,_7903342).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_7886422,_7886440),_7883942,_7904316,_7904322,_7904328):-!,hnf(_7883942,_7904316,_7904322,_7904328).
'blocked_blocked_Pretty.prettyPrec._\'23caseor0._\'23caseor0_1'('FAIL'(_7905016),_7883942,'FAIL'(_7905016),_7905030,_7905030).

'Pretty.prettyNeg._\'23caseor0._\'23caseor0'(_7906642,_7906644,_7906646,_7906648,_7906650,_7906652,_7906654):-freeze(_7906652,'blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0'(_7906642,_7906644,_7906646,_7906648,_7906650,_7906652,_7906654)).
'blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0'(_7906758,_7906776,_7906794,_7906812,_7908506,_7908512,_7908518):-hnf(_7906758,_7910348,_7908512,_7910378),'blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'(_7910348,_7906776,_7906794,_7906812,_7908506,_7910378,_7908518).

'blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'(_7910890,_7910892,_7910894,_7910896,_7910898,_7910900,_7910902):-freeze(_7910900,freeze(_7910890,'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'(_7910890,_7910892,_7910894,_7910896,_7910898,_7910900,_7910902))).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_7906928),_7906776,_7906794,_7906812,_7911302,_7911308,_7911314):-hnf('Pretty.prettyPrec'(_7906776,'RadExpr.Mul'('RadExpr.Lit'('Rational.ratNeg'(_7906928)),_7906794)),_7911302,_7911308,_7911314).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_7907514),_7906776,_7906794,_7906812,_7913340,_7913346,_7913352):-hnf(_7906812,_7913340,_7913346,_7913352).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_7907624,_7907642),_7906776,_7906794,_7906812,_7914402,_7914408,_7914414):-hnf(_7906812,_7914402,_7914408,_7914414).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_7907758,_7907776),_7906776,_7906794,_7906812,_7915530,_7915536,_7915542):-hnf(_7906812,_7915530,_7915536,_7915542).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_7907892),_7906776,_7906794,_7906812,_7916650,_7916656,_7916662):-hnf(_7906812,_7916650,_7916656,_7916662).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_7908002,_7908020),_7906776,_7906794,_7906812,_7917724,_7917730,_7917736):-hnf(_7906812,_7917724,_7917730,_7917736).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_7908136,_7908154),_7906776,_7906794,_7906812,_7918852,_7918858,_7918864):-!,hnf(_7906812,_7918852,_7918858,_7918864).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0._\'23caseor0_1'('FAIL'(_7919694),_7906776,_7906794,_7906812,'FAIL'(_7919694),_7919708,_7919708).

'Pretty.prettyNeg._\'23caseor0'(_7920928,_7920930,_7920932,_7920934,_7920936,_7920938,_7920940):-freeze(_7920938,'blocked_Pretty.prettyNeg._\'23caseor0'(_7920928,_7920930,_7920932,_7920934,_7920936,_7920938,_7920940)).
'blocked_Pretty.prettyNeg._\'23caseor0'(_7921044,_7921062,_7921080,_7921098,_7925008,_7925014,_7925020):-hnf(_7921044,_7926418,_7925014,_7926448),'blocked_Pretty.prettyNeg._\'23caseor0_1'(_7926418,_7921062,_7921080,_7921098,_7925008,_7926448,_7925020).

'blocked_Pretty.prettyNeg._\'23caseor0_1'(_7926888,_7926890,_7926892,_7926894,_7926896,_7926898,_7926900):-freeze(_7926898,freeze(_7926888,'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'(_7926888,_7926890,_7926892,_7926894,_7926896,_7926898,_7926900))).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'('RadExpr.Add'(_7921214,_7921232),_7921062,_7921080,_7921098,_7927308,_7927314,_7927320):-hnf('Pretty.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7921080),'Pretty.precAdd'),'Pretty.renderTermsBasic'('Prelude.map'(partcall(1,'Pretty.negTerm',[]),'Pretty.flattenAddBasic'(_7921062)))),_7927308,_7927314,_7927320).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'('RadExpr.Mul'(_7922174,_7922192),_7921062,_7921080,_7921098,_7930510,_7930516,_7930522):-makeShare(_7922232,_7930848),makeShare(_7921080,_7930868),hnf('Prelude.cond'(letrec4PAKCS(_7930848,'Pretty.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7930868),'Pretty.precNeg'),'Prelude.++'([^-],'Pretty.prettyPrec'('Pretty.precNeg',_7921062)))),'Pretty.prettyNeg._\'23caseor0._\'23caseor0'(_7922174,_7930868,_7922192,_7930848)),_7930510,_7930516,_7930522).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'('RadExpr.Lit'(_7923924),_7921062,_7921080,_7921098,_7935668,_7935674,_7935680):-hnf('Pretty.prettyPrec'(_7921080,'RadExpr.Lit'('Rational.ratNeg'(_7923924))),_7935668,_7935674,_7935680).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'('RadExpr.Neg'(_7924356),_7921062,_7921080,_7921098,_7937342,_7937348,_7937354):-hnf(_7921098,_7937342,_7937348,_7937354).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'('RadExpr.Inv'(_7924466),_7921062,_7921080,_7921098,_7938324,_7938330,_7938336):-hnf(_7921098,_7938324,_7938330,_7938336).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'('RadExpr.Root'(_7924576,_7924594),_7921062,_7921080,_7921098,_7939326,_7939332,_7939338):-hnf(_7921098,_7939326,_7939332,_7939338).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'('RadExpr.Pow'(_7924710,_7924728),_7921062,_7921080,_7921098,_7940382,_7940388,_7940394):-!,hnf(_7921098,_7940382,_7940388,_7940394).
'blocked_blocked_Pretty.prettyNeg._\'23caseor0_1'('FAIL'(_7941152),_7921062,_7921080,_7921098,'FAIL'(_7941152),_7941166,_7941166).

'Pretty.prettyRadicand._\'23caseor0'(_7942576,_7942578,_7942580,_7942582,_7942584,_7942586):-freeze(_7942584,'blocked_Pretty.prettyRadicand._\'23caseor0'(_7942576,_7942578,_7942580,_7942582,_7942584,_7942586)).
'blocked_Pretty.prettyRadicand._\'23caseor0'(_7942682,_7942700,_7942718,_7945946,_7945952,_7945958):-hnf(_7942682,_7947528,_7945952,_7947552),'blocked_Pretty.prettyRadicand._\'23caseor0_1'(_7947528,_7942700,_7942718,_7945946,_7947552,_7945958).

'blocked_Pretty.prettyRadicand._\'23caseor0_1'(_7948014,_7948016,_7948018,_7948020,_7948022,_7948024):-freeze(_7948022,freeze(_7948014,'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'(_7948014,_7948016,_7948018,_7948020,_7948022,_7948024))).
'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'('RadExpr.Lit'(_7942834),_7942700,_7942718,_7951162,_7951168,_7951174):-makeShare(_7942834,_7948552),hnf('Prelude.&&'('Rational.ratGe'(_7948552,'Rational.fromInt'(0)),'Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'('Rational.denominator'(_7948552),1)),_7953572,_7951168,_7953532),'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7953572,_7948552,_7942700,_7942718,_7951162,_7953532,_7951174).

'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7954206,_7954208,_7954210,_7954212,_7954214,_7954216,_7954218):-freeze(_7954216,freeze(_7954206,'blocked_blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7954206,_7954208,_7954210,_7954212,_7954214,_7954216,_7954218))).
'blocked_blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_7948552,_7942700,_7942718,_7954620,_7954626,_7954632):-hnf('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23','Rational.numerator'(_7948552)),_7954620,_7954626,_7954632).
'blocked_blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_7948552,_7942700,_7942718,_7956602,_7956608,_7956614):-!,hnf('Prelude.++'(['^('],'Prelude.++'('Prelude.apply'('Pretty.pretty',_7942700),['^)'])),_7956602,_7956608,_7956614).
'blocked_blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_7958916),_7948552,_7942700,_7942718,'FAIL'(_7958916),_7958930,_7958930).
'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'('RadExpr.Root'(_7944828,_7944846),_7942700,_7942718,_7959322,_7959328,_7959334):-hnf('Pretty.prettyPrec'('Pretty.precPow',_7942700),_7959322,_7959328,_7959334).
'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'('RadExpr.Neg'(_7945130),_7942700,_7942718,_7960730,_7960736,_7960742):-hnf(_7942718,_7960730,_7960736,_7960742).
'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'('RadExpr.Add'(_7945240,_7945258),_7942700,_7942718,_7961676,_7961682,_7961688):-hnf(_7942718,_7961676,_7961682,_7961688).
'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'('RadExpr.Mul'(_7945374,_7945392),_7942700,_7942718,_7962688,_7962694,_7962700):-hnf(_7942718,_7962688,_7962694,_7962700).
'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'('RadExpr.Inv'(_7945508),_7942700,_7942718,_7963692,_7963698,_7963704):-hnf(_7942718,_7963692,_7963698,_7963704).
'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'('RadExpr.Pow'(_7945618,_7945636),_7942700,_7942718,_7964638,_7964644,_7964650):-!,hnf(_7942718,_7964638,_7964644,_7964650).
'blocked_blocked_Pretty.prettyRadicand._\'23caseor0_1'('FAIL'(_7965364),_7942700,_7942718,'FAIL'(_7965364),_7965378,_7965378).

'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0'(_7967634,_7967636,_7967638,_7967640,_7967642,_7967644,_7967646):-freeze(_7967644,'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0'(_7967634,_7967636,_7967638,_7967640,_7967642,_7967644,_7967646)).
'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0'(_7967750,_7967768,_7967786,_7967804,_7970636,_7970642,_7970648):-hnf(_7967750,_7973126,_7970642,_7973156),'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'(_7973126,_7967768,_7967786,_7967804,_7970636,_7973156,_7970648).

'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'(_7973776,_7973778,_7973780,_7973782,_7973784,_7973786,_7973788):-freeze(_7973786,freeze(_7973776,'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'(_7973776,_7973778,_7973780,_7973782,_7973784,_7973786,_7973788))).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_7967920),_7967768,_7967786,_7967804,_7976128,_7976134,_7976140):-makeShare(_7967920,_7974338),hnf('Rational.ratLt'(_7974338,'Rational.fromInt'(0)),_7979452,_7976134,_7979406),'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7979452,_7974338,_7967768,_7967786,_7967804,_7976128,_7979406,_7976140).

'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7980238,_7980240,_7980242,_7980244,_7980246,_7980248,_7980250,_7980252):-freeze(_7980250,freeze(_7980238,'blocked_blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_7980238,_7980240,_7980242,_7980244,_7980246,_7980248,_7980250,_7980252))).
'blocked_blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_7974338,_7967768,_7967786,_7967804,['Prelude.(,)'('Prelude.False','Pretty.rebuildMul'(['RadExpr.Lit'('Rational.ratNeg'(_7974338))|_7967768]))],_7980668,_7980668).
'blocked_blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_7974338,_7967768,_7967786,_7967804,['Prelude.(,)'('Prelude.True',_7967786)],_7983390,_7983390):-!.
'blocked_blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_7985000),_7974338,_7967768,_7967786,_7967804,'FAIL'(_7985000),_7985014,_7985014).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_7969536),_7967768,_7967786,_7967804,_7985394,_7985400,_7985406):-hnf(_7967804,_7985394,_7985400,_7985406).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_7969646,_7969664),_7967768,_7967786,_7967804,_7986564,_7986570,_7986576):-hnf(_7967804,_7986564,_7986570,_7986576).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_7969780,_7969798),_7967768,_7967786,_7967804,_7987800,_7987806,_7987812):-hnf(_7967804,_7987800,_7987806,_7987812).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_7969914),_7967768,_7967786,_7967804,_7989028,_7989034,_7989040):-hnf(_7967804,_7989028,_7989034,_7989040).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_7970024,_7970042),_7967768,_7967786,_7967804,_7990210,_7990216,_7990222):-hnf(_7967804,_7990210,_7990216,_7990222).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_7970158,_7970176),_7967768,_7967786,_7967804,_7991446,_7991452,_7991458):-!,hnf(_7967804,_7991446,_7991452,_7991458).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_7992396),_7967768,_7967786,_7967804,'FAIL'(_7992396),_7992410,_7992410).

'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0'(_7994266,_7994268,_7994270,_7994272,_7994274):-freeze(_7994272,'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0'(_7994266,_7994268,_7994270,_7994272,_7994274)).
'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0'(_7994362,_7994380,_7996296,_7996302,_7996308):-hnf(_7994362,_7998338,_7996302,_7998356),'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0_1'(_7998338,_7994380,_7996296,_7998356,_7996308).

'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0_1'(_7998888,_7998890,_7998892,_7998894,_7998896):-freeze(_7998894,freeze(_7998888,'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0_1'(_7998888,_7998890,_7998892,_7998894,_7998896))).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0_1'([_7994496|_7994514],_7994380,_7999174,_7999180,_7999186):-makeShare(_7994554,_7999420),makeShare(_7994380,_7999440),hnf('Prelude.cond'(letrec4PAKCS(_7999420,['Prelude.(,)'('Prelude.True',_7999440)]),'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0'(_7994496,_7994514,_7999440,_7999420)),_7999174,_7999180,_7999186).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0_1'([],_7994380,['Prelude.(,)'('Prelude.True',_7994380)],_8002552,_8002552):-!.
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0_1'('FAIL'(_8003676),_7994380,'FAIL'(_8003676),_8003690,_8003690).

'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_8006346,_8006348,_8006350,_8006352,_8006354,_8006356):-freeze(_8006354,'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_8006346,_8006348,_8006350,_8006352,_8006354,_8006356)).
'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_8006452,_8006470,_8006488,_8008376,_8008382,_8008388):-hnf(_8006452,_8011290,_8008382,_8011314),'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_8011290,_8006470,_8006488,_8008376,_8011314,_8008388).

'blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_8011998,_8012000,_8012002,_8012004,_8012006,_8012008):-freeze(_8012006,freeze(_8011998,'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_8011998,_8012000,_8012002,_8012004,_8012006,_8012008))).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_8006604),_8006470,_8006488,['Prelude.(,)'('Prelude.False','RadExpr.Mul'(_8006604,_8006470))],_8012406,_8012406).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_8007204),_8006470,_8006488,_8014384,_8014390,_8014396):-hnf(_8006488,_8014384,_8014390,_8014396).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_8007314,_8007332),_8006470,_8006488,_8015552,_8015558,_8015564):-hnf(_8006488,_8015552,_8015558,_8015564).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_8007448,_8007466),_8006470,_8006488,_8016786,_8016792,_8016798):-hnf(_8006488,_8016786,_8016792,_8016798).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_8007582),_8006470,_8006488,_8018012,_8018018,_8018024):-hnf(_8006488,_8018012,_8018018,_8018024).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_8007692,_8007710),_8006470,_8006488,_8019192,_8019198,_8019204):-hnf(_8006488,_8019192,_8019198,_8019204).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_8007826,_8007844),_8006470,_8006488,_8020426,_8020432,_8020438):-!,hnf(_8006488,_8020426,_8020432,_8020438).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_8021374),_8006470,_8006488,'FAIL'(_8021374),_8021388,_8021388).

'Pretty.flattenAddBasic._\'23caseor0'(_8022828,_8022830,_8022832,_8022834,_8022836,_8022838):-freeze(_8022836,'blocked_Pretty.flattenAddBasic._\'23caseor0'(_8022828,_8022830,_8022832,_8022834,_8022836,_8022838)).
'blocked_Pretty.flattenAddBasic._\'23caseor0'(_8022934,_8022952,_8022970,_8026882,_8026888,_8026894):-hnf(_8022934,_8028500,_8026888,_8028524),'blocked_Pretty.flattenAddBasic._\'23caseor0_1'(_8028500,_8022952,_8022970,_8026882,_8028524,_8026894).

'blocked_Pretty.flattenAddBasic._\'23caseor0_1'(_8028992,_8028994,_8028996,_8028998,_8029000,_8029002):-freeze(_8029000,freeze(_8028992,'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'(_8028992,_8028994,_8028996,_8028998,_8029000,_8029002))).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'('RadExpr.Add'(_8023086,_8023104),_8022952,_8022970,_8029402,_8029408,_8029414):-hnf('Prelude.++'('Pretty.flattenAddBasic'(_8023086),'Pretty.flattenAddBasic'(_8023104)),_8029402,_8029408,_8029414).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'('RadExpr.Neg'(_8023542),_8022952,_8022970,_8031234,_8031240,_8031246):-hnf('Prelude.map'(partcall(1,'Pretty.negTerm',[]),'Pretty.flattenAddBasic'(_8023542)),_8031234,_8031240,_8031246).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'('RadExpr.Lit'(_8023904),_8022952,_8022970,_8034446,_8034452,_8034458):-makeShare(_8023904,_8032890),hnf('Rational.ratLt'(_8032890,'Rational.fromInt'(0)),_8036892,_8034452,_8036852),'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_8036892,_8032890,_8022952,_8022970,_8034446,_8036852,_8034458).

'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_8037532,_8037534,_8037536,_8037538,_8037540,_8037542,_8037544):-freeze(_8037542,freeze(_8037532,'blocked_blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_8037532,_8037534,_8037536,_8037538,_8037540,_8037542,_8037544))).
'blocked_blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_8032890,_8022952,_8022970,['Prelude.(,)'('Prelude.False','RadExpr.Lit'('Rational.ratNeg'(_8032890)))],_8037952,_8037952).
'blocked_blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_8032890,_8022952,_8022970,['Prelude.(,)'('Prelude.True','RadExpr.Lit'(_8032890))],_8040030,_8040030):-!.
'blocked_blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_8041598),_8032890,_8022952,_8022970,'FAIL'(_8041598),_8041612,_8041612).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'('RadExpr.Mul'(_8025366,_8025384),_8022952,_8022970,_8041992,_8041998,_8042004):-makeShare(_8025424,_8042242),makeShare(_8022952,_8042262),hnf('Prelude.cond'(letrec4PAKCS(_8042242,'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0'('Pretty.flattenMulBasic'(_8042262),_8042262)),'Pretty.flattenAddBasic._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_8025366,_8025384,_8042242)),_8041992,_8041998,_8042004).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'('RadExpr.Inv'(_8026304),_8022952,_8022970,_8045650,_8045656,_8045662):-hnf(_8022970,_8045650,_8045656,_8045662).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'('RadExpr.Root'(_8026414,_8026432),_8022952,_8022970,_8046614,_8046620,_8046626):-hnf(_8022970,_8046614,_8046620,_8046626).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'('RadExpr.Pow'(_8026548,_8026566),_8022952,_8022970,_8047632,_8047638,_8047644):-!,hnf(_8022970,_8047632,_8047638,_8047644).
'blocked_blocked_Pretty.flattenAddBasic._\'23caseor0_1'('FAIL'(_8048364),_8022952,_8022970,'FAIL'(_8048364),_8048378,_8048378).

'Pretty.flattenMulBasic._\'23caseor0'(_8049818,_8049820,_8049822,_8049824,_8049826):-freeze(_8049824,'blocked_Pretty.flattenMulBasic._\'23caseor0'(_8049818,_8049820,_8049822,_8049824,_8049826)).
'blocked_Pretty.flattenMulBasic._\'23caseor0'(_8049914,_8049932,_8051436,_8051442,_8051448):-hnf(_8049914,_8053046,_8051442,_8053064),'blocked_Pretty.flattenMulBasic._\'23caseor0_1'(_8053046,_8049932,_8051436,_8053064,_8051448).

'blocked_Pretty.flattenMulBasic._\'23caseor0_1'(_8053524,_8053526,_8053528,_8053530,_8053532):-freeze(_8053530,freeze(_8053524,'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'(_8053524,_8053526,_8053528,_8053530,_8053532))).
'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'('RadExpr.Mul'(_8050048,_8050066),_8049932,_8053924,_8053930,_8053936):-hnf('Prelude.++'('Pretty.flattenMulBasic'(_8050048),'Pretty.flattenMulBasic'(_8050066)),_8053924,_8053930,_8053936).
'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'('RadExpr.Lit'(_8050504),_8049932,_8055682,_8055688,_8055694):-hnf(_8049932,_8055682,_8055688,_8055694).
'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'('RadExpr.Neg'(_8050614),_8049932,_8056552,_8056558,_8056564):-hnf(_8049932,_8056552,_8056558,_8056564).
'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'('RadExpr.Add'(_8050724,_8050742),_8049932,_8057430,_8057436,_8057442):-hnf(_8049932,_8057430,_8057436,_8057442).
'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'('RadExpr.Inv'(_8050858),_8049932,_8058366,_8058372,_8058378):-hnf(_8049932,_8058366,_8058372,_8058378).
'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'('RadExpr.Root'(_8050968,_8050986),_8049932,_8059256,_8059262,_8059268):-hnf(_8049932,_8059256,_8059262,_8059268).
'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'('RadExpr.Pow'(_8051102,_8051120),_8049932,_8060200,_8060206,_8060212):-!,hnf(_8049932,_8060200,_8060206,_8060212).
'blocked_blocked_Pretty.flattenMulBasic._\'23caseor0_1'('FAIL'(_8060858),_8049932,'FAIL'(_8060858),_8060872,_8060872).

'Pretty.renderTermsBasic._\'23caseor0'(_8062342,_8062344,_8062346,_8062348,_8062350):-freeze(_8062348,'blocked_Pretty.renderTermsBasic._\'23caseor0'(_8062342,_8062344,_8062346,_8062348,_8062350)).
'blocked_Pretty.renderTermsBasic._\'23caseor0'(_8062438,_8062456,_8063608,_8063614,_8063620):-hnf(_8062438,_8065254,_8063614,_8065272),'blocked_Pretty.renderTermsBasic._\'23caseor0_1'(_8065254,_8062456,_8063608,_8065272,_8063620).

'blocked_Pretty.renderTermsBasic._\'23caseor0_1'(_8065738,_8065740,_8065742,_8065744,_8065746):-freeze(_8065744,freeze(_8065738,'blocked_blocked_Pretty.renderTermsBasic._\'23caseor0_1'(_8065738,_8065740,_8065742,_8065744,_8065746))).
'blocked_blocked_Pretty.renderTermsBasic._\'23caseor0_1'('Prelude.True',_8062456,_8066132,_8066138,_8066144):-hnf('Pretty.prettyPrec'('Pretty.precAdd',_8062456),_8066132,_8066138,_8066144).
'blocked_blocked_Pretty.renderTermsBasic._\'23caseor0_1'('Prelude.False',_8062456,_8067360,_8067366,_8067372):-!,hnf('Prelude.++'([^-],'Pretty.prettyPrec'('Pretty.precMul',_8062456)),_8067360,_8067366,_8067372).
'blocked_blocked_Pretty.renderTermsBasic._\'23caseor0_1'('FAIL'(_8068836),_8062456,'FAIL'(_8068836),_8068850,_8068850).

'Pretty.renderFactorsBasic._\'23caseor0'(_8070396,_8070398,_8070400,_8070402,_8070404,_8070406):-freeze(_8070404,'blocked_Pretty.renderFactorsBasic._\'23caseor0'(_8070396,_8070398,_8070400,_8070402,_8070404,_8070406)).
'blocked_Pretty.renderFactorsBasic._\'23caseor0'(_8070502,_8070520,_8070538,_8075282,_8075288,_8075294):-hnf(_8070502,_8077008,_8075288,_8077032),'blocked_Pretty.renderFactorsBasic._\'23caseor0_1'(_8077008,_8070520,_8070538,_8075282,_8077032,_8075294).

'blocked_Pretty.renderFactorsBasic._\'23caseor0_1'(_8077518,_8077520,_8077522,_8077524,_8077526,_8077528):-freeze(_8077526,freeze(_8077518,'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'(_8077518,_8077520,_8077522,_8077524,_8077526,_8077528))).
'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'('RadExpr.Lit'(_8070654),_8070520,_8070538,_8080212,_8080218,_8080224):-makeShare(_8070654,_8078146),hnf('Rational.ratEq'(_8078146,'Rational.fromInt'(1)),_8082766,_8080218,_8082726),'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_8082766,_8078146,_8070520,_8070538,_8080212,_8082726,_8080224).

'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_8083424,_8083426,_8083428,_8083430,_8083432,_8083434,_8083436):-freeze(_8083434,freeze(_8083424,'blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_8083424,_8083426,_8083428,_8083430,_8083432,_8083434,_8083436))).
'blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_8078146,_8070520,_8070538,_8083838,_8083844,_8083850):-hnf('Pretty.joinWith'([^*],'Prelude.map'(partcall(1,'Pretty.prettyPrec',['Pretty.precPow']),_8070520)),_8083838,_8083844,_8083850).
'blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_8078146,_8070520,_8070538,_8088642,_8088648,_8088654):-!,makeShare(_8078146,_8086270),hnf('Rational.ratEq'(_8086270,'Rational.fromInt'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1))),_8092416,_8088648,_8092376),'blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_8092416,_8086270,_8070520,_8070538,_8088642,_8092376,_8088654).

'blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_8093296,_8093298,_8093300,_8093302,_8093304,_8093306,_8093308):-freeze(_8093306,freeze(_8093296,'blocked_blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_8093296,_8093298,_8093300,_8093302,_8093304,_8093306,_8093308))).
'blocked_blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8086270,_8070520,_8070538,_8093710,_8093716,_8093722):-hnf('Prelude.++'([^-],'Pretty.joinWith'([^*],'Prelude.map'(partcall(1,'Pretty.prettyPrec',['Pretty.precPow']),_8070520))),_8093710,_8093716,_8093722).
'blocked_blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8086270,_8070520,_8070538,_8096722,_8096728,_8096734):-!,hnf('Prelude.++'('Pretty.prettyRat'(_8086270),'Prelude.++'([^*],'Pretty.joinWith'([^*],'Prelude.map'(partcall(1,'Pretty.prettyPrec',['Pretty.precPow']),_8070520)))),_8096722,_8096728,_8096734).
'blocked_blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8099986),_8086270,_8070520,_8070538,'FAIL'(_8099986),_8100000,_8100000).
'blocked_blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_8100076),_8078146,_8070520,_8070538,'FAIL'(_8100076),_8100090,_8100090).
'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'('RadExpr.Neg'(_8074308),_8070520,_8070538,_8100462,_8100468,_8100474):-hnf(_8070538,_8100462,_8100468,_8100474).
'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'('RadExpr.Add'(_8074418,_8074436),_8070520,_8070538,_8101432,_8101438,_8101444):-hnf(_8070538,_8101432,_8101438,_8101444).
'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'('RadExpr.Mul'(_8074552,_8074570),_8070520,_8070538,_8102468,_8102474,_8102480):-hnf(_8070538,_8102468,_8102474,_8102480).
'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'('RadExpr.Inv'(_8074686),_8070520,_8070538,_8103496,_8103502,_8103508):-hnf(_8070538,_8103496,_8103502,_8103508).
'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'('RadExpr.Root'(_8074796,_8074814),_8070520,_8070538,_8104478,_8104484,_8104490):-hnf(_8070538,_8104478,_8104484,_8104490).
'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'('RadExpr.Pow'(_8074930,_8074948),_8070520,_8070538,_8105514,_8105520,_8105526):-!,hnf(_8070538,_8105514,_8105520,_8105526).
'blocked_blocked_Pretty.renderFactorsBasic._\'23caseor0_1'('FAIL'(_8106264),_8070520,_8070538,'FAIL'(_8106264),_8106278,_8106278).

'Pretty.prettyRat._\'23caseor0'(_8107490,_8107492,_8107494,_8107496,_8107498,_8107500):-freeze(_8107498,'blocked_Pretty.prettyRat._\'23caseor0'(_8107490,_8107492,_8107494,_8107496,_8107498,_8107500)).
'blocked_Pretty.prettyRat._\'23caseor0'(_8107596,_8107614,_8107632,_8111914,_8111920,_8111926):-hnf(_8107596,_8113316,_8111920,_8113340),'blocked_Pretty.prettyRat._\'23caseor0_1'(_8113316,_8107614,_8107632,_8111914,_8113340,_8111926).

'blocked_Pretty.prettyRat._\'23caseor0_1'(_8113772,_8113774,_8113776,_8113778,_8113780,_8113782):-freeze(_8113780,freeze(_8113772,'blocked_blocked_Pretty.prettyRat._\'23caseor0_1'(_8113772,_8113774,_8113776,_8113778,_8113780,_8113782))).
'blocked_blocked_Pretty.prettyRat._\'23caseor0_1'('Prelude.True',_8107614,_8107632,_8114176,_8114182,_8114188):-hnf('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_8107614),_8114176,_8114182,_8114188).
'blocked_blocked_Pretty.prettyRat._\'23caseor0_1'('Prelude.False',_8107614,_8107632,_8118044,_8118050,_8118056):-!,makeShare(_8107614,_8115848),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_8115848),0),_8120328,_8118050,_8120294),'blocked_blocked_Pretty.prettyRat._\'23caseor0_1_Prelude.False_ComplexCase'(_8120328,_8115848,_8107632,_8118044,_8120294,_8118056).

'blocked_blocked_Pretty.prettyRat._\'23caseor0_1_Prelude.False_ComplexCase'(_8120960,_8120962,_8120964,_8120966,_8120968,_8120970):-freeze(_8120968,freeze(_8120960,'blocked_blocked_blocked_Pretty.prettyRat._\'23caseor0_1_Prelude.False_ComplexCase'(_8120960,_8120962,_8120964,_8120966,_8120968,_8120970))).
'blocked_blocked_blocked_Pretty.prettyRat._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_8115848,_8107632,_8121364,_8121370,_8121376):-hnf('Prelude.++'(['^(',^-],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23','Pretty.absInt'(_8115848)),'Prelude.++'([^/],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_8107632),['^)'])))),_8121364,_8121370,_8121376).
'blocked_blocked_blocked_Pretty.prettyRat._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_8115848,_8107632,_8125996,_8126002,_8126008):-!,hnf('Prelude.++'(['^('],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_8115848),'Prelude.++'([^/],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_8107632),['^)'])))),_8125996,_8126002,_8126008).
'blocked_blocked_blocked_Pretty.prettyRat._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_8129946),_8115848,_8107632,'FAIL'(_8129946),_8129960,_8129960).
'blocked_blocked_Pretty.prettyRat._\'23caseor0_1'('FAIL'(_8130028),_8107614,_8107632,'FAIL'(_8130028),_8130042,_8130042).

:-costCenters(['']).




%%%%% Number of shared variables: 37

