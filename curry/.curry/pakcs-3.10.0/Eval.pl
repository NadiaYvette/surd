%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').
:-importModule('RadExpr').
:-importModule('Rational').

:-curryModule('Eval').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Eval.ratToFloat','Eval.ratToFloat',1,'Eval.ratToFloat',nofix,notype).
functiontype('Eval.evalDouble',evalDouble,1,'Eval.evalDouble',nofix,notype).
functiontype('Eval.evalComplex',evalComplex,1,'Eval.evalComplex',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP2\'23re','Eval.evalComplex._#selFP2#re',1,'Eval.evalComplex._\'23selFP2\'23re',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP3\'23im','Eval.evalComplex._#selFP3#im',1,'Eval.evalComplex._\'23selFP3\'23im',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP8\'23ar','Eval.evalComplex._#selFP8#ar',1,'Eval.evalComplex._\'23selFP8\'23ar',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP9\'23ai','Eval.evalComplex._#selFP9#ai',1,'Eval.evalComplex._\'23selFP9\'23ai',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP6\'23br','Eval.evalComplex._#selFP6#br',1,'Eval.evalComplex._\'23selFP6\'23br',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP7\'23bi','Eval.evalComplex._#selFP7#bi',1,'Eval.evalComplex._\'23selFP7\'23bi',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP14\'23ar','Eval.evalComplex._#selFP14#ar',1,'Eval.evalComplex._\'23selFP14\'23ar',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP15\'23ai','Eval.evalComplex._#selFP15#ai',1,'Eval.evalComplex._\'23selFP15\'23ai',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP12\'23br','Eval.evalComplex._#selFP12#br',1,'Eval.evalComplex._\'23selFP12\'23br',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP13\'23bi','Eval.evalComplex._#selFP13#bi',1,'Eval.evalComplex._\'23selFP13\'23bi',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP17\'23re','Eval.evalComplex._#selFP17#re',1,'Eval.evalComplex._\'23selFP17\'23re',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP18\'23im','Eval.evalComplex._#selFP18#im',1,'Eval.evalComplex._\'23selFP18\'23im',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP20\'23zr','Eval.evalComplex._#selFP20#zr',1,'Eval.evalComplex._\'23selFP20\'23zr',nofix,notype).
functiontype('Eval.evalComplex._\'23selFP21\'23zi','Eval.evalComplex._#selFP21#zi',1,'Eval.evalComplex._\'23selFP21\'23zi',nofix,notype).
functiontype('Eval.complexPow','Eval.complexPow',2,'Eval.complexPow',nofix,notype).
functiontype('Eval.complexPow._\'23selFP23\'23hr','Eval.complexPow._#selFP23#hr',1,'Eval.complexPow._\'23selFP23\'23hr',nofix,notype).
functiontype('Eval.complexPow._\'23selFP24\'23hi','Eval.complexPow._#selFP24#hi',1,'Eval.complexPow._\'23selFP24\'23hi',nofix,notype).
functiontype('Eval.complexPow._\'23selFP29\'23zr','Eval.complexPow._#selFP29#zr',1,'Eval.complexPow._\'23selFP29\'23zr',nofix,notype).
functiontype('Eval.complexPow._\'23selFP30\'23zi','Eval.complexPow._#selFP30#zi',1,'Eval.complexPow._\'23selFP30\'23zi',nofix,notype).
functiontype('Eval.complexPow._\'23selFP27\'23rr','Eval.complexPow._#selFP27#rr',1,'Eval.complexPow._\'23selFP27\'23rr',nofix,notype).
functiontype('Eval.complexPow._\'23selFP28\'23ri','Eval.complexPow._#selFP28#ri',1,'Eval.complexPow._\'23selFP28\'23ri',nofix,notype).
functiontype('Eval.floatAtan2','Eval.floatAtan2',2,'Eval.floatAtan2',nofix,notype).
functiontype('Eval.complexNthRoot',complexNthRoot,2,'Eval.complexNthRoot',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Eval.ratToFloat'(_7838504,_7838506,_7838508,_7838510):-freeze(_7838508,'blocked_Eval.ratToFloat'(_7838504,_7838506,_7838508,_7838510)).
'blocked_Eval.ratToFloat'(_7838590,_7839096,_7839102,_7839108):-makeShare(_7838590,_7839190),hnf('Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'('Prelude._impl\'23fromInt\'23Prelude.Num\'23Prelude.Float\'23'('Rational.numerator'(_7839190)),'Prelude._impl\'23fromInt\'23Prelude.Num\'23Prelude.Float\'23'('Rational.denominator'(_7839190))),_7839096,_7839102,_7839108).

'Eval.evalDouble'(_7842266,_7842268,_7842270,_7842272):-freeze(_7842270,'blocked_Eval.evalDouble'(_7842266,_7842268,_7842270,_7842272)).
'blocked_Eval.evalDouble'(_7842352,_7846928,_7846934,_7846940):-hnf(_7842352,_7847846,_7846934,_7847858),'blocked_Eval.evalDouble_1'(_7847846,_7846928,_7847858,_7846940).

'blocked_Eval.evalDouble_1'(_7848196,_7848198,_7848200,_7848202):-freeze(_7848200,freeze(_7848196,'blocked_blocked_Eval.evalDouble_1'(_7848196,_7848198,_7848200,_7848202))).
'blocked_blocked_Eval.evalDouble_1'('RadExpr.Lit'(_7842468),_7848578,_7848584,_7848590):-hnf('Eval.ratToFloat'(_7842468),_7848578,_7848584,_7848590).
'blocked_blocked_Eval.evalDouble_1'('RadExpr.Neg'(_7842662),_7849460,_7849466,_7849472):-hnf('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Float\'23'('Eval.evalDouble'(_7842662)),_7849460,_7849466,_7849472).
'blocked_blocked_Eval.evalDouble_1'('RadExpr.Add'(_7842940,_7842958),_7850794,_7850800,_7850806):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'('Eval.evalDouble'(_7842940),'Eval.evalDouble'(_7842958)),_7850794,_7850800,_7850806).
'blocked_blocked_Eval.evalDouble_1'('RadExpr.Mul'(_7843396,_7843414),_7852480,_7852486,_7852492):-hnf('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'('Eval.evalDouble'(_7843396),'Eval.evalDouble'(_7843414)),_7852480,_7852486,_7852492).
'blocked_blocked_Eval.evalDouble_1'('RadExpr.Inv'(_7843852),_7854158,_7854164,_7854170):-hnf('Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(1.0,'Eval.evalDouble'(_7843852)),_7854158,_7854164,_7854170).
'blocked_blocked_Eval.evalDouble_1'('RadExpr.Root'(_7844200,_7844218),_7855608,_7855614,_7855620):-hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'2A\'2A\'23Prelude.Floating\'23Prelude.Float\'23','Eval.evalDouble'(_7844218)),'Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(1.0,'Prelude._impl\'23fromInt\'23Prelude.Num\'23Prelude.Float\'23'(_7844200))),_7855608,_7855614,_7855620).
'blocked_blocked_Eval.evalDouble_1'('RadExpr.Pow'(_7844978,_7844996),_7860494,_7860500,_7860506):-!,makeShare(_7844996,_7858670),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'3D\'23Prelude.Ord\'23Prelude.Int\'23',_7858670),0),_7862234,_7860500,_7862200),'blocked_blocked_Eval.evalDouble_1_RadExpr.Pow_ComplexCase'(_7862234,_7844978,_7858670,_7860494,_7862200,_7860506).

'blocked_blocked_Eval.evalDouble_1_RadExpr.Pow_ComplexCase'(_7862776,_7862778,_7862780,_7862782,_7862784,_7862786):-freeze(_7862784,freeze(_7862776,'blocked_blocked_blocked_Eval.evalDouble_1_RadExpr.Pow_ComplexCase'(_7862776,_7862778,_7862780,_7862782,_7862784,_7862786))).
'blocked_blocked_blocked_Eval.evalDouble_1_RadExpr.Pow_ComplexCase'('Prelude.True',_7844978,_7858670,_7863180,_7863186,_7863192):-hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'2A\'2A\'23Prelude.Floating\'23Prelude.Float\'23','Eval.evalDouble'(_7844978)),'Prelude._impl\'23fromInt\'23Prelude.Num\'23Prelude.Float\'23'(_7858670)),_7863180,_7863186,_7863192).
'blocked_blocked_blocked_Eval.evalDouble_1_RadExpr.Pow_ComplexCase'('Prelude.False',_7844978,_7858670,_7865752,_7865758,_7865764):-!,hnf('Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(1.0,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'2A\'2A\'23Prelude.Floating\'23Prelude.Float\'23','Eval.evalDouble'(_7844978)),'Prelude._impl\'23fromInt\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_7858670)))),_7865752,_7865758,_7865764).
'blocked_blocked_blocked_Eval.evalDouble_1_RadExpr.Pow_ComplexCase'('FAIL'(_7869012),_7844978,_7858670,'FAIL'(_7869012),_7869026,_7869026).
'blocked_blocked_Eval.evalDouble_1'('FAIL'(_7869094),'FAIL'(_7869094),_7869108,_7869108).

'Eval.evalComplex'(_7869896,_7869898,_7869900,_7869902):-freeze(_7869900,'blocked_Eval.evalComplex'(_7869896,_7869898,_7869900,_7869902)).
'blocked_Eval.evalComplex'(_7869982,_7885848,_7885854,_7885860):-hnf(_7869982,_7886802,_7885854,_7886814),'blocked_Eval.evalComplex_1'(_7886802,_7885848,_7886814,_7885860).

'blocked_Eval.evalComplex_1'(_7887158,_7887160,_7887162,_7887164):-freeze(_7887162,freeze(_7887158,'blocked_blocked_Eval.evalComplex_1'(_7887158,_7887160,_7887162,_7887164))).
'blocked_blocked_Eval.evalComplex_1'('RadExpr.Lit'(_7870098),'Prelude.(,)'('Eval.ratToFloat'(_7870098),0.0),_7887546,_7887546).
'blocked_blocked_Eval.evalComplex_1'('RadExpr.Neg'(_7870446),_7888652,_7888658,_7888664):-makeShare(_7870480,_7888976),makeShare(_7870498,_7888996),makeShare(_7870516,_7889016),hnf('Prelude.cond'(letrec4PAKCS(_7888976,'Eval.evalComplex'(_7870446)),'Prelude.cond'(letrec4PAKCS(_7888996,'Eval.evalComplex._\'23selFP2\'23re'(_7888976)),'Prelude.cond'(letrec4PAKCS(_7889016,'Eval.evalComplex._\'23selFP3\'23im'(_7888976)),'Prelude.(,)'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Float\'23'(_7888996),'Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Float\'23'(_7889016))))),_7888652,_7888658,_7888664).
'blocked_blocked_Eval.evalComplex_1'('RadExpr.Add'(_7872136,_7872154),_7894006,_7894012,_7894018):-makeShare(_7872194,_7894976),makeShare(_7872212,_7894996),makeShare(_7872230,_7895016),makeShare(_7872248,_7895036),makeShare(_7872266,_7895056),makeShare(_7872284,_7895076),hnf('Prelude.cond'(letrec4PAKCS(_7894976,'Eval.evalComplex'(_7872136)),'Prelude.cond'(letrec4PAKCS(_7894996,'Eval.evalComplex._\'23selFP8\'23ar'(_7894976)),'Prelude.cond'(letrec4PAKCS(_7895016,'Eval.evalComplex._\'23selFP9\'23ai'(_7894976)),'Prelude.cond'(letrec4PAKCS(_7895036,'Eval.evalComplex'(_7872154)),'Prelude.cond'(letrec4PAKCS(_7895056,'Eval.evalComplex._\'23selFP6\'23br'(_7895036)),'Prelude.cond'(letrec4PAKCS(_7895076,'Eval.evalComplex._\'23selFP7\'23bi'(_7895036)),'Prelude.(,)'('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'(_7894996,_7895056),'Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'(_7895016,_7895076)))))))),_7894006,_7894012,_7894018).
'blocked_blocked_Eval.evalComplex_1'('RadExpr.Mul'(_7875238,_7875256),_7903456,_7903462,_7903468):-makeShare(_7875296,_7904706),makeShare(_7875314,_7904726),makeShare(_7875332,_7904746),makeShare(_7875350,_7904766),makeShare(_7875368,_7904786),makeShare(_7875386,_7904806),hnf('Prelude.cond'(letrec4PAKCS(_7904706,'Eval.evalComplex'(_7875238)),'Prelude.cond'(letrec4PAKCS(_7904726,'Eval.evalComplex._\'23selFP14\'23ar'(_7904706)),'Prelude.cond'(letrec4PAKCS(_7904746,'Eval.evalComplex._\'23selFP15\'23ai'(_7904706)),'Prelude.cond'(letrec4PAKCS(_7904766,'Eval.evalComplex'(_7875256)),'Prelude.cond'(letrec4PAKCS(_7904786,'Eval.evalComplex._\'23selFP12\'23br'(_7904766)),'Prelude.cond'(letrec4PAKCS(_7904806,'Eval.evalComplex._\'23selFP13\'23bi'(_7904766)),'Prelude.(,)'('Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_7904726,_7904786),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_7904746,_7904806)),'Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_7904726,_7904806),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_7904746,_7904786))))))))),_7903456,_7903462,_7903468).
'blocked_blocked_Eval.evalComplex_1'('RadExpr.Inv'(_7878956),_7915210,_7915216,_7915222):-makeShare(_7878990,_7915908),makeShare(_7879008,_7915928),makeShare(_7879026,_7915948),makeShare(_7879044,_7915968),hnf('Prelude.cond'(letrec4PAKCS(_7915908,'Eval.evalComplex'(_7878956)),'Prelude.cond'(letrec4PAKCS(_7915928,'Eval.evalComplex._\'23selFP17\'23re'(_7915908)),'Prelude.cond'(letrec4PAKCS(_7915948,'Eval.evalComplex._\'23selFP18\'23im'(_7915908)),'Prelude.cond'(letrec4PAKCS(_7915968,'Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_7915928,_7915928),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_7915948,_7915948))),'Prelude.(,)'('Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(_7915928,_7915968),'Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Float\'23'(_7915948),_7915968)))))),_7915210,_7915216,_7915222).
'blocked_blocked_Eval.evalComplex_1'('RadExpr.Root'(_7881664,_7881682),_7923932,_7923938,_7923944):-hnf('Eval.complexNthRoot'(_7881664,'Eval.evalComplex'(_7881682)),_7923932,_7923938,_7923944).
'blocked_blocked_Eval.evalComplex_1'('RadExpr.Pow'(_7882036,_7882054),_7928566,_7928572,_7928578):-!,makeShare(_7882054,_7926186),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'3D\'23Prelude.Ord\'23Prelude.Int\'23',_7926186),0),_7930342,_7928572,_7930308),'blocked_blocked_Eval.evalComplex_1_RadExpr.Pow_ComplexCase'(_7930342,_7882036,_7926186,_7928566,_7930308,_7928578).

'blocked_blocked_Eval.evalComplex_1_RadExpr.Pow_ComplexCase'(_7930890,_7930892,_7930894,_7930896,_7930898,_7930900):-freeze(_7930898,freeze(_7930890,'blocked_blocked_blocked_Eval.evalComplex_1_RadExpr.Pow_ComplexCase'(_7930890,_7930892,_7930894,_7930896,_7930898,_7930900))).
'blocked_blocked_blocked_Eval.evalComplex_1_RadExpr.Pow_ComplexCase'('Prelude.True',_7882036,_7926186,_7931294,_7931300,_7931306):-hnf('Eval.complexPow'('Eval.evalComplex'(_7882036),_7926186),_7931294,_7931300,_7931306).
'blocked_blocked_blocked_Eval.evalComplex_1_RadExpr.Pow_ComplexCase'('Prelude.False',_7882036,_7926186,_7932780,_7932786,_7932792):-!,makeShare(_7882838,_7933668),makeShare(_7882856,_7933688),makeShare(_7882874,_7933708),makeShare(_7882892,_7933728),hnf('Prelude.cond'(letrec4PAKCS(_7933668,'Eval.complexPow'('Eval.evalComplex'(_7882036),'Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_7926186))),'Prelude.cond'(letrec4PAKCS(_7933688,'Eval.evalComplex._\'23selFP20\'23zr'(_7933668)),'Prelude.cond'(letrec4PAKCS(_7933708,'Eval.evalComplex._\'23selFP21\'23zi'(_7933668)),'Prelude.cond'(letrec4PAKCS(_7933728,'Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_7933688,_7933688),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_7933708,_7933708))),'Prelude.(,)'('Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(_7933688,_7933728),'Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Float\'23'(_7933708),_7933728)))))),_7932780,_7932786,_7932792).
'blocked_blocked_blocked_Eval.evalComplex_1_RadExpr.Pow_ComplexCase'('FAIL'(_7942364),_7882036,_7926186,'FAIL'(_7942364),_7942378,_7942378).
'blocked_blocked_Eval.evalComplex_1'('FAIL'(_7942446),'FAIL'(_7942446),_7942460,_7942460).

'Eval.evalComplex._\'23selFP2\'23re'(_7943760,_7943762,_7943764,_7943766):-freeze(_7943764,'blocked_Eval.evalComplex._\'23selFP2\'23re'(_7943760,_7943762,_7943764,_7943766)).
'blocked_Eval.evalComplex._\'23selFP2\'23re'(_7943846,_7944284,_7944290,_7944296):-hnf(_7943846,_7945814,_7944290,_7945826),'blocked_Eval.evalComplex._\'23selFP2\'23re_1'(_7945814,_7944284,_7945826,_7944296).

'blocked_Eval.evalComplex._\'23selFP2\'23re_1'(_7946260,_7946262,_7946264,_7946266):-freeze(_7946264,'blocked_blocked_Eval.evalComplex._\'23selFP2\'23re_1'(_7946260,_7946262,_7946264,_7946266)).
'blocked_blocked_Eval.evalComplex._\'23selFP2\'23re_1'('Prelude.(,)'(_7943962,_7943980),_7946630,_7946636,_7946642):-!,hnf(_7943962,_7946630,_7946636,_7946642).
'blocked_blocked_Eval.evalComplex._\'23selFP2\'23re_1'('FAIL'(_7947202),'FAIL'(_7947202),_7947216,_7947216):-nonvar(_7947202).

'Eval.evalComplex._\'23selFP3\'23im'(_7948520,_7948522,_7948524,_7948526):-freeze(_7948524,'blocked_Eval.evalComplex._\'23selFP3\'23im'(_7948520,_7948522,_7948524,_7948526)).
'blocked_Eval.evalComplex._\'23selFP3\'23im'(_7948606,_7949044,_7949050,_7949056):-hnf(_7948606,_7950574,_7949050,_7950586),'blocked_Eval.evalComplex._\'23selFP3\'23im_1'(_7950574,_7949044,_7950586,_7949056).

'blocked_Eval.evalComplex._\'23selFP3\'23im_1'(_7951020,_7951022,_7951024,_7951026):-freeze(_7951024,'blocked_blocked_Eval.evalComplex._\'23selFP3\'23im_1'(_7951020,_7951022,_7951024,_7951026)).
'blocked_blocked_Eval.evalComplex._\'23selFP3\'23im_1'('Prelude.(,)'(_7948722,_7948740),_7951390,_7951396,_7951402):-!,hnf(_7948740,_7951390,_7951396,_7951402).
'blocked_blocked_Eval.evalComplex._\'23selFP3\'23im_1'('FAIL'(_7951962),'FAIL'(_7951962),_7951976,_7951976):-nonvar(_7951962).

'Eval.evalComplex._\'23selFP8\'23ar'(_7953280,_7953282,_7953284,_7953286):-freeze(_7953284,'blocked_Eval.evalComplex._\'23selFP8\'23ar'(_7953280,_7953282,_7953284,_7953286)).
'blocked_Eval.evalComplex._\'23selFP8\'23ar'(_7953366,_7953804,_7953810,_7953816):-hnf(_7953366,_7955334,_7953810,_7955346),'blocked_Eval.evalComplex._\'23selFP8\'23ar_1'(_7955334,_7953804,_7955346,_7953816).

'blocked_Eval.evalComplex._\'23selFP8\'23ar_1'(_7955780,_7955782,_7955784,_7955786):-freeze(_7955784,'blocked_blocked_Eval.evalComplex._\'23selFP8\'23ar_1'(_7955780,_7955782,_7955784,_7955786)).
'blocked_blocked_Eval.evalComplex._\'23selFP8\'23ar_1'('Prelude.(,)'(_7953482,_7953500),_7956150,_7956156,_7956162):-!,hnf(_7953482,_7956150,_7956156,_7956162).
'blocked_blocked_Eval.evalComplex._\'23selFP8\'23ar_1'('FAIL'(_7956722),'FAIL'(_7956722),_7956736,_7956736):-nonvar(_7956722).

'Eval.evalComplex._\'23selFP9\'23ai'(_7958040,_7958042,_7958044,_7958046):-freeze(_7958044,'blocked_Eval.evalComplex._\'23selFP9\'23ai'(_7958040,_7958042,_7958044,_7958046)).
'blocked_Eval.evalComplex._\'23selFP9\'23ai'(_7958126,_7958564,_7958570,_7958576):-hnf(_7958126,_7960094,_7958570,_7960106),'blocked_Eval.evalComplex._\'23selFP9\'23ai_1'(_7960094,_7958564,_7960106,_7958576).

'blocked_Eval.evalComplex._\'23selFP9\'23ai_1'(_7960540,_7960542,_7960544,_7960546):-freeze(_7960544,'blocked_blocked_Eval.evalComplex._\'23selFP9\'23ai_1'(_7960540,_7960542,_7960544,_7960546)).
'blocked_blocked_Eval.evalComplex._\'23selFP9\'23ai_1'('Prelude.(,)'(_7958242,_7958260),_7960910,_7960916,_7960922):-!,hnf(_7958260,_7960910,_7960916,_7960922).
'blocked_blocked_Eval.evalComplex._\'23selFP9\'23ai_1'('FAIL'(_7961482),'FAIL'(_7961482),_7961496,_7961496):-nonvar(_7961482).

'Eval.evalComplex._\'23selFP6\'23br'(_7962800,_7962802,_7962804,_7962806):-freeze(_7962804,'blocked_Eval.evalComplex._\'23selFP6\'23br'(_7962800,_7962802,_7962804,_7962806)).
'blocked_Eval.evalComplex._\'23selFP6\'23br'(_7962886,_7963324,_7963330,_7963336):-hnf(_7962886,_7964854,_7963330,_7964866),'blocked_Eval.evalComplex._\'23selFP6\'23br_1'(_7964854,_7963324,_7964866,_7963336).

'blocked_Eval.evalComplex._\'23selFP6\'23br_1'(_7965300,_7965302,_7965304,_7965306):-freeze(_7965304,'blocked_blocked_Eval.evalComplex._\'23selFP6\'23br_1'(_7965300,_7965302,_7965304,_7965306)).
'blocked_blocked_Eval.evalComplex._\'23selFP6\'23br_1'('Prelude.(,)'(_7963002,_7963020),_7965670,_7965676,_7965682):-!,hnf(_7963002,_7965670,_7965676,_7965682).
'blocked_blocked_Eval.evalComplex._\'23selFP6\'23br_1'('FAIL'(_7966242),'FAIL'(_7966242),_7966256,_7966256):-nonvar(_7966242).

'Eval.evalComplex._\'23selFP7\'23bi'(_7967560,_7967562,_7967564,_7967566):-freeze(_7967564,'blocked_Eval.evalComplex._\'23selFP7\'23bi'(_7967560,_7967562,_7967564,_7967566)).
'blocked_Eval.evalComplex._\'23selFP7\'23bi'(_7967646,_7968084,_7968090,_7968096):-hnf(_7967646,_7969614,_7968090,_7969626),'blocked_Eval.evalComplex._\'23selFP7\'23bi_1'(_7969614,_7968084,_7969626,_7968096).

'blocked_Eval.evalComplex._\'23selFP7\'23bi_1'(_7970060,_7970062,_7970064,_7970066):-freeze(_7970064,'blocked_blocked_Eval.evalComplex._\'23selFP7\'23bi_1'(_7970060,_7970062,_7970064,_7970066)).
'blocked_blocked_Eval.evalComplex._\'23selFP7\'23bi_1'('Prelude.(,)'(_7967762,_7967780),_7970430,_7970436,_7970442):-!,hnf(_7967780,_7970430,_7970436,_7970442).
'blocked_blocked_Eval.evalComplex._\'23selFP7\'23bi_1'('FAIL'(_7971002),'FAIL'(_7971002),_7971016,_7971016):-nonvar(_7971002).

'Eval.evalComplex._\'23selFP14\'23ar'(_7972358,_7972360,_7972362,_7972364):-freeze(_7972362,'blocked_Eval.evalComplex._\'23selFP14\'23ar'(_7972358,_7972360,_7972362,_7972364)).
'blocked_Eval.evalComplex._\'23selFP14\'23ar'(_7972444,_7972888,_7972894,_7972900):-hnf(_7972444,_7974454,_7972894,_7974466),'blocked_Eval.evalComplex._\'23selFP14\'23ar_1'(_7974454,_7972888,_7974466,_7972900).

'blocked_Eval.evalComplex._\'23selFP14\'23ar_1'(_7974906,_7974908,_7974910,_7974912):-freeze(_7974910,'blocked_blocked_Eval.evalComplex._\'23selFP14\'23ar_1'(_7974906,_7974908,_7974910,_7974912)).
'blocked_blocked_Eval.evalComplex._\'23selFP14\'23ar_1'('Prelude.(,)'(_7972560,_7972578),_7975276,_7975282,_7975288):-!,hnf(_7972560,_7975276,_7975282,_7975288).
'blocked_blocked_Eval.evalComplex._\'23selFP14\'23ar_1'('FAIL'(_7975854),'FAIL'(_7975854),_7975868,_7975868):-nonvar(_7975854).

'Eval.evalComplex._\'23selFP15\'23ai'(_7977210,_7977212,_7977214,_7977216):-freeze(_7977214,'blocked_Eval.evalComplex._\'23selFP15\'23ai'(_7977210,_7977212,_7977214,_7977216)).
'blocked_Eval.evalComplex._\'23selFP15\'23ai'(_7977296,_7977740,_7977746,_7977752):-hnf(_7977296,_7979306,_7977746,_7979318),'blocked_Eval.evalComplex._\'23selFP15\'23ai_1'(_7979306,_7977740,_7979318,_7977752).

'blocked_Eval.evalComplex._\'23selFP15\'23ai_1'(_7979758,_7979760,_7979762,_7979764):-freeze(_7979762,'blocked_blocked_Eval.evalComplex._\'23selFP15\'23ai_1'(_7979758,_7979760,_7979762,_7979764)).
'blocked_blocked_Eval.evalComplex._\'23selFP15\'23ai_1'('Prelude.(,)'(_7977412,_7977430),_7980128,_7980134,_7980140):-!,hnf(_7977430,_7980128,_7980134,_7980140).
'blocked_blocked_Eval.evalComplex._\'23selFP15\'23ai_1'('FAIL'(_7980706),'FAIL'(_7980706),_7980720,_7980720):-nonvar(_7980706).

'Eval.evalComplex._\'23selFP12\'23br'(_7982062,_7982064,_7982066,_7982068):-freeze(_7982066,'blocked_Eval.evalComplex._\'23selFP12\'23br'(_7982062,_7982064,_7982066,_7982068)).
'blocked_Eval.evalComplex._\'23selFP12\'23br'(_7982148,_7982592,_7982598,_7982604):-hnf(_7982148,_7984158,_7982598,_7984170),'blocked_Eval.evalComplex._\'23selFP12\'23br_1'(_7984158,_7982592,_7984170,_7982604).

'blocked_Eval.evalComplex._\'23selFP12\'23br_1'(_7984610,_7984612,_7984614,_7984616):-freeze(_7984614,'blocked_blocked_Eval.evalComplex._\'23selFP12\'23br_1'(_7984610,_7984612,_7984614,_7984616)).
'blocked_blocked_Eval.evalComplex._\'23selFP12\'23br_1'('Prelude.(,)'(_7982264,_7982282),_7984980,_7984986,_7984992):-!,hnf(_7982264,_7984980,_7984986,_7984992).
'blocked_blocked_Eval.evalComplex._\'23selFP12\'23br_1'('FAIL'(_7985558),'FAIL'(_7985558),_7985572,_7985572):-nonvar(_7985558).

'Eval.evalComplex._\'23selFP13\'23bi'(_7986914,_7986916,_7986918,_7986920):-freeze(_7986918,'blocked_Eval.evalComplex._\'23selFP13\'23bi'(_7986914,_7986916,_7986918,_7986920)).
'blocked_Eval.evalComplex._\'23selFP13\'23bi'(_7987000,_7987444,_7987450,_7987456):-hnf(_7987000,_7989010,_7987450,_7989022),'blocked_Eval.evalComplex._\'23selFP13\'23bi_1'(_7989010,_7987444,_7989022,_7987456).

'blocked_Eval.evalComplex._\'23selFP13\'23bi_1'(_7989462,_7989464,_7989466,_7989468):-freeze(_7989466,'blocked_blocked_Eval.evalComplex._\'23selFP13\'23bi_1'(_7989462,_7989464,_7989466,_7989468)).
'blocked_blocked_Eval.evalComplex._\'23selFP13\'23bi_1'('Prelude.(,)'(_7987116,_7987134),_7989832,_7989838,_7989844):-!,hnf(_7987134,_7989832,_7989838,_7989844).
'blocked_blocked_Eval.evalComplex._\'23selFP13\'23bi_1'('FAIL'(_7990410),'FAIL'(_7990410),_7990424,_7990424):-nonvar(_7990410).

'Eval.evalComplex._\'23selFP17\'23re'(_7991766,_7991768,_7991770,_7991772):-freeze(_7991770,'blocked_Eval.evalComplex._\'23selFP17\'23re'(_7991766,_7991768,_7991770,_7991772)).
'blocked_Eval.evalComplex._\'23selFP17\'23re'(_7991852,_7992296,_7992302,_7992308):-hnf(_7991852,_7993862,_7992302,_7993874),'blocked_Eval.evalComplex._\'23selFP17\'23re_1'(_7993862,_7992296,_7993874,_7992308).

'blocked_Eval.evalComplex._\'23selFP17\'23re_1'(_7994314,_7994316,_7994318,_7994320):-freeze(_7994318,'blocked_blocked_Eval.evalComplex._\'23selFP17\'23re_1'(_7994314,_7994316,_7994318,_7994320)).
'blocked_blocked_Eval.evalComplex._\'23selFP17\'23re_1'('Prelude.(,)'(_7991968,_7991986),_7994684,_7994690,_7994696):-!,hnf(_7991968,_7994684,_7994690,_7994696).
'blocked_blocked_Eval.evalComplex._\'23selFP17\'23re_1'('FAIL'(_7995262),'FAIL'(_7995262),_7995276,_7995276):-nonvar(_7995262).

'Eval.evalComplex._\'23selFP18\'23im'(_7996618,_7996620,_7996622,_7996624):-freeze(_7996622,'blocked_Eval.evalComplex._\'23selFP18\'23im'(_7996618,_7996620,_7996622,_7996624)).
'blocked_Eval.evalComplex._\'23selFP18\'23im'(_7996704,_7997148,_7997154,_7997160):-hnf(_7996704,_7998714,_7997154,_7998726),'blocked_Eval.evalComplex._\'23selFP18\'23im_1'(_7998714,_7997148,_7998726,_7997160).

'blocked_Eval.evalComplex._\'23selFP18\'23im_1'(_7999166,_7999168,_7999170,_7999172):-freeze(_7999170,'blocked_blocked_Eval.evalComplex._\'23selFP18\'23im_1'(_7999166,_7999168,_7999170,_7999172)).
'blocked_blocked_Eval.evalComplex._\'23selFP18\'23im_1'('Prelude.(,)'(_7996820,_7996838),_7999536,_7999542,_7999548):-!,hnf(_7996838,_7999536,_7999542,_7999548).
'blocked_blocked_Eval.evalComplex._\'23selFP18\'23im_1'('FAIL'(_8000114),'FAIL'(_8000114),_8000128,_8000128):-nonvar(_8000114).

'Eval.evalComplex._\'23selFP20\'23zr'(_8001470,_8001472,_8001474,_8001476):-freeze(_8001474,'blocked_Eval.evalComplex._\'23selFP20\'23zr'(_8001470,_8001472,_8001474,_8001476)).
'blocked_Eval.evalComplex._\'23selFP20\'23zr'(_8001556,_8002000,_8002006,_8002012):-hnf(_8001556,_8003566,_8002006,_8003578),'blocked_Eval.evalComplex._\'23selFP20\'23zr_1'(_8003566,_8002000,_8003578,_8002012).

'blocked_Eval.evalComplex._\'23selFP20\'23zr_1'(_8004018,_8004020,_8004022,_8004024):-freeze(_8004022,'blocked_blocked_Eval.evalComplex._\'23selFP20\'23zr_1'(_8004018,_8004020,_8004022,_8004024)).
'blocked_blocked_Eval.evalComplex._\'23selFP20\'23zr_1'('Prelude.(,)'(_8001672,_8001690),_8004388,_8004394,_8004400):-!,hnf(_8001672,_8004388,_8004394,_8004400).
'blocked_blocked_Eval.evalComplex._\'23selFP20\'23zr_1'('FAIL'(_8004966),'FAIL'(_8004966),_8004980,_8004980):-nonvar(_8004966).

'Eval.evalComplex._\'23selFP21\'23zi'(_8006322,_8006324,_8006326,_8006328):-freeze(_8006326,'blocked_Eval.evalComplex._\'23selFP21\'23zi'(_8006322,_8006324,_8006326,_8006328)).
'blocked_Eval.evalComplex._\'23selFP21\'23zi'(_8006408,_8006852,_8006858,_8006864):-hnf(_8006408,_8008418,_8006858,_8008430),'blocked_Eval.evalComplex._\'23selFP21\'23zi_1'(_8008418,_8006852,_8008430,_8006864).

'blocked_Eval.evalComplex._\'23selFP21\'23zi_1'(_8008870,_8008872,_8008874,_8008876):-freeze(_8008874,'blocked_blocked_Eval.evalComplex._\'23selFP21\'23zi_1'(_8008870,_8008872,_8008874,_8008876)).
'blocked_blocked_Eval.evalComplex._\'23selFP21\'23zi_1'('Prelude.(,)'(_8006524,_8006542),_8009240,_8009246,_8009252):-!,hnf(_8006542,_8009240,_8009246,_8009252).
'blocked_blocked_Eval.evalComplex._\'23selFP21\'23zi_1'('FAIL'(_8009818),'FAIL'(_8009818),_8009832,_8009832):-nonvar(_8009818).

'Eval.complexPow'(_8010586,_8010588,_8010590,_8010592,_8010594):-freeze(_8010592,'blocked_Eval.complexPow'(_8010586,_8010588,_8010590,_8010592,_8010594)).
'blocked_Eval.complexPow'(_8010682,_8010700,_8024124,_8024130,_8024136):-makeShare(_8010700,_8020820),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_8020820,0),_8025152,_8024130,_8025118),'blocked_Eval.complexPow_ComplexCase'(_8025152,_8010682,_8020820,_8024124,_8025118,_8024136).

'blocked_Eval.complexPow_ComplexCase'(_8025544,_8025546,_8025548,_8025550,_8025552,_8025554):-freeze(_8025552,freeze(_8025544,'blocked_blocked_Eval.complexPow_ComplexCase'(_8025544,_8025546,_8025548,_8025550,_8025552,_8025554))).
'blocked_blocked_Eval.complexPow_ComplexCase'('Prelude.True',_8010682,_8020820,'Prelude.(,)'(1.0,0.0),_8025954,_8025954).
'blocked_blocked_Eval.complexPow_ComplexCase'('Prelude.False',_8010682,_8020820,_8032562,_8032568,_8032574):-!,makeShare(_8020820,_8029356),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_8029356,1),_8034738,_8032568,_8034704),'blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase'(_8034738,_8010682,_8029356,_8032562,_8034704,_8032574).

'blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase'(_8035352,_8035354,_8035356,_8035358,_8035360,_8035362):-freeze(_8035360,freeze(_8035352,'blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase'(_8035352,_8035354,_8035356,_8035358,_8035360,_8035362))).
'blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8010682,_8029356,_8035756,_8035762,_8035768):-hnf(_8010682,_8035756,_8035762,_8035768).
'blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8010682,_8029356,_8042420,_8042426,_8042432):-!,makeShare(_8029356,_8039122),hnf('Prelude.even'(partcall(1,'Prelude._inst\'23Prelude.Integral\'23Prelude.Int\'23',[]),_8039122),_8045820,_8042426,_8045786),'blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8045820,_8010682,_8039122,_8042420,_8045786,_8042432).

'blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8046638,_8046640,_8046642,_8046644,_8046646,_8046648):-freeze(_8046646,freeze(_8046638,'blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8046638,_8046640,_8046642,_8046644,_8046646,_8046648))).
'blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8010682,_8039122,_8047042,_8047048,_8047054):-makeShare(_8011824,_8047708),makeShare(_8011842,_8047728),makeShare(_8011860,_8047748),hnf('Prelude.cond'(letrec4PAKCS(_8047708,'Eval.complexPow'(_8010682,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23div\'23Prelude.Integral\'23Prelude.Int\'23',_8039122),2))),'Prelude.cond'(letrec4PAKCS(_8047728,'Eval.complexPow._\'23selFP23\'23hr'(_8047708)),'Prelude.cond'(letrec4PAKCS(_8047748,'Eval.complexPow._\'23selFP24\'23hi'(_8047708)),'Prelude.(,)'('Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8047728,_8047728),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8047748,_8047748)),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(2.0,_8047728),_8047748))))),_8047042,_8047048,_8047054).
'blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8010682,_8039122,_8058912,_8058918,_8058924):-!,hnf('Prelude.otherwise',_8063536,_8058918,_8063502),'blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8063536,_8010682,_8039122,_8058912,_8063502,_8058924).

'blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8064546,_8064548,_8064550,_8064552,_8064554,_8064556):-freeze(_8064554,freeze(_8064546,'blocked_blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8064546,_8064548,_8064550,_8064552,_8064554,_8064556))).
'blocked_blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8010682,_8039122,_8064950,_8064956,_8064962):-makeShare(_8014596,_8065996),makeShare(_8010682,_8066016),makeShare(_8014614,_8066036),makeShare(_8014632,_8066056),makeShare(_8014650,_8066076),makeShare(_8014668,_8066096),hnf('Prelude.cond'(letrec4PAKCS(_8065996,'Eval.complexPow._\'23selFP29\'23zr'(_8066016)),'Prelude.cond'(letrec4PAKCS(_8066036,'Eval.complexPow._\'23selFP30\'23zi'(_8066016)),'Prelude.cond'(letrec4PAKCS(_8066056,'Eval.complexPow'(_8066016,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_8039122,1))),'Prelude.cond'(letrec4PAKCS(_8066076,'Eval.complexPow._\'23selFP27\'23rr'(_8066056)),'Prelude.cond'(letrec4PAKCS(_8066096,'Eval.complexPow._\'23selFP28\'23ri'(_8066056)),'Prelude.(,)'('Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8065996,_8066076),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8066036,_8066096)),'Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8065996,_8066096),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8066036,_8066076)))))))),_8064950,_8064956,_8064962).
'blocked_blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8010682,_8039122,_8076934,_8076940,_8076946):-!,hnf(reportFailure4PAKCS('Eval.complexPow',['Prelude.False']),_8076934,_8076940,_8076946).
'blocked_blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8078820),_8010682,_8039122,'FAIL'(_8078820),_8078834,_8078834).
'blocked_blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8078902),_8010682,_8039122,'FAIL'(_8078902),_8078916,_8078916).
'blocked_blocked_blocked_Eval.complexPow_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8078984),_8010682,_8029356,'FAIL'(_8078984),_8078998,_8078998).
'blocked_blocked_Eval.complexPow_ComplexCase'('FAIL'(_8079066),_8010682,_8020820,'FAIL'(_8079066),_8079080,_8079080).

'Eval.complexPow._\'23selFP23\'23hr'(_8080396,_8080398,_8080400,_8080402):-freeze(_8080400,'blocked_Eval.complexPow._\'23selFP23\'23hr'(_8080396,_8080398,_8080400,_8080402)).
'blocked_Eval.complexPow._\'23selFP23\'23hr'(_8080482,_8080920,_8080926,_8080932):-hnf(_8080482,_8082450,_8080926,_8082462),'blocked_Eval.complexPow._\'23selFP23\'23hr_1'(_8082450,_8080920,_8082462,_8080932).

'blocked_Eval.complexPow._\'23selFP23\'23hr_1'(_8082896,_8082898,_8082900,_8082902):-freeze(_8082900,'blocked_blocked_Eval.complexPow._\'23selFP23\'23hr_1'(_8082896,_8082898,_8082900,_8082902)).
'blocked_blocked_Eval.complexPow._\'23selFP23\'23hr_1'('Prelude.(,)'(_8080598,_8080616),_8083266,_8083272,_8083278):-!,hnf(_8080598,_8083266,_8083272,_8083278).
'blocked_blocked_Eval.complexPow._\'23selFP23\'23hr_1'('FAIL'(_8083838),'FAIL'(_8083838),_8083852,_8083852):-nonvar(_8083838).

'Eval.complexPow._\'23selFP24\'23hi'(_8085156,_8085158,_8085160,_8085162):-freeze(_8085160,'blocked_Eval.complexPow._\'23selFP24\'23hi'(_8085156,_8085158,_8085160,_8085162)).
'blocked_Eval.complexPow._\'23selFP24\'23hi'(_8085242,_8085680,_8085686,_8085692):-hnf(_8085242,_8087210,_8085686,_8087222),'blocked_Eval.complexPow._\'23selFP24\'23hi_1'(_8087210,_8085680,_8087222,_8085692).

'blocked_Eval.complexPow._\'23selFP24\'23hi_1'(_8087656,_8087658,_8087660,_8087662):-freeze(_8087660,'blocked_blocked_Eval.complexPow._\'23selFP24\'23hi_1'(_8087656,_8087658,_8087660,_8087662)).
'blocked_blocked_Eval.complexPow._\'23selFP24\'23hi_1'('Prelude.(,)'(_8085358,_8085376),_8088026,_8088032,_8088038):-!,hnf(_8085376,_8088026,_8088032,_8088038).
'blocked_blocked_Eval.complexPow._\'23selFP24\'23hi_1'('FAIL'(_8088598),'FAIL'(_8088598),_8088612,_8088612):-nonvar(_8088598).

'Eval.complexPow._\'23selFP29\'23zr'(_8089916,_8089918,_8089920,_8089922):-freeze(_8089920,'blocked_Eval.complexPow._\'23selFP29\'23zr'(_8089916,_8089918,_8089920,_8089922)).
'blocked_Eval.complexPow._\'23selFP29\'23zr'(_8090002,_8090440,_8090446,_8090452):-hnf(_8090002,_8091970,_8090446,_8091982),'blocked_Eval.complexPow._\'23selFP29\'23zr_1'(_8091970,_8090440,_8091982,_8090452).

'blocked_Eval.complexPow._\'23selFP29\'23zr_1'(_8092416,_8092418,_8092420,_8092422):-freeze(_8092420,'blocked_blocked_Eval.complexPow._\'23selFP29\'23zr_1'(_8092416,_8092418,_8092420,_8092422)).
'blocked_blocked_Eval.complexPow._\'23selFP29\'23zr_1'('Prelude.(,)'(_8090118,_8090136),_8092786,_8092792,_8092798):-!,hnf(_8090118,_8092786,_8092792,_8092798).
'blocked_blocked_Eval.complexPow._\'23selFP29\'23zr_1'('FAIL'(_8093358),'FAIL'(_8093358),_8093372,_8093372):-nonvar(_8093358).

'Eval.complexPow._\'23selFP30\'23zi'(_8094676,_8094678,_8094680,_8094682):-freeze(_8094680,'blocked_Eval.complexPow._\'23selFP30\'23zi'(_8094676,_8094678,_8094680,_8094682)).
'blocked_Eval.complexPow._\'23selFP30\'23zi'(_8094762,_8095200,_8095206,_8095212):-hnf(_8094762,_8096730,_8095206,_8096742),'blocked_Eval.complexPow._\'23selFP30\'23zi_1'(_8096730,_8095200,_8096742,_8095212).

'blocked_Eval.complexPow._\'23selFP30\'23zi_1'(_8097176,_8097178,_8097180,_8097182):-freeze(_8097180,'blocked_blocked_Eval.complexPow._\'23selFP30\'23zi_1'(_8097176,_8097178,_8097180,_8097182)).
'blocked_blocked_Eval.complexPow._\'23selFP30\'23zi_1'('Prelude.(,)'(_8094878,_8094896),_8097546,_8097552,_8097558):-!,hnf(_8094896,_8097546,_8097552,_8097558).
'blocked_blocked_Eval.complexPow._\'23selFP30\'23zi_1'('FAIL'(_8098118),'FAIL'(_8098118),_8098132,_8098132):-nonvar(_8098118).

'Eval.complexPow._\'23selFP27\'23rr'(_8099436,_8099438,_8099440,_8099442):-freeze(_8099440,'blocked_Eval.complexPow._\'23selFP27\'23rr'(_8099436,_8099438,_8099440,_8099442)).
'blocked_Eval.complexPow._\'23selFP27\'23rr'(_8099522,_8099960,_8099966,_8099972):-hnf(_8099522,_8101490,_8099966,_8101502),'blocked_Eval.complexPow._\'23selFP27\'23rr_1'(_8101490,_8099960,_8101502,_8099972).

'blocked_Eval.complexPow._\'23selFP27\'23rr_1'(_8101936,_8101938,_8101940,_8101942):-freeze(_8101940,'blocked_blocked_Eval.complexPow._\'23selFP27\'23rr_1'(_8101936,_8101938,_8101940,_8101942)).
'blocked_blocked_Eval.complexPow._\'23selFP27\'23rr_1'('Prelude.(,)'(_8099638,_8099656),_8102306,_8102312,_8102318):-!,hnf(_8099638,_8102306,_8102312,_8102318).
'blocked_blocked_Eval.complexPow._\'23selFP27\'23rr_1'('FAIL'(_8102878),'FAIL'(_8102878),_8102892,_8102892):-nonvar(_8102878).

'Eval.complexPow._\'23selFP28\'23ri'(_8104196,_8104198,_8104200,_8104202):-freeze(_8104200,'blocked_Eval.complexPow._\'23selFP28\'23ri'(_8104196,_8104198,_8104200,_8104202)).
'blocked_Eval.complexPow._\'23selFP28\'23ri'(_8104282,_8104720,_8104726,_8104732):-hnf(_8104282,_8106250,_8104726,_8106262),'blocked_Eval.complexPow._\'23selFP28\'23ri_1'(_8106250,_8104720,_8106262,_8104732).

'blocked_Eval.complexPow._\'23selFP28\'23ri_1'(_8106696,_8106698,_8106700,_8106702):-freeze(_8106700,'blocked_blocked_Eval.complexPow._\'23selFP28\'23ri_1'(_8106696,_8106698,_8106700,_8106702)).
'blocked_blocked_Eval.complexPow._\'23selFP28\'23ri_1'('Prelude.(,)'(_8104398,_8104416),_8107066,_8107072,_8107078):-!,hnf(_8104416,_8107066,_8107072,_8107078).
'blocked_blocked_Eval.complexPow._\'23selFP28\'23ri_1'('FAIL'(_8107638),'FAIL'(_8107638),_8107652,_8107652):-nonvar(_8107638).

'Eval.floatAtan2'(_8108406,_8108408,_8108410,_8108412,_8108414):-freeze(_8108412,'blocked_Eval.floatAtan2'(_8108406,_8108408,_8108410,_8108412,_8108414)).
'blocked_Eval.floatAtan2'(_8108502,_8108520,_8118610,_8118616,_8118622):-makeShare(_8108520,_8115398),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Float\'23',_8115398),0.0),_8119638,_8118616,_8119604),'blocked_Eval.floatAtan2_ComplexCase'(_8119638,_8108502,_8115398,_8118610,_8119604,_8118622).

'blocked_Eval.floatAtan2_ComplexCase'(_8120030,_8120032,_8120034,_8120036,_8120038,_8120040):-freeze(_8120038,freeze(_8120030,'blocked_blocked_Eval.floatAtan2_ComplexCase'(_8120030,_8120032,_8120034,_8120036,_8120038,_8120040))).
'blocked_blocked_Eval.floatAtan2_ComplexCase'('Prelude.True',_8108502,_8115398,_8120434,_8120440,_8120446):-hnf('Prelude.apply'('Prelude._impl\'23atan\'23Prelude.Floating\'23Prelude.Float\'23','Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(_8108502,_8115398)),_8120434,_8120440,_8120446).
'blocked_blocked_Eval.floatAtan2_ComplexCase'('Prelude.False',_8108502,_8115398,_8127294,_8127300,_8127306):-!,makeShare(_8115398,_8122886),makeShare(_8108502,_8122906),hnf('Prelude.&&'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Float\'23',_8122886),0.0),'Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'3D\'23Prelude.Ord\'23Prelude.Float\'23',_8122906),0.0)),_8129470,_8127300,_8129436),'blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase'(_8129470,_8122906,_8122886,_8127294,_8129436,_8127306).

'blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase'(_8130096,_8130098,_8130100,_8130102,_8130104,_8130106):-freeze(_8130104,freeze(_8130096,'blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase'(_8130096,_8130098,_8130100,_8130102,_8130104,_8130106))).
'blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8122906,_8122886,_8130500,_8130506,_8130512):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'('Prelude.apply'('Prelude._impl\'23atan\'23Prelude.Floating\'23Prelude.Float\'23','Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(_8122906,_8122886)),'Prelude._impl\'23pi\'23Prelude.Floating\'23Prelude.Float\'23'),_8130500,_8130506,_8130512).
'blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8122906,_8122886,_8138136,_8138142,_8138148):-!,makeShare(_8122886,_8133920),makeShare(_8122906,_8133940),hnf('Prelude.&&'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Float\'23',_8133920),0.0),'Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Float\'23',_8133940),0.0)),_8141536,_8138142,_8141502),'blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8141536,_8133940,_8133920,_8138136,_8141502,_8138148).

'blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8142366,_8142368,_8142370,_8142372,_8142374,_8142376):-freeze(_8142374,freeze(_8142366,'blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8142366,_8142368,_8142370,_8142372,_8142374,_8142376))).
'blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8133940,_8133920,_8142770,_8142776,_8142782):-hnf('Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Float\'23'('Prelude.apply'('Prelude._impl\'23atan\'23Prelude.Floating\'23Prelude.Float\'23','Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(_8133940,_8133920)),'Prelude._impl\'23pi\'23Prelude.Floating\'23Prelude.Float\'23'),_8142770,_8142776,_8142782).
'blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8133940,_8133920,_8149996,_8150002,_8150008):-!,makeShare(_8133920,_8146284),makeShare(_8133940,_8146304),hnf('Prelude.&&'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Float\'23'(_8146284,0.0),'Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Float\'23',_8146304),0.0)),_8154620,_8150002,_8154586),'blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8154620,_8146304,_8146284,_8149996,_8154586,_8150008).

'blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8155654,_8155656,_8155658,_8155660,_8155662,_8155664):-freeze(_8155662,freeze(_8155654,'blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8155654,_8155656,_8155658,_8155660,_8155662,_8155664))).
'blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8146304,_8146284,_8156058,_8156064,_8156070):-hnf('Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'('Prelude._impl\'23pi\'23Prelude.Floating\'23Prelude.Float\'23',2.0),_8156058,_8156064,_8156070).
'blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8146304,_8146284,_8161690,_8161696,_8161702):-!,hnf('Prelude.&&'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Float\'23'(_8146284,0.0),'Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Float\'23',_8146304),0.0)),_8167538,_8161696,_8167504),'blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8167538,_8146304,_8146284,_8161690,_8167504,_8161702).

'blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8168752,_8168754,_8168756,_8168758,_8168760,_8168762):-freeze(_8168760,freeze(_8168752,'blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8168752,_8168754,_8168756,_8168758,_8168760,_8168762))).
'blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8146304,_8146284,_8169156,_8169162,_8169168):-hnf('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'('Prelude._impl\'23pi\'23Prelude.Floating\'23Prelude.Float\'23',2.0)),_8169156,_8169162,_8169168).
'blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8146304,_8146284,_8173782,_8173788,_8173794):-!,hnf('Prelude.otherwise',_8180854,_8173788,_8180820),'blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8180854,_8146304,_8146284,_8173782,_8180820,_8173794).

'blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8182272,_8182274,_8182276,_8182278,_8182280,_8182282):-freeze(_8182280,freeze(_8182272,'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_8182272,_8182274,_8182276,_8182278,_8182280,_8182282))).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_8146304,_8146284,0.0,_8182682,_8182682).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_8146304,_8146284,_8184504,_8184510,_8184516):-!,hnf(reportFailure4PAKCS('Eval.floatAtan2',['Prelude.False']),_8184504,_8184510,_8184516).
'blocked_blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8186798),_8146304,_8146284,'FAIL'(_8186798),_8186812,_8186812).
'blocked_blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8186880),_8146304,_8146284,'FAIL'(_8186880),_8186894,_8186894).
'blocked_blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8186962),_8146304,_8146284,'FAIL'(_8186962),_8186976,_8186976).
'blocked_blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8187044),_8133940,_8133920,'FAIL'(_8187044),_8187058,_8187058).
'blocked_blocked_blocked_Eval.floatAtan2_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_8187126),_8122906,_8122886,'FAIL'(_8187126),_8187140,_8187140).
'blocked_blocked_Eval.floatAtan2_ComplexCase'('FAIL'(_8187208),_8108502,_8115398,'FAIL'(_8187208),_8187222,_8187222).

'Eval.complexNthRoot'(_8188140,_8188142,_8188144,_8188146,_8188148):-freeze(_8188146,'blocked_Eval.complexNthRoot'(_8188140,_8188142,_8188144,_8188146,_8188148)).
'blocked_Eval.complexNthRoot'(_8188236,_8188254,_8192340,_8192346,_8192352):-hnf(_8188254,_8193410,_8192346,_8193428),'blocked_Eval.complexNthRoot_2'(_8193410,_8188236,_8192340,_8193428,_8192352).

'blocked_Eval.complexNthRoot_2'(_8193792,_8193794,_8193796,_8193798,_8193800):-freeze(_8193798,'blocked_blocked_Eval.complexNthRoot_2'(_8193792,_8193794,_8193796,_8193798,_8193800)).
'blocked_blocked_Eval.complexNthRoot_2'('Prelude.(,)'(_8188370,_8188388),_8188236,_8194172,_8194178,_8194184):-!,makeShare(_8188428,_8195206),makeShare(_8188370,_8195226),makeShare(_8188388,_8195246),makeShare(_8188446,_8195266),makeShare(_8188464,_8195286),makeShare(_8188236,_8195306),makeShare(_8188482,_8195326),hnf('Prelude.cond'(letrec4PAKCS(_8195206,'Prelude.apply'('Prelude._impl\'23sqrt\'23Prelude.Floating\'23Prelude.Float\'23','Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Float\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8195226,_8195226),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8195246,_8195246)))),'Prelude.cond'(letrec4PAKCS(_8195266,'Eval.floatAtan2'(_8195246,_8195226)),'Prelude.cond'(letrec4PAKCS(_8195286,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'2A\'2A\'23Prelude.Floating\'23Prelude.Float\'23',_8195206),'Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(1.0,'Prelude._impl\'23fromInt\'23Prelude.Num\'23Prelude.Float\'23'(_8195306)))),'Prelude.cond'(letrec4PAKCS(_8195326,'Prelude._impl\'23\'2F\'23Prelude.Fractional\'23Prelude.Float\'23'(_8195266,'Prelude._impl\'23fromInt\'23Prelude.Num\'23Prelude.Float\'23'(_8195306))),'Prelude.(,)'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8195286,'Prelude.apply'('Prelude._impl\'23cos\'23Prelude.Floating\'23Prelude.Float\'23',_8195326)),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Float\'23'(_8195286,'Prelude.apply'('Prelude._impl\'23sin\'23Prelude.Floating\'23Prelude.Float\'23',_8195326))))))),_8194172,_8194178,_8194184).
'blocked_blocked_Eval.complexNthRoot_2'('FAIL'(_8207566),_8188236,'FAIL'(_8207566),_8207580,_8207580):-nonvar(_8207566).

:-costCenters(['']).




%%%%% Number of shared variables: 52

