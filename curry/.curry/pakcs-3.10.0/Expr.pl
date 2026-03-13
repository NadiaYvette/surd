%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Data.List').
:-importModule('Prelude').
:-importModule('RadExpr').

:-curryModule('Expr').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Expr.exprDepth',exprDepth,1,'Expr.exprDepth',nofix,notype).
functiontype('Expr.exprSize',exprSize,1,'Expr.exprSize',nofix,notype).
functiontype('Expr.freeOf',freeOf,2,'Expr.freeOf',nofix,notype).
functiontype('Expr.collectRadicals',collectRadicals,2,'Expr.collectRadicals',nofix,notype).
functiontype('Expr.goCollect','Expr.goCollect',1,'Expr.goCollect',nofix,notype).
functiontype('Expr.topoSortRadicals',topoSortRadicals,2,'Expr.topoSortRadicals',nofix,notype).
functiontype('Expr.goTopo','Expr.goTopo',3,'Expr.goTopo',nofix,notype).
functiontype('Expr.goTopo._\'23lambda5','Expr.goTopo._#lambda5',3,'Expr.goTopo._\'23lambda5',nofix,notype).
functiontype('Expr.goTopo._\'23lambda6','Expr.goTopo._#lambda6',3,'Expr.goTopo._\'23lambda6',nofix,notype).
functiontype('Expr.allRootsResolved',allRootsResolved,3,'Expr.allRootsResolved',nofix,notype).
functiontype('Expr.goTopo._\'23caseor0','Expr.goTopo._#caseor0',6,'Expr.goTopo._\'23caseor0',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Expr.exprDepth'(_4437360,_4437362,_4437364,_4437366):-freeze(_4437364,'blocked_Expr.exprDepth'(_4437360,_4437362,_4437364,_4437366)).
'blocked_Expr.exprDepth'(_4437446,_4440594,_4440600,_4440606):-hnf(_4437446,_4441476,_4440600,_4441488),'blocked_Expr.exprDepth_1'(_4441476,_4440594,_4441488,_4440606).

'blocked_Expr.exprDepth_1'(_4441820,_4441822,_4441824,_4441826):-freeze(_4441824,freeze(_4441820,'blocked_blocked_Expr.exprDepth_1'(_4441820,_4441822,_4441824,_4441826))).
'blocked_blocked_Expr.exprDepth_1'('RadExpr.Lit'(_4437562),0,_4442208,_4442208).
'blocked_blocked_Expr.exprDepth_1'('RadExpr.Neg'(_4437672),_4442848,_4442854,_4442860):-hnf('Expr.exprDepth'(_4437672),_4442848,_4442854,_4442860).
'blocked_blocked_Expr.exprDepth_1'('RadExpr.Add'(_4437866,_4437884),_4443726,_4443732,_4443738):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23max\'23Prelude.Ord\'23Prelude.Int\'23','Expr.exprDepth'(_4437866)),'Expr.exprDepth'(_4437884))),_4443726,_4443732,_4443738).
'blocked_blocked_Expr.exprDepth_1'('RadExpr.Mul'(_4438644,_4438662),_4446250,_4446256,_4446262):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23max\'23Prelude.Ord\'23Prelude.Int\'23','Expr.exprDepth'(_4438644)),'Expr.exprDepth'(_4438662))),_4446250,_4446256,_4446262).
'blocked_blocked_Expr.exprDepth_1'('RadExpr.Inv'(_4439422),_4448766,_4448772,_4448778):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprDepth'(_4439422)),_4448766,_4448772,_4448778).
'blocked_blocked_Expr.exprDepth_1'('RadExpr.Root'(_4439770,_4439788),_4450150,_4450156,_4450162):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprDepth'(_4439788)),_4450150,_4450156,_4450162).
'blocked_blocked_Expr.exprDepth_1'('RadExpr.Pow'(_4440142,_4440160),_4451588,_4451594,_4451600):-!,hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprDepth'(_4440142)),_4451588,_4451594,_4451600).
'blocked_blocked_Expr.exprDepth_1'('FAIL'(_4452740),'FAIL'(_4452740),_4452754,_4452754).

'Expr.exprSize'(_4453428,_4453430,_4453432,_4453434):-freeze(_4453432,'blocked_Expr.exprSize'(_4453428,_4453430,_4453432,_4453434)).
'blocked_Expr.exprSize'(_4453514,_4456474,_4456480,_4456486):-hnf(_4453514,_4457320,_4456480,_4457332),'blocked_Expr.exprSize_1'(_4457320,_4456474,_4457332,_4456486).

'blocked_Expr.exprSize_1'(_4457658,_4457660,_4457662,_4457664):-freeze(_4457662,freeze(_4457658,'blocked_blocked_Expr.exprSize_1'(_4457658,_4457660,_4457662,_4457664))).
'blocked_blocked_Expr.exprSize_1'('RadExpr.Lit'(_4453630),1,_4458046,_4458046).
'blocked_blocked_Expr.exprSize_1'('RadExpr.Neg'(_4453740),_4458680,_4458686,_4458692):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprSize'(_4453740)),_4458680,_4458686,_4458692).
'blocked_blocked_Expr.exprSize_1'('RadExpr.Add'(_4454088,_4454106),_4460040,_4460046,_4460052):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprSize'(_4454088)),'Expr.exprSize'(_4454106)),_4460040,_4460046,_4460052).
'blocked_blocked_Expr.exprSize_1'('RadExpr.Mul'(_4454698,_4454716),_4462172,_4462178,_4462184):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprSize'(_4454698)),'Expr.exprSize'(_4454716)),_4462172,_4462178,_4462184).
'blocked_blocked_Expr.exprSize_1'('RadExpr.Inv'(_4455308),_4464296,_4464302,_4464308):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprSize'(_4455308)),_4464296,_4464302,_4464308).
'blocked_blocked_Expr.exprSize_1'('RadExpr.Root'(_4455656,_4455674),_4465668,_4465674,_4465680):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprSize'(_4455674)),_4465668,_4465674,_4465680).
'blocked_blocked_Expr.exprSize_1'('RadExpr.Pow'(_4456028,_4456046),_4467094,_4467100,_4467106):-!,hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Expr.exprSize'(_4456028)),_4467094,_4467100,_4467106).
'blocked_blocked_Expr.exprSize_1'('FAIL'(_4468234),'FAIL'(_4468234),_4468248,_4468248).

'Expr.freeOf'(_4468846,_4468848,_4468850,_4468852,_4468854):-freeze(_4468852,'blocked_Expr.freeOf'(_4468846,_4468848,_4468850,_4468852,_4468854)).
'blocked_Expr.freeOf'(_4468942,_4468960,_4471706,_4471712,_4471718):-hnf(_4468960,_4472488,_4471712,_4472506),'blocked_Expr.freeOf_2'(_4472488,_4468942,_4471706,_4472506,_4471718).

'blocked_Expr.freeOf_2'(_4472828,_4472830,_4472832,_4472834,_4472836):-freeze(_4472834,freeze(_4472828,'blocked_blocked_Expr.freeOf_2'(_4472828,_4472830,_4472832,_4472834,_4472836))).
'blocked_blocked_Expr.freeOf_2'('RadExpr.Lit'(_4469076),_4468942,_4473220,_4473226,_4473232):-hnf('Prelude.apply'(_4468942,_4469076),_4473220,_4473226,_4473232).
'blocked_blocked_Expr.freeOf_2'('RadExpr.Neg'(_4469340),_4468942,_4474244,_4474250,_4474256):-hnf('Expr.freeOf'(_4468942,_4469340),_4474244,_4474250,_4474256).
'blocked_blocked_Expr.freeOf_2'('RadExpr.Add'(_4469604,_4469622),_4468942,_4475264,_4475270,_4475276):-makeShare(_4468942,_4475426),hnf('Prelude.&&'('Expr.freeOf'(_4475426,_4469604),'Expr.freeOf'(_4475426,_4469622)),_4475264,_4475270,_4475276).
'blocked_blocked_Expr.freeOf_2'('RadExpr.Mul'(_4470200,_4470218),_4468942,_4477206,_4477212,_4477218):-makeShare(_4468942,_4477368),hnf('Prelude.&&'('Expr.freeOf'(_4477368,_4470200),'Expr.freeOf'(_4477368,_4470218)),_4477206,_4477212,_4477218).
'blocked_blocked_Expr.freeOf_2'('RadExpr.Inv'(_4470796),_4468942,_4479140,_4479146,_4479152):-hnf('Expr.freeOf'(_4468942,_4470796),_4479140,_4479146,_4479152).
'blocked_blocked_Expr.freeOf_2'('RadExpr.Root'(_4471060,_4471078),_4468942,_4480172,_4480178,_4480184):-hnf('Expr.freeOf'(_4468942,_4471078),_4480172,_4480178,_4480184).
'blocked_blocked_Expr.freeOf_2'('RadExpr.Pow'(_4471348,_4471366),_4468942,_4481258,_4481264,_4481270):-!,hnf('Expr.freeOf'(_4468942,_4471348),_4481258,_4481264,_4481270).
'blocked_blocked_Expr.freeOf_2'('FAIL'(_4482058),_4468942,'FAIL'(_4482058),_4482072,_4482072).

'Expr.collectRadicals'(_4483020,_4483022,_4483024,_4483026,_4483028):-freeze(_4483026,'blocked_Expr.collectRadicals'(_4483020,_4483022,_4483024,_4483026,_4483028)).
'blocked_Expr.collectRadicals'(_4483116,_4483134,_4483640,_4483646,_4483652):-hnf('Data.List.nub'(partcall(1,'Prelude._inst\'23Prelude.Eq\'23\'28\'2C\'29\'230\'23\'231\'23\'23',[partcall(1,'RadExpr._inst\'23Prelude.Eq\'23RadExpr.RadExpr\'230\'23\'23',[_4483116]),partcall(1,'Prelude._inst\'23Prelude.Eq\'23Prelude.Int\'23',[])]),'Expr.goCollect'(_4483134)),_4483640,_4483646,_4483652).

'Expr.goCollect'(_4486488,_4486490,_4486492,_4486494):-freeze(_4486492,'blocked_Expr.goCollect'(_4486488,_4486490,_4486492,_4486494)).
'blocked_Expr.goCollect'(_4486574,_4489106,_4489112,_4489118):-hnf(_4486574,_4489988,_4489112,_4490000),'blocked_Expr.goCollect_1'(_4489988,_4489106,_4490000,_4489118).

'blocked_Expr.goCollect_1'(_4490332,_4490334,_4490336,_4490338):-freeze(_4490336,freeze(_4490332,'blocked_blocked_Expr.goCollect_1'(_4490332,_4490334,_4490336,_4490338))).
'blocked_blocked_Expr.goCollect_1'('RadExpr.Lit'(_4486690),[],_4490720,_4490720).
'blocked_blocked_Expr.goCollect_1'('RadExpr.Neg'(_4486814),_4491384,_4491390,_4491396):-hnf('Expr.goCollect'(_4486814),_4491384,_4491390,_4491396).
'blocked_blocked_Expr.goCollect_1'('RadExpr.Add'(_4487008,_4487026),_4492262,_4492268,_4492274):-hnf('Prelude.++'('Expr.goCollect'(_4487008),'Expr.goCollect'(_4487026)),_4492262,_4492268,_4492274).
'blocked_blocked_Expr.goCollect_1'('RadExpr.Mul'(_4487464,_4487482),_4493738,_4493744,_4493750):-hnf('Prelude.++'('Expr.goCollect'(_4487464),'Expr.goCollect'(_4487482)),_4493738,_4493744,_4493750).
'blocked_blocked_Expr.goCollect_1'('RadExpr.Inv'(_4487920),_4495206,_4495212,_4495218):-hnf('Expr.goCollect'(_4487920),_4495206,_4495212,_4495218).
'blocked_blocked_Expr.goCollect_1'('RadExpr.Pow'(_4488114,_4488132),_4496084,_4496090,_4496096):-hnf('Expr.goCollect'(_4488114),_4496084,_4496090,_4496096).
'blocked_blocked_Expr.goCollect_1'('RadExpr.Root'(_4488332,_4488350),_4497040,_4497046,_4497052):-!,makeShare(_4488350,_4497160),hnf('Prelude.++'('Expr.goCollect'(_4497160),['Prelude.(,)'(_4488332,_4497160)]),_4497040,_4497046,_4497052).
'blocked_blocked_Expr.goCollect_1'('FAIL'(_4498742),'FAIL'(_4498742),_4498756,_4498756).

'Expr.topoSortRadicals'(_4499734,_4499736,_4499738,_4499740,_4499742):-freeze(_4499740,'blocked_Expr.topoSortRadicals'(_4499734,_4499736,_4499738,_4499740,_4499742)).
'blocked_Expr.topoSortRadicals'(_4499830,_4499848,_4500102,_4500108,_4500114):-hnf('Expr.goTopo'(_4499830,[],_4499848),_4500102,_4500108,_4500114).

'Expr.goTopo'(_4501468,_4501470,_4501472,_4501474,_4501476,_4501478):-freeze(_4501476,'blocked_Expr.goTopo'(_4501468,_4501470,_4501472,_4501474,_4501476,_4501478)).
'blocked_Expr.goTopo'(_4501574,_4501592,_4501610,_4506448,_4506454,_4506460):-makeShare(_4501610,_4504886),hnf('Prelude.null'(_4504886),_4507346,_4506454,_4507306),'blocked_Expr.goTopo_ComplexCase'(_4507346,_4501574,_4501592,_4504886,_4506448,_4507306,_4506460).

'blocked_Expr.goTopo_ComplexCase'(_4507716,_4507718,_4507720,_4507722,_4507724,_4507726,_4507728):-freeze(_4507726,freeze(_4507716,'blocked_blocked_Expr.goTopo_ComplexCase'(_4507716,_4507718,_4507720,_4507722,_4507724,_4507726,_4507728))).
'blocked_blocked_Expr.goTopo_ComplexCase'('Prelude.True',_4501574,_4501592,_4504886,_4508130,_4508136,_4508142):-hnf(_4501592,_4508130,_4508136,_4508142).
'blocked_blocked_Expr.goTopo_ComplexCase'('Prelude.False',_4501574,_4501592,_4504886,_4510800,_4510806,_4510812):-!,hnf('Prelude.otherwise',_4512846,_4510806,_4512806),'blocked_blocked_Expr.goTopo_ComplexCase_Prelude.False_ComplexCase'(_4512846,_4501574,_4501592,_4504886,_4510800,_4512806,_4510812).

'blocked_blocked_Expr.goTopo_ComplexCase_Prelude.False_ComplexCase'(_4513426,_4513428,_4513430,_4513432,_4513434,_4513436,_4513438):-freeze(_4513436,freeze(_4513426,'blocked_blocked_blocked_Expr.goTopo_ComplexCase_Prelude.False_ComplexCase'(_4513426,_4513428,_4513430,_4513432,_4513434,_4513436,_4513438))).
'blocked_blocked_blocked_Expr.goTopo_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_4501574,_4501592,_4504886,_4513840,_4513846,_4513852):-makeShare(_4502018,_4514436),makeShare(_4501592,_4514456),makeShare(_4501574,_4514476),makeShare(_4504886,_4514496),makeShare(_4502036,_4514516),hnf('Prelude.cond'(letrec4PAKCS(_4514436,'Prelude.filter'(partcall(1,'Expr.goTopo._\'23lambda5',[_4514476,_4514456]),_4514496)),'Prelude.cond'(letrec4PAKCS(_4514516,'Prelude.filter'(partcall(1,'Expr.goTopo._\'23lambda6',[_4514476,_4514436]),_4514496)),'Expr.goTopo._\'23caseor0'('Prelude.null'(_4514436),_4514496,_4514476,_4514456,_4514436,_4514516))),_4513840,_4513846,_4513852).
'blocked_blocked_blocked_Expr.goTopo_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_4501574,_4501592,_4504886,_4519844,_4519850,_4519856):-!,hnf(reportFailure4PAKCS('Expr.goTopo',['Prelude.False']),_4519844,_4519850,_4519856).
'blocked_blocked_blocked_Expr.goTopo_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_4521348),_4501574,_4501592,_4504886,'FAIL'(_4521348),_4521362,_4521362).
'blocked_blocked_Expr.goTopo_ComplexCase'('FAIL'(_4521438),_4501574,_4501592,_4504886,'FAIL'(_4521438),_4521452,_4521452).

'Expr.goTopo._\'23lambda5'(_4522482,_4522484,_4522486,_4522488,_4522490,_4522492):-freeze(_4522490,'blocked_Expr.goTopo._\'23lambda5'(_4522482,_4522484,_4522486,_4522488,_4522490,_4522492)).
'blocked_Expr.goTopo._\'23lambda5'(_4522588,_4522606,_4522624,_4522948,_4522954,_4522960):-hnf('Expr.allRootsResolved'(_4522606,_4522588,'Prelude.snd'(_4522624)),_4522948,_4522954,_4522960).

'Expr.goTopo._\'23lambda6'(_4525056,_4525058,_4525060,_4525062,_4525064,_4525066):-freeze(_4525064,'blocked_Expr.goTopo._\'23lambda6'(_4525056,_4525058,_4525060,_4525062,_4525064,_4525066)).
'blocked_Expr.goTopo._\'23lambda6'(_4525162,_4525180,_4525198,_4525858,_4525864,_4525870):-hnf('Prelude.not'('Prelude.apply'('Prelude.elem'(partcall(1,'Prelude._inst\'23Prelude.Eq\'23\'28\'2C\'29\'230\'23\'231\'23\'23',[partcall(1,'RadExpr._inst\'23Prelude.Eq\'23RadExpr.RadExpr\'230\'23\'23',[_4525180]),partcall(1,'Prelude._inst\'23Prelude.Eq\'23Prelude.Int\'23',[])]),_4525198),_4525162)),_4525858,_4525864,_4525870).

'Expr.allRootsResolved'(_4529344,_4529346,_4529348,_4529350,_4529352,_4529354):-freeze(_4529352,'blocked_Expr.allRootsResolved'(_4529344,_4529346,_4529348,_4529350,_4529352,_4529354)).
'blocked_Expr.allRootsResolved'(_4529450,_4529468,_4529486,_4533210,_4533216,_4533222):-hnf(_4529486,_4534360,_4533216,_4534384),'blocked_Expr.allRootsResolved_3'(_4534360,_4529450,_4529468,_4533210,_4534384,_4533222).

'blocked_Expr.allRootsResolved_3'(_4534774,_4534776,_4534778,_4534780,_4534782,_4534784):-freeze(_4534782,freeze(_4534774,'blocked_blocked_Expr.allRootsResolved_3'(_4534774,_4534776,_4534778,_4534780,_4534782,_4534784))).
'blocked_blocked_Expr.allRootsResolved_3'('RadExpr.Lit'(_4529602),_4529450,_4529468,'Prelude.True',_4535182,_4535182).
'blocked_blocked_Expr.allRootsResolved_3'('RadExpr.Neg'(_4529726),_4529450,_4529468,_4536112,_4536118,_4536124):-hnf('Expr.allRootsResolved'(_4529450,_4529468,_4529726),_4536112,_4536118,_4536124).
'blocked_blocked_Expr.allRootsResolved_3'('RadExpr.Add'(_4530060,_4530078),_4529450,_4529468,_4537442,_4537448,_4537454):-makeShare(_4529450,_4537688),makeShare(_4529468,_4537708),hnf('Prelude.&&'('Expr.allRootsResolved'(_4537688,_4537708,_4530060),'Expr.allRootsResolved'(_4537688,_4537708,_4530078)),_4537442,_4537448,_4537454).
'blocked_blocked_Expr.allRootsResolved_3'('RadExpr.Mul'(_4530796,_4530814),_4529450,_4529468,_4540108,_4540114,_4540120):-makeShare(_4529450,_4540354),makeShare(_4529468,_4540374),hnf('Prelude.&&'('Expr.allRootsResolved'(_4540354,_4540374,_4530796),'Expr.allRootsResolved'(_4540354,_4540374,_4530814)),_4540108,_4540114,_4540120).
'blocked_blocked_Expr.allRootsResolved_3'('RadExpr.Inv'(_4531532),_4529450,_4529468,_4542766,_4542772,_4542778):-hnf('Expr.allRootsResolved'(_4529450,_4529468,_4531532),_4542766,_4542772,_4542778).
'blocked_blocked_Expr.allRootsResolved_3'('RadExpr.Pow'(_4531866,_4531884),_4529450,_4529468,_4544096,_4544102,_4544108):-hnf('Expr.allRootsResolved'(_4529450,_4529468,_4531866),_4544096,_4544102,_4544108).
'blocked_blocked_Expr.allRootsResolved_3'('RadExpr.Root'(_4532224,_4532242),_4529450,_4529468,_4545504,_4545510,_4545516):-!,hnf('Prelude.apply'('Prelude.elem'(partcall(1,'Prelude._inst\'23Prelude.Eq\'23\'28\'2C\'29\'230\'23\'231\'23\'23',[partcall(1,'RadExpr._inst\'23Prelude.Eq\'23RadExpr.RadExpr\'230\'23\'23',[_4529450]),partcall(1,'Prelude._inst\'23Prelude.Eq\'23Prelude.Int\'23',[])]),'Prelude.(,)'(_4532224,_4532242)),_4529468),_4545504,_4545510,_4545516).
'blocked_blocked_Expr.allRootsResolved_3'('FAIL'(_4548310),_4529450,_4529468,'FAIL'(_4548310),_4548324,_4548324).

'Expr.goTopo._\'23caseor0'(_4549346,_4549348,_4549350,_4549352,_4549354,_4549356,_4549358,_4549360,_4549362):-freeze(_4549360,'blocked_Expr.goTopo._\'23caseor0'(_4549346,_4549348,_4549350,_4549352,_4549354,_4549356,_4549358,_4549360,_4549362)).
'blocked_Expr.goTopo._\'23caseor0'(_4549482,_4549500,_4549518,_4549536,_4549554,_4549572,_4550526,_4550532,_4550538):-hnf(_4549482,_4551772,_4550532,_4551814),'blocked_Expr.goTopo._\'23caseor0_1'(_4551772,_4549500,_4549518,_4549536,_4549554,_4549572,_4550526,_4551814,_4550538).

'blocked_Expr.goTopo._\'23caseor0_1'(_4552240,_4552242,_4552244,_4552246,_4552248,_4552250,_4552252,_4552254,_4552256):-freeze(_4552254,freeze(_4552240,'blocked_blocked_Expr.goTopo._\'23caseor0_1'(_4552240,_4552242,_4552244,_4552246,_4552248,_4552250,_4552252,_4552254,_4552256))).
'blocked_blocked_Expr.goTopo._\'23caseor0_1'('Prelude.True',_4549500,_4549518,_4549536,_4549554,_4549572,_4552674,_4552680,_4552686):-hnf('Prelude.++'(_4549536,_4549500),_4552674,_4552680,_4552686).
'blocked_blocked_Expr.goTopo._\'23caseor0_1'('Prelude.False',_4549500,_4549518,_4549536,_4549554,_4549572,_4554060,_4554066,_4554072):-!,hnf('Expr.goTopo'(_4549518,'Prelude.++'(_4549536,_4549554),_4549572),_4554060,_4554066,_4554072).
'blocked_blocked_Expr.goTopo._\'23caseor0_1'('FAIL'(_4555574),_4549500,_4549518,_4549536,_4549554,_4549572,'FAIL'(_4555574),_4555588,_4555588).

:-costCenters(['']).




%%%%% Number of shared variables: 13

