%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').
:-importModule('RadExpr').
:-importModule('Rational').

:-curryModule('LaTeX').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('LaTeX.precAdd','LaTeX.precAdd',0,'LaTeX.precAdd',nofix,notype).
functiontype('LaTeX.precMul','LaTeX.precMul',0,'LaTeX.precMul',nofix,notype).
functiontype('LaTeX.precNeg','LaTeX.precNeg',0,'LaTeX.precNeg',nofix,notype).
functiontype('LaTeX.precPow','LaTeX.precPow',0,'LaTeX.precPow',nofix,notype).
functiontype('LaTeX.latex',latex,0,'LaTeX.latex',nofix,notype).
functiontype('LaTeX.latexPrec',latexPrec,2,'LaTeX.latexPrec',nofix,notype).
functiontype('LaTeX.latexNeg','LaTeX.latexNeg',2,'LaTeX.latexNeg',nofix,notype).
functiontype('LaTeX.negTerm','LaTeX.negTerm',1,'LaTeX.negTerm',nofix,notype).
functiontype('LaTeX.latexBase','LaTeX.latexBase',1,'LaTeX.latexBase',nofix,notype).
functiontype('LaTeX.latexRadicand','LaTeX.latexRadicand',1,'LaTeX.latexRadicand',nofix,notype).
functiontype('LaTeX.flattenAdd','LaTeX.flattenAdd',1,'LaTeX.flattenAdd',nofix,notype).
functiontype('LaTeX.flattenMul','LaTeX.flattenMul',1,'LaTeX.flattenMul',nofix,notype).
functiontype('LaTeX.rebuildMul','LaTeX.rebuildMul',1,'LaTeX.rebuildMul',nofix,notype).
functiontype('LaTeX.renderTerms','LaTeX.renderTerms',1,'LaTeX.renderTerms',nofix,notype).
functiontype('LaTeX.renderRest','LaTeX.renderRest',1,'LaTeX.renderRest',nofix,notype).
functiontype('LaTeX.renderFactors','LaTeX.renderFactors',1,'LaTeX.renderFactors',nofix,notype).
functiontype('LaTeX.joinMul','LaTeX.joinMul',1,'LaTeX.joinMul',nofix,notype).
functiontype('LaTeX.latexRat','LaTeX.latexRat',1,'LaTeX.latexRat',nofix,notype).
functiontype('LaTeX.absInt','LaTeX.absInt',1,'LaTeX.absInt',nofix,notype).
functiontype('LaTeX.parensIf','LaTeX.parensIf',2,'LaTeX.parensIf',nofix,notype).
functiontype('LaTeX.latexPrec._\'23caseor0','LaTeX.latexPrec._#caseor0',4,'LaTeX.latexPrec._\'23caseor0',nofix,notype).
functiontype('LaTeX.latexPrec._\'23caseor0._\'23caseor0','LaTeX.latexPrec._#caseor0._#caseor0',4,'LaTeX.latexPrec._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0','LaTeX.latexPrec._#caseor0._#caseor0._#caseor0',2,'LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('LaTeX.latexNeg._\'23caseor0._\'23caseor0','LaTeX.latexNeg._#caseor0._#caseor0',4,'LaTeX.latexNeg._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('LaTeX.latexNeg._\'23caseor0','LaTeX.latexNeg._#caseor0',4,'LaTeX.latexNeg._\'23caseor0',nofix,notype).
functiontype('LaTeX.latexBase._\'23caseor0','LaTeX.latexBase._#caseor0',3,'LaTeX.latexBase._\'23caseor0',nofix,notype).
functiontype('LaTeX.latexRadicand._\'23caseor0._\'23caseor0','LaTeX.latexRadicand._#caseor0._#caseor0',3,'LaTeX.latexRadicand._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('LaTeX.latexRadicand._\'23caseor0','LaTeX.latexRadicand._#caseor0',2,'LaTeX.latexRadicand._\'23caseor0',nofix,notype).
functiontype('LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0','LaTeX.flattenAdd._#caseor0._#caseor0._#caseor0',4,'LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('LaTeX.flattenAdd._\'23caseor0._\'23caseor0','LaTeX.flattenAdd._#caseor0._#caseor0',2,'LaTeX.flattenAdd._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0','LaTeX.flattenAdd._#caseor0._#caseor0._#caseor0._#caseor0',3,'LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('LaTeX.flattenAdd._\'23caseor0','LaTeX.flattenAdd._#caseor0',3,'LaTeX.flattenAdd._\'23caseor0',nofix,notype).
functiontype('LaTeX.flattenMul._\'23caseor0','LaTeX.flattenMul._#caseor0',2,'LaTeX.flattenMul._\'23caseor0',nofix,notype).
functiontype('LaTeX.renderTerms._\'23caseor0','LaTeX.renderTerms._#caseor0',2,'LaTeX.renderTerms._\'23caseor0',nofix,notype).
functiontype('LaTeX.renderFactors._\'23caseor0','LaTeX.renderFactors._#caseor0',3,'LaTeX.renderFactors._\'23caseor0',nofix,notype).
functiontype('LaTeX.latexRat._\'23caseor0','LaTeX.latexRat._#caseor0',3,'LaTeX.latexRat._\'23caseor0',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'LaTeX.precAdd'(_1177992,_1177994,_1177996):-freeze(_1177994,'blocked_LaTeX.precAdd'(_1177992,_1177994,_1177996)).
'blocked_LaTeX.precAdd'(1,_1178072,_1178072).

'LaTeX.precMul'(_1178898,_1178900,_1178902):-freeze(_1178900,'blocked_LaTeX.precMul'(_1178898,_1178900,_1178902)).
'blocked_LaTeX.precMul'(2,_1178978,_1178978).

'LaTeX.precNeg'(_1179804,_1179806,_1179808):-freeze(_1179806,'blocked_LaTeX.precNeg'(_1179804,_1179806,_1179808)).
'blocked_LaTeX.precNeg'(3,_1179884,_1179884).

'LaTeX.precPow'(_1180710,_1180712,_1180714):-freeze(_1180712,'blocked_LaTeX.precPow'(_1180710,_1180712,_1180714)).
'blocked_LaTeX.precPow'(4,_1180790,_1180790).

'LaTeX.latex'(_1181540,_1181542,_1181544):-freeze(_1181542,'blocked_LaTeX.latex'(_1181540,_1181542,_1181544)).
'blocked_LaTeX.latex'(_1181698,_1181704,_1181710):-hnf(partcall(1,'LaTeX.latexPrec',[0]),_1181698,_1181704,_1181710).

'LaTeX.latexPrec'(_1182818,_1182820,_1182822,_1182824,_1182826):-freeze(_1182824,'blocked_LaTeX.latexPrec'(_1182818,_1182820,_1182822,_1182824,_1182826)).
'blocked_LaTeX.latexPrec'(_1182914,_1182932,_1201554,_1201560,_1201566):-makeShare(_1182932,_1197514),hnf(_1197514,_1202480,_1201560,_1202504),'blocked_LaTeX.latexPrec_2'(_1202480,_1182914,_1202480,_1201554,_1202504,_1201566).

'blocked_LaTeX.latexPrec_2'(_1202870,_1202872,_1202874,_1202876,_1202878,_1202880):-freeze(_1202878,freeze(_1202870,'blocked_blocked_LaTeX.latexPrec_2'(_1202870,_1202872,_1202874,_1202876,_1202878,_1202880))).
'blocked_blocked_LaTeX.latexPrec_2'('RadExpr.Lit'(_1183048),_1182914,_1197514,_1203272,_1203278,_1203284):-hnf('LaTeX.latexRat'(_1183048),_1203272,_1203278,_1203284).
'blocked_blocked_LaTeX.latexPrec_2'('RadExpr.Neg'(_1183242),_1182914,_1197514,_1204296,_1204302,_1204308):-hnf('LaTeX.latexNeg'(_1182914,_1183242),_1204296,_1204302,_1204308).
'blocked_blocked_LaTeX.latexPrec_2'('RadExpr.Add'(_1183506,_1183524),_1182914,_1197514,_1205432,_1205438,_1205444):-hnf('LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1182914),'LaTeX.precAdd'),'LaTeX.renderTerms'('LaTeX.flattenAdd'(_1197514))),_1205432,_1205438,_1205444).
'blocked_blocked_LaTeX.latexPrec_2'('RadExpr.Mul'(_1184298,_1184316),_1182914,_1197514,_1208026,_1208032,_1208038):-makeShare(_1184356,_1208640),makeShare(_1184616,_1208660),makeShare(_1182914,_1208680),makeShare(_1184298,_1208700),makeShare(_1184316,_1208720),hnf('Prelude.cond'(letrec4PAKCS(_1208640,'Prelude.cond'(letrec4PAKCS(_1208660,'LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1208680),'LaTeX.precMul'),'LaTeX.renderFactors'('LaTeX.flattenMul'(_1197514)))),'LaTeX.latexPrec._\'23caseor0'(_1208700,_1208680,_1208720,_1208660))),'LaTeX.latexPrec._\'23caseor0._\'23caseor0'(_1208720,_1208680,_1208700,_1208640)),_1208026,_1208032,_1208038).
'blocked_blocked_LaTeX.latexPrec_2'('RadExpr.Inv'(_1186362),_1182914,_1197514,_1214632,_1214638,_1214644):-hnf('LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1182914),'LaTeX.precMul'),'Prelude.++'([^\,'^f','^r','^a','^c','^{','^1','^}','^{'],'Prelude.++'('LaTeX.latexPrec'(0,_1186362),['^}']))),_1214632,_1214638,_1214644).
'blocked_blocked_LaTeX.latexPrec_2'('RadExpr.Root'(_1188992,_1189010),_1182914,_1197514,_1222196,_1222202,_1222208):-makeShare(_1188992,_1220056),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_1220056,2),_1223994,_1222202,_1223948),'blocked_blocked_LaTeX.latexPrec_2_RadExpr.Root_ComplexCase'(_1223994,_1220056,_1189010,_1182914,_1197514,_1222196,_1223948,_1222208).

'blocked_blocked_LaTeX.latexPrec_2_RadExpr.Root_ComplexCase'(_1224528,_1224530,_1224532,_1224534,_1224536,_1224538,_1224540,_1224542):-freeze(_1224540,freeze(_1224528,'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Root_ComplexCase'(_1224528,_1224530,_1224532,_1224534,_1224536,_1224538,_1224540,_1224542))).
'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Root_ComplexCase'('Prelude.True',_1220056,_1189010,_1182914,_1197514,_1224952,_1224958,_1224964):-makeShare(_1189302,_1225164),makeShare(_1189010,_1225184),hnf('Prelude.cond'(letrec4PAKCS(_1225164,'Prelude.++'([^\,'^s','^q','^r','^t','^{'],'Prelude.++'('LaTeX.latexRadicand'(_1225184),['^}']))),'LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0'(_1225184,_1225164)),_1224952,_1224958,_1224964).
'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Root_ComplexCase'('Prelude.False',_1220056,_1189010,_1182914,_1197514,_1230052,_1230058,_1230064):-!,hnf('Prelude.++'([^\,'^s','^q','^r','^t','^['],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1220056),'Prelude.++'(['^]','^{'],'Prelude.++'('LaTeX.latexRadicand'(_1189010),['^}'])))),_1230052,_1230058,_1230064).
'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Root_ComplexCase'('FAIL'(_1234890),_1220056,_1189010,_1182914,_1197514,'FAIL'(_1234890),_1234904,_1234904).
'blocked_blocked_LaTeX.latexPrec_2'('RadExpr.Pow'(_1193760,_1193778),_1182914,_1197514,_1237856,_1237862,_1237868):-!,makeShare(_1193778,_1235656),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_1235656,0),_1239624,_1237862,_1239578),'blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase'(_1239624,_1193760,_1235656,_1182914,_1197514,_1237856,_1239578,_1237868).

'blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase'(_1240170,_1240172,_1240174,_1240176,_1240178,_1240180,_1240182,_1240184):-freeze(_1240182,freeze(_1240170,'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase'(_1240170,_1240172,_1240174,_1240176,_1240178,_1240180,_1240182,_1240184))).
'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase'('Prelude.True',_1193760,_1235656,_1182914,_1197514,['^1'],_1240600,_1240600).
'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase'('Prelude.False',_1193760,_1235656,_1182914,_1197514,_1244746,_1244752,_1244758):-!,makeShare(_1235656,_1242246),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_1242246),0),_1247742,_1244752,_1247696),'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'(_1247742,_1193760,_1242246,_1182914,_1197514,_1244746,_1247696,_1244758).

'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'(_1248492,_1248494,_1248496,_1248498,_1248500,_1248502,_1248504,_1248506):-freeze(_1248504,freeze(_1248492,'blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'(_1248492,_1248494,_1248496,_1248498,_1248500,_1248502,_1248504,_1248506))).
'blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_1193760,_1242246,_1182914,_1197514,_1248916,_1248922,_1248928):-hnf('LaTeX.latexPrec'(_1182914,'RadExpr.Inv'('RadExpr.Pow'(_1193760,'Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_1242246)))),_1248916,_1248922,_1248928).
'blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_1193760,_1242246,_1182914,_1197514,_1253746,_1253752,_1253758):-!,makeShare(_1242246,_1251670),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_1251670,1),_1257966,_1253752,_1257920),'blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_1257966,_1193760,_1251670,_1182914,_1197514,_1253746,_1257920,_1253758).

'blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_1258920,_1258922,_1258924,_1258926,_1258928,_1258930,_1258932,_1258934):-freeze(_1258932,freeze(_1258920,'blocked_blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_1258920,_1258922,_1258924,_1258926,_1258928,_1258930,_1258932,_1258934))).
'blocked_blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_1193760,_1251670,_1182914,_1197514,_1259344,_1259350,_1259356):-hnf('LaTeX.latexPrec'(_1182914,_1193760),_1259344,_1259350,_1259356).
'blocked_blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_1193760,_1251670,_1182914,_1197514,_1261174,_1261180,_1261186):-!,hnf('LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1182914),'LaTeX.precPow'),'Prelude.++'('LaTeX.latexBase'(_1193760),'Prelude.++'([^^,'^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1251670),['^}'])))),_1261174,_1261180,_1261186).
'blocked_blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_1266124),_1193760,_1251670,_1182914,_1197514,'FAIL'(_1266124),_1266138,_1266138).
'blocked_blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_1266222),_1193760,_1242246,_1182914,_1197514,'FAIL'(_1266222),_1266236,_1266236).
'blocked_blocked_blocked_LaTeX.latexPrec_2_RadExpr.Pow_ComplexCase'('FAIL'(_1266320),_1193760,_1235656,_1182914,_1197514,'FAIL'(_1266320),_1266334,_1266334).
'blocked_blocked_LaTeX.latexPrec_2'('FAIL'(_1266418),_1182914,_1197514,'FAIL'(_1266418),_1266432,_1266432).

'LaTeX.latexNeg'(_1267160,_1267162,_1267164,_1267166,_1267168):-freeze(_1267166,'blocked_LaTeX.latexNeg'(_1267160,_1267162,_1267164,_1267166,_1267168)).
'blocked_LaTeX.latexNeg'(_1267256,_1267274,_1268906,_1268912,_1268918):-makeShare(_1267302,_1269192),makeShare(_1267256,_1269212),makeShare(_1267274,_1269232),hnf('Prelude.cond'(letrec4PAKCS(_1269192,'LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1269212),'LaTeX.precNeg'),'Prelude.++'([^-],'LaTeX.latexPrec'('LaTeX.precNeg',_1269232)))),'LaTeX.latexNeg._\'23caseor0'(_1269232,_1269232,_1269212,_1269192)),_1268906,_1268912,_1268918).

'LaTeX.negTerm'(_1274102,_1274104,_1274106,_1274108):-freeze(_1274106,'blocked_LaTeX.negTerm'(_1274102,_1274104,_1274106,_1274108)).
'blocked_LaTeX.negTerm'(_1274188,_1274750,_1274756,_1274762):-hnf(_1274188,_1275596,_1274756,_1275608),'blocked_LaTeX.negTerm_1'(_1275596,_1274750,_1275608,_1274762).

'blocked_LaTeX.negTerm_1'(_1275928,_1275930,_1275932,_1275934):-freeze(_1275932,'blocked_blocked_LaTeX.negTerm_1'(_1275928,_1275930,_1275932,_1275934)).
'blocked_blocked_LaTeX.negTerm_1'('Prelude.(,)'(_1274304,_1274322),'Prelude.(,)'('Prelude.not'(_1274304),_1274322),_1276304,_1276304):-!.
'blocked_blocked_LaTeX.negTerm_1'('FAIL'(_1277174),'FAIL'(_1277174),_1277188,_1277188):-nonvar(_1277174).

'LaTeX.latexBase'(_1277942,_1277944,_1277946,_1277948):-freeze(_1277946,'blocked_LaTeX.latexBase'(_1277942,_1277944,_1277946,_1277948)).
'blocked_LaTeX.latexBase'(_1278028,_1278778,_1278784,_1278790):-makeShare(_1278056,_1278950),makeShare(_1278028,_1278970),hnf('Prelude.cond'(letrec4PAKCS(_1278950,'LaTeX.latexPrec'('LaTeX.precPow',_1278970)),'LaTeX.latexBase._\'23caseor0'(_1278970,_1278970,_1278950)),_1278778,_1278784,_1278790).

'LaTeX.latexRadicand'(_1281944,_1281946,_1281948,_1281950):-freeze(_1281948,'blocked_LaTeX.latexRadicand'(_1281944,_1281946,_1281948,_1281950)).
'blocked_LaTeX.latexRadicand'(_1282030,_1282696,_1282702,_1282708):-makeShare(_1282058,_1282842),makeShare(_1282030,_1282862),hnf('Prelude.cond'(letrec4PAKCS(_1282842,'LaTeX.latexPrec'(0,_1282862)),'LaTeX.latexRadicand._\'23caseor0'(_1282862,_1282842)),_1282696,_1282702,_1282708).

'LaTeX.flattenAdd'(_1285586,_1285588,_1285590,_1285592):-freeze(_1285590,'blocked_LaTeX.flattenAdd'(_1285586,_1285588,_1285590,_1285592)).
'blocked_LaTeX.flattenAdd'(_1285672,_1286590,_1286596,_1286602):-makeShare(_1285700,_1286766),makeShare(_1285672,_1286786),hnf('Prelude.cond'(letrec4PAKCS(_1286766,['Prelude.(,)'('Prelude.True',_1286786)]),'LaTeX.flattenAdd._\'23caseor0'(_1286786,_1286786,_1286766)),_1286590,_1286596,_1286602).

'LaTeX.flattenMul'(_1289824,_1289826,_1289828,_1289830):-freeze(_1289828,'blocked_LaTeX.flattenMul'(_1289824,_1289826,_1289828,_1289830)).
'blocked_LaTeX.flattenMul'(_1289910,_1290590,_1290596,_1290602):-makeShare(_1289938,_1290736),makeShare(_1289910,_1290756),hnf('Prelude.cond'(letrec4PAKCS(_1290736,[_1290756]),'LaTeX.flattenMul._\'23caseor0'(_1290756,_1290736)),_1290590,_1290596,_1290602).

'LaTeX.rebuildMul'(_1293378,_1293380,_1293382,_1293384):-freeze(_1293382,'blocked_LaTeX.rebuildMul'(_1293378,_1293380,_1293382,_1293384)).
'blocked_LaTeX.rebuildMul'(_1293464,_1294536,_1294542,_1294548):-hnf(_1293464,_1295490,_1294542,_1295502),'blocked_LaTeX.rebuildMul_1'(_1295490,_1294536,_1295502,_1294548).

'blocked_LaTeX.rebuildMul_1'(_1295846,_1295848,_1295850,_1295852):-freeze(_1295850,freeze(_1295846,'blocked_blocked_LaTeX.rebuildMul_1'(_1295846,_1295848,_1295850,_1295852))).
'blocked_blocked_LaTeX.rebuildMul_1'([],'RadExpr.Lit'('Rational.fromInt'(1)),_1296100,_1296100).
'blocked_blocked_LaTeX.rebuildMul_1'([_1293834|_1293852],_1297770,_1297776,_1297782):-!,makeShare(_1293852,_1296972),hnf(_1296972,_1299204,_1297776,_1299228),'blocked_blocked_LaTeX.rebuildMul_1_[|]_2'(_1299204,_1293834,_1299204,_1297770,_1299228,_1297782).

'blocked_blocked_LaTeX.rebuildMul_1_[|]_2'(_1299702,_1299704,_1299706,_1299708,_1299710,_1299712):-freeze(_1299710,freeze(_1299702,'blocked_blocked_blocked_LaTeX.rebuildMul_1_[|]_2'(_1299702,_1299704,_1299706,_1299708,_1299710,_1299712))).
'blocked_blocked_blocked_LaTeX.rebuildMul_1_[|]_2'([],_1293834,_1296972,_1299970,_1299976,_1299982):-hnf(_1293834,_1299970,_1299976,_1299982).
'blocked_blocked_blocked_LaTeX.rebuildMul_1_[|]_2'([_1294066|_1294084],_1293834,_1296972,_1300718,_1300724,_1300730):-!,hnf('Prelude.foldl'('RadExpr.Mul',_1293834,_1296972),_1300718,_1300724,_1300730).
'blocked_blocked_blocked_LaTeX.rebuildMul_1_[|]_2'('FAIL'(_1301892),_1293834,_1296972,'FAIL'(_1301892),_1301906,_1301906).
'blocked_blocked_LaTeX.rebuildMul_1'('FAIL'(_1301974),'FAIL'(_1301974),_1301988,_1301988).

'LaTeX.renderTerms'(_1302814,_1302816,_1302818,_1302820):-freeze(_1302818,'blocked_LaTeX.renderTerms'(_1302814,_1302816,_1302818,_1302820)).
'blocked_LaTeX.renderTerms'(_1302900,_1304556,_1304562,_1304568):-hnf(_1302900,_1305546,_1304562,_1305558),'blocked_LaTeX.renderTerms_1'(_1305546,_1304556,_1305558,_1304568).

'blocked_LaTeX.renderTerms_1'(_1305908,_1305910,_1305912,_1305914):-freeze(_1305912,freeze(_1305908,'blocked_blocked_LaTeX.renderTerms_1'(_1305908,_1305910,_1305912,_1305914))).
'blocked_blocked_LaTeX.renderTerms_1'([],['^0'],_1306162,_1306162).
'blocked_blocked_LaTeX.renderTerms_1'([_1303270|_1303288],_1307114,_1307120,_1307126):-!,hnf(_1303270,_1308584,_1307120,_1308602),'blocked_blocked_LaTeX.renderTerms_1_[|]_1'(_1308584,_1303288,_1307114,_1308602,_1307126).

'blocked_blocked_LaTeX.renderTerms_1_[|]_1'(_1309062,_1309064,_1309066,_1309068,_1309070):-freeze(_1309068,freeze(_1309062,'blocked_blocked_blocked_LaTeX.renderTerms_1_[|]_1'(_1309062,_1309064,_1309066,_1309068,_1309070))).
'blocked_blocked_blocked_LaTeX.renderTerms_1_[|]_1'('Prelude.(,)'(_1303416,_1303434),_1303288,_1309448,_1309454,_1309460):-!,makeShare(_1303474,_1309668),hnf('Prelude.cond'(letrec4PAKCS(_1309668,'LaTeX.renderTerms._\'23caseor0'(_1303416,_1303434)),'Prelude.++'(_1309668,'Prelude.apply'('Prelude.concatMap'(partcall(1,'LaTeX.renderRest',[])),_1303288))),_1309448,_1309454,_1309460).
'blocked_blocked_blocked_LaTeX.renderTerms_1_[|]_1'('FAIL'(_1312356),_1303288,'FAIL'(_1312356),_1312370,_1312370).
'blocked_blocked_LaTeX.renderTerms_1'('FAIL'(_1312430),'FAIL'(_1312430),_1312444,_1312444).

'LaTeX.renderRest'(_1313232,_1313234,_1313236,_1313238):-freeze(_1313236,'blocked_LaTeX.renderRest'(_1313232,_1313234,_1313236,_1313238)).
'blocked_LaTeX.renderRest'(_1313318,_1315446,_1315452,_1315458):-hnf(_1313318,_1316400,_1315452,_1316412),'blocked_LaTeX.renderRest_1'(_1316400,_1315446,_1316412,_1315458).

'blocked_LaTeX.renderRest_1'(_1316750,_1316752,_1316754,_1316756):-freeze(_1316754,'blocked_blocked_LaTeX.renderRest_1'(_1316750,_1316752,_1316754,_1316756)).
'blocked_blocked_LaTeX.renderRest_1'('Prelude.(,)'(_1313434,_1313452),_1317354,_1317360,_1317366):-!,hnf(_1313434,_1319028,_1317360,_1319046),'blocked_blocked_LaTeX.renderRest_1_Prelude.(,)_1'(_1319028,_1313452,_1317354,_1319046,_1317366).

'blocked_blocked_LaTeX.renderRest_1_Prelude.(,)_1'(_1319548,_1319550,_1319552,_1319554,_1319556):-freeze(_1319554,freeze(_1319548,'blocked_blocked_blocked_LaTeX.renderRest_1_Prelude.(,)_1'(_1319548,_1319550,_1319552,_1319554,_1319556))).
'blocked_blocked_blocked_LaTeX.renderRest_1_Prelude.(,)_1'('Prelude.True',_1313452,_1319942,_1319948,_1319954):-hnf('Prelude.++'(['^ ',^+,'^ '],'LaTeX.latexPrec'('LaTeX.precAdd',_1313452)),_1319942,_1319948,_1319954).
'blocked_blocked_blocked_LaTeX.renderRest_1_Prelude.(,)_1'('Prelude.False',_1313452,_1322114,_1322120,_1322126):-!,hnf('Prelude.++'(['^ ',^-,'^ '],'LaTeX.latexPrec'('LaTeX.precMul',_1313452)),_1322114,_1322120,_1322126).
'blocked_blocked_blocked_LaTeX.renderRest_1_Prelude.(,)_1'('FAIL'(_1323994),_1313452,'FAIL'(_1323994),_1324008,_1324008).
'blocked_blocked_LaTeX.renderRest_1'('FAIL'(_1324068),'FAIL'(_1324068),_1324082,_1324082):-nonvar(_1324068).

'LaTeX.renderFactors'(_1324988,_1324990,_1324992,_1324994):-freeze(_1324992,'blocked_LaTeX.renderFactors'(_1324988,_1324990,_1324992,_1324994)).
'blocked_LaTeX.renderFactors'(_1325074,_1327904,_1327910,_1327916):-makeShare(_1325074,_1326806),hnf(_1326806,_1328966,_1327910,_1328984),'blocked_LaTeX.renderFactors_1'(_1328966,_1328966,_1327904,_1328984,_1327916).

'blocked_LaTeX.renderFactors_1'(_1329366,_1329368,_1329370,_1329372,_1329374):-freeze(_1329372,freeze(_1329366,'blocked_blocked_LaTeX.renderFactors_1'(_1329366,_1329368,_1329370,_1329372,_1329374))).
'blocked_blocked_LaTeX.renderFactors_1'([],_1326806,['^1'],_1329630,_1329630).
'blocked_blocked_LaTeX.renderFactors_1'([_1325444|_1325462],_1326806,_1331530,_1331536,_1331542):-!,makeShare(_1325462,_1330438),hnf(_1330438,_1333080,_1331536,_1333110),'blocked_blocked_LaTeX.renderFactors_1_[|]_2'(_1333080,_1325444,_1333080,_1326806,_1331530,_1333110,_1331542).

'blocked_blocked_LaTeX.renderFactors_1_[|]_2'(_1333610,_1333612,_1333614,_1333616,_1333618,_1333620,_1333622):-freeze(_1333620,freeze(_1333610,'blocked_blocked_blocked_LaTeX.renderFactors_1_[|]_2'(_1333610,_1333612,_1333614,_1333616,_1333618,_1333620,_1333622))).
'blocked_blocked_blocked_LaTeX.renderFactors_1_[|]_2'([],_1325444,_1330438,_1326806,_1333888,_1333894,_1333900):-hnf('LaTeX.latexPrec'('LaTeX.precMul',_1325444),_1333888,_1333894,_1333900).
'blocked_blocked_blocked_LaTeX.renderFactors_1_[|]_2'([_1325844|_1325862],_1325444,_1330438,_1326806,_1335114,_1335120,_1335126):-!,makeShare(_1325902,_1335350),hnf('Prelude.cond'(letrec4PAKCS(_1335350,'LaTeX.joinMul'('Prelude.map'(partcall(1,'LaTeX.latexPrec',['LaTeX.precPow']),_1326806))),'LaTeX.renderFactors._\'23caseor0'(_1325444,_1330438,_1335350)),_1335114,_1335120,_1335126).
'blocked_blocked_blocked_LaTeX.renderFactors_1_[|]_2'('FAIL'(_1338120),_1325444,_1330438,_1326806,'FAIL'(_1338120),_1338134,_1338134).
'blocked_blocked_LaTeX.renderFactors_1'('FAIL'(_1338210),_1326806,'FAIL'(_1338210),_1338224,_1338224).

'LaTeX.joinMul'(_1338906,_1338908,_1338910,_1338912):-freeze(_1338910,'blocked_LaTeX.joinMul'(_1338906,_1338908,_1338910,_1338912)).
'blocked_LaTeX.joinMul'(_1338992,_1341222,_1341228,_1341234):-hnf(_1338992,_1342068,_1341228,_1342080),'blocked_LaTeX.joinMul_1'(_1342068,_1341222,_1342080,_1341234).

'blocked_LaTeX.joinMul_1'(_1342406,_1342408,_1342410,_1342412):-freeze(_1342410,freeze(_1342406,'blocked_blocked_LaTeX.joinMul_1'(_1342406,_1342408,_1342410,_1342412))).
'blocked_blocked_LaTeX.joinMul_1'([],[],_1342660,_1342660).
'blocked_blocked_LaTeX.joinMul_1'([_1339208|_1339226],_1344146,_1344152,_1344158):-!,makeShare(_1339226,_1343156),hnf(_1343156,_1345472,_1344152,_1345496),'blocked_blocked_LaTeX.joinMul_1_[|]_2'(_1345472,_1339208,_1345472,_1344146,_1345496,_1344158).

'blocked_blocked_LaTeX.joinMul_1_[|]_2'(_1345952,_1345954,_1345956,_1345958,_1345960,_1345962):-freeze(_1345960,freeze(_1345952,'blocked_blocked_blocked_LaTeX.joinMul_1_[|]_2'(_1345952,_1345954,_1345956,_1345958,_1345960,_1345962))).
'blocked_blocked_blocked_LaTeX.joinMul_1_[|]_2'([],_1339208,_1343156,_1346220,_1346226,_1346232):-hnf(_1339208,_1346220,_1346226,_1346232).
'blocked_blocked_blocked_LaTeX.joinMul_1_[|]_2'([_1339440|_1339458],_1339208,_1343156,_1346950,_1346956,_1346962):-!,hnf('Prelude.++'(_1339208,'Prelude.apply'('Prelude.concatMap'(partcall(1,'Prelude.++',[['^ ',^\,'^c','^d','^o','^t','^ ']])),_1343156)),_1346950,_1346956,_1346962).
'blocked_blocked_blocked_LaTeX.joinMul_1_[|]_2'('FAIL'(_1350136),_1339208,_1343156,'FAIL'(_1350136),_1350150,_1350150).
'blocked_blocked_LaTeX.joinMul_1'('FAIL'(_1350218),'FAIL'(_1350218),_1350232,_1350232).

'LaTeX.latexRat'(_1350944,_1350946,_1350948,_1350950):-freeze(_1350948,'blocked_LaTeX.latexRat'(_1350944,_1350946,_1350948,_1350950)).
'blocked_LaTeX.latexRat'(_1351030,_1352266,_1352272,_1352278):-makeShare(_1351058,_1352534),makeShare(_1351030,_1352554),makeShare(_1351076,_1352574),hnf('Prelude.cond'(letrec4PAKCS(_1352534,'Rational.numerator'(_1352554)),'Prelude.cond'(letrec4PAKCS(_1352574,'Rational.denominator'(_1352554)),'LaTeX.latexRat._\'23caseor0'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_1352574,1),_1352534,_1352574))),_1352266,_1352272,_1352278).

'LaTeX.absInt'(_1356632,_1356634,_1356636,_1356638):-freeze(_1356636,'blocked_LaTeX.absInt'(_1356632,_1356634,_1356636,_1356638)).
'blocked_LaTeX.absInt'(_1356718,_1358888,_1358894,_1358900):-makeShare(_1356718,_1357410),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_1357410),0),_1359794,_1358894,_1359766),'blocked_LaTeX.absInt_ComplexCase'(_1359794,_1357410,_1358888,_1359766,_1358900).

'blocked_LaTeX.absInt_ComplexCase'(_1360166,_1360168,_1360170,_1360172,_1360174):-freeze(_1360172,freeze(_1360166,'blocked_blocked_LaTeX.absInt_ComplexCase'(_1360166,_1360168,_1360170,_1360172,_1360174))).
'blocked_blocked_LaTeX.absInt_ComplexCase'('Prelude.True',_1357410,_1360560,_1360566,_1360572):-hnf('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_1357410),_1360560,_1360566,_1360572).
'blocked_blocked_LaTeX.absInt_ComplexCase'('Prelude.False',_1357410,_1361738,_1361744,_1361750):-!,hnf(_1357410,_1361738,_1361744,_1361750).
'blocked_blocked_LaTeX.absInt_ComplexCase'('FAIL'(_1362192),_1357410,'FAIL'(_1362192),_1362206,_1362206).

'LaTeX.parensIf'(_1362926,_1362928,_1362930,_1362932,_1362934):-freeze(_1362932,'blocked_LaTeX.parensIf'(_1362926,_1362928,_1362930,_1362932,_1362934)).
'blocked_LaTeX.parensIf'(_1363022,_1363040,_1365746,_1365752,_1365758):-hnf(_1363022,_1366636,_1365752,_1366654),'blocked_LaTeX.parensIf_1'(_1366636,_1363040,_1365746,_1366654,_1365758).

'blocked_LaTeX.parensIf_1'(_1366994,_1366996,_1366998,_1367000,_1367002):-freeze(_1367000,freeze(_1366994,'blocked_blocked_LaTeX.parensIf_1'(_1366994,_1366996,_1366998,_1367000,_1367002))).
'blocked_blocked_LaTeX.parensIf_1'('Prelude.True',_1363040,_1367388,_1367394,_1367400):-hnf('Prelude.++'([^\,'^l','^e','^f','^t','^('],'Prelude.++'(_1363040,[^\,'^r','^i','^g','^h','^t','^)'])),_1367388,_1367394,_1367400).
'blocked_blocked_LaTeX.parensIf_1'('Prelude.False',_1363040,_1371388,_1371394,_1371400):-!,hnf(_1363040,_1371388,_1371394,_1371400).
'blocked_blocked_LaTeX.parensIf_1'('FAIL'(_1371794),_1363040,'FAIL'(_1371794),_1371808,_1371808).

'LaTeX.latexPrec._\'23caseor0'(_1372974,_1372976,_1372978,_1372980,_1372982,_1372984,_1372986):-freeze(_1372984,'blocked_LaTeX.latexPrec._\'23caseor0'(_1372974,_1372976,_1372978,_1372980,_1372982,_1372984,_1372986)).
'blocked_LaTeX.latexPrec._\'23caseor0'(_1373090,_1373108,_1373126,_1373144,_1377126,_1377132,_1377138):-hnf(_1373090,_1378500,_1377132,_1378530),'blocked_LaTeX.latexPrec._\'23caseor0_1'(_1378500,_1373108,_1373126,_1373144,_1377126,_1378530,_1377138).

'blocked_LaTeX.latexPrec._\'23caseor0_1'(_1378964,_1378966,_1378968,_1378970,_1378972,_1378974,_1378976):-freeze(_1378974,freeze(_1378964,'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'(_1378964,_1378966,_1378968,_1378970,_1378972,_1378974,_1378976))).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'('RadExpr.Inv'(_1373260),_1373108,_1373126,_1373144,_1379376,_1379382,_1379388):-hnf('LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1373108),'LaTeX.precMul'),'Prelude.++'([^\,'^f','^r','^a','^c','^{'],'Prelude.++'('LaTeX.latexPrec'(0,_1373126),'Prelude.++'(['^}','^{'],'Prelude.++'('LaTeX.latexPrec'(0,_1373260),['^}']))))),_1379376,_1379382,_1379388).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'('RadExpr.Lit'(_1376212),_1373108,_1373126,_1373144,_1385366,_1385372,_1385378):-hnf(_1373144,_1385366,_1385372,_1385378).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'('RadExpr.Neg'(_1376322),_1373108,_1373126,_1373144,_1386342,_1386348,_1386354):-hnf(_1373144,_1386342,_1386348,_1386354).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'('RadExpr.Add'(_1376432,_1376450),_1373108,_1373126,_1373144,_1387326,_1387332,_1387338):-hnf(_1373144,_1387326,_1387332,_1387338).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'('RadExpr.Mul'(_1376566,_1376584),_1373108,_1373126,_1373144,_1388376,_1388382,_1388388):-hnf(_1373144,_1388376,_1388382,_1388388).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'('RadExpr.Root'(_1376700,_1376718),_1373108,_1373126,_1373144,_1389438,_1389444,_1389450):-hnf(_1373144,_1389438,_1389444,_1389450).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'('RadExpr.Pow'(_1376834,_1376852),_1373108,_1373126,_1373144,_1390488,_1390494,_1390500):-!,hnf(_1373144,_1390488,_1390494,_1390500).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0_1'('FAIL'(_1391252),_1373108,_1373126,_1373144,'FAIL'(_1391252),_1391266,_1391266).

'LaTeX.latexPrec._\'23caseor0._\'23caseor0'(_1392856,_1392858,_1392860,_1392862,_1392864,_1392866,_1392868):-freeze(_1392866,'blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0'(_1392856,_1392858,_1392860,_1392862,_1392864,_1392866,_1392868)).
'blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0'(_1392972,_1392990,_1393008,_1393026,_1397080,_1397086,_1397092):-hnf(_1392972,_1398886,_1397086,_1398916),'blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'(_1398886,_1392990,_1393008,_1393026,_1397080,_1398916,_1397092).

'blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'(_1399422,_1399424,_1399426,_1399428,_1399430,_1399432,_1399434):-freeze(_1399432,freeze(_1399422,'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'(_1399422,_1399424,_1399426,_1399428,_1399430,_1399432,_1399434))).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_1393142),_1392990,_1393008,_1393026,_1399834,_1399840,_1399846):-hnf('LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1392990),'LaTeX.precMul'),'Prelude.++'([^\,'^f','^r','^a','^c','^{'],'Prelude.++'('LaTeX.latexPrec'(0,_1393008),'Prelude.++'(['^}','^{'],'Prelude.++'('LaTeX.latexPrec'(0,_1393142),['^}']))))),_1399834,_1399840,_1399846).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_1396094),_1392990,_1393008,_1393026,_1405896,_1405902,_1405908):-hnf(_1393026,_1405896,_1405902,_1405908).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_1396204),_1392990,_1393008,_1393026,_1406944,_1406950,_1406956):-hnf(_1393026,_1406944,_1406950,_1406956).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_1396314,_1396332),_1392990,_1393008,_1393026,_1408000,_1408006,_1408012):-hnf(_1393026,_1408000,_1408006,_1408012).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_1396448,_1396466),_1392990,_1393008,_1393026,_1409122,_1409128,_1409134):-hnf(_1393026,_1409122,_1409128,_1409134).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_1396582,_1396600),_1392990,_1393008,_1393026,_1410256,_1410262,_1410268):-hnf(_1393026,_1410256,_1410262,_1410268).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_1396716,_1396734),_1392990,_1393008,_1393026,_1411378,_1411384,_1411390):-!,hnf(_1393026,_1411378,_1411384,_1411390).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0_1'('FAIL'(_1412214),_1392990,_1393008,_1393026,'FAIL'(_1412214),_1412228,_1412228).

'LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0'(_1414226,_1414228,_1414230,_1414232,_1414234):-freeze(_1414232,'blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0'(_1414226,_1414228,_1414230,_1414232,_1414234)).
'blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0'(_1414322,_1414340,_1419272,_1419278,_1419284):-hnf(_1414322,_1421494,_1419278,_1421512),'blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1421494,_1414340,_1419272,_1421512,_1419284).

'blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1422074,_1422076,_1422078,_1422080,_1422082):-freeze(_1422080,freeze(_1422074,'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1422074,_1422076,_1422078,_1422080,_1422082))).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_1414456),_1414340,_1424976,_1424982,_1424988):-makeShare(_1414456,_1422612),hnf('Rational.ratEq'(_1422612,'Rational.fromInt'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1))),_1428020,_1424982,_1427986),'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1428020,_1422612,_1414340,_1424976,_1427986,_1424988).

'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1428760,_1428762,_1428764,_1428766,_1428768,_1428770):-freeze(_1428768,freeze(_1428760,'blocked_blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1428760,_1428762,_1428764,_1428766,_1428768,_1428770))).
'blocked_blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_1422612,_1414340,[^\,'^m','^a','^t','^h','^r','^m','^{','^i','^}'],_1429170,_1429170).
'blocked_blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_1422612,_1414340,_1432364,_1432370,_1432376):-!,hnf('Prelude.++'([^\,'^s','^q','^r','^t','^{'],'Prelude.++'('LaTeX.latexRadicand'('RadExpr.Lit'(_1422612)),['^}'])),_1432364,_1432370,_1432376).
'blocked_blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_1435748),_1422612,_1414340,'FAIL'(_1435748),_1435762,_1435762).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_1418214),_1414340,_1436126,_1436132,_1436138):-hnf(_1414340,_1436126,_1436132,_1436138).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_1418324,_1418342),_1414340,_1437106,_1437112,_1437118):-hnf(_1414340,_1437106,_1437112,_1437118).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_1418458,_1418476),_1414340,_1438152,_1438158,_1438164):-hnf(_1414340,_1438152,_1438158,_1438164).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_1418592),_1414340,_1439190,_1439196,_1439202):-hnf(_1414340,_1439190,_1439196,_1439202).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_1418702,_1418720),_1414340,_1440182,_1440188,_1440194):-hnf(_1414340,_1440182,_1440188,_1440194).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_1418836,_1418854),_1414340,_1441228,_1441234,_1441240):-!,hnf(_1414340,_1441228,_1441234,_1441240).
'blocked_blocked_LaTeX.latexPrec._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_1441988),_1414340,'FAIL'(_1441988),_1442002,_1442002).

'LaTeX.latexNeg._\'23caseor0._\'23caseor0'(_1443538,_1443540,_1443542,_1443544,_1443546,_1443548,_1443550):-freeze(_1443548,'blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0'(_1443538,_1443540,_1443542,_1443544,_1443546,_1443548,_1443550)).
'blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0'(_1443654,_1443672,_1443690,_1443708,_1445390,_1445396,_1445402):-hnf(_1443654,_1447160,_1445396,_1447190),'blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'(_1447160,_1443672,_1443690,_1443708,_1445390,_1447190,_1445402).

'blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'(_1447690,_1447692,_1447694,_1447696,_1447698,_1447700,_1447702):-freeze(_1447700,freeze(_1447690,'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'(_1447690,_1447692,_1447694,_1447696,_1447698,_1447700,_1447702))).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_1443824),_1443672,_1443690,_1443708,_1448102,_1448108,_1448114):-hnf('LaTeX.latexPrec'(_1443672,'RadExpr.Mul'('RadExpr.Lit'('Rational.ratNeg'(_1443824)),_1443690)),_1448102,_1448108,_1448114).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_1444410),_1443672,_1443690,_1443708,_1450116,_1450122,_1450128):-hnf(_1443708,_1450116,_1450122,_1450128).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_1444520,_1444538),_1443672,_1443690,_1443708,_1451166,_1451172,_1451178):-hnf(_1443708,_1451166,_1451172,_1451178).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_1444654,_1444672),_1443672,_1443690,_1443708,_1452282,_1452288,_1452294):-hnf(_1443708,_1452282,_1452288,_1452294).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_1444788),_1443672,_1443690,_1443708,_1453390,_1453396,_1453402):-hnf(_1443708,_1453390,_1453396,_1453402).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_1444898,_1444916),_1443672,_1443690,_1443708,_1454452,_1454458,_1454464):-hnf(_1443708,_1454452,_1454458,_1454464).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_1445032,_1445050),_1443672,_1443690,_1443708,_1455568,_1455574,_1455580):-!,hnf(_1443708,_1455568,_1455574,_1455580).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0._\'23caseor0_1'('FAIL'(_1456398),_1443672,_1443690,_1443708,'FAIL'(_1456398),_1456412,_1456412).

'LaTeX.latexNeg._\'23caseor0'(_1457556,_1457558,_1457560,_1457562,_1457564,_1457566,_1457568):-freeze(_1457566,'blocked_LaTeX.latexNeg._\'23caseor0'(_1457556,_1457558,_1457560,_1457562,_1457564,_1457566,_1457568)).
'blocked_LaTeX.latexNeg._\'23caseor0'(_1457672,_1457690,_1457708,_1457726,_1461624,_1461630,_1461636):-hnf(_1457672,_1462962,_1461630,_1462992),'blocked_LaTeX.latexNeg._\'23caseor0_1'(_1462962,_1457690,_1457708,_1457726,_1461624,_1462992,_1461636).

'blocked_LaTeX.latexNeg._\'23caseor0_1'(_1463420,_1463422,_1463424,_1463426,_1463428,_1463430,_1463432):-freeze(_1463430,freeze(_1463420,'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'(_1463420,_1463422,_1463424,_1463426,_1463428,_1463430,_1463432))).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'('RadExpr.Add'(_1457842,_1457860),_1457690,_1457708,_1457726,_1463840,_1463846,_1463852):-hnf('LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1457708),'LaTeX.precAdd'),'LaTeX.renderTerms'('Prelude.map'(partcall(1,'LaTeX.negTerm',[]),'LaTeX.flattenAdd'(_1457690)))),_1463840,_1463846,_1463852).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'('RadExpr.Mul'(_1458802,_1458820),_1457690,_1457708,_1457726,_1466940,_1466946,_1466952):-makeShare(_1458860,_1467278),makeShare(_1457708,_1467298),hnf('Prelude.cond'(letrec4PAKCS(_1467278,'LaTeX.parensIf'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_1467298),'LaTeX.precNeg'),'Prelude.++'([^-],'LaTeX.latexPrec'('LaTeX.precNeg',_1457690)))),'LaTeX.latexNeg._\'23caseor0._\'23caseor0'(_1458802,_1467298,_1458820,_1467278)),_1466940,_1466946,_1466952).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'('RadExpr.Lit'(_1460552),_1457690,_1457708,_1457726,_1472044,_1472050,_1472056):-hnf('LaTeX.latexPrec'(_1457708,'RadExpr.Lit'('Rational.ratNeg'(_1460552))),_1472044,_1472050,_1472056).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'('RadExpr.Neg'(_1460984),_1457690,_1457708,_1457726,_1473694,_1473700,_1473706):-hnf(_1457726,_1473694,_1473700,_1473706).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'('RadExpr.Inv'(_1461094),_1457690,_1457708,_1457726,_1474664,_1474670,_1474676):-hnf(_1457726,_1474664,_1474670,_1474676).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'('RadExpr.Root'(_1461204,_1461222),_1457690,_1457708,_1457726,_1475654,_1475660,_1475666):-hnf(_1457726,_1475654,_1475660,_1475666).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'('RadExpr.Pow'(_1461338,_1461356),_1457690,_1457708,_1457726,_1476698,_1476704,_1476710):-!,hnf(_1457726,_1476698,_1476704,_1476710).
'blocked_blocked_LaTeX.latexNeg._\'23caseor0_1'('FAIL'(_1477456),_1457690,_1457708,_1457726,'FAIL'(_1477456),_1477470,_1477470).

'LaTeX.latexBase._\'23caseor0'(_1478652,_1478654,_1478656,_1478658,_1478660,_1478662):-freeze(_1478660,'blocked_LaTeX.latexBase._\'23caseor0'(_1478652,_1478654,_1478656,_1478658,_1478660,_1478662)).
'blocked_LaTeX.latexBase._\'23caseor0'(_1478758,_1478776,_1478794,_1492394,_1492400,_1492406):-hnf(_1478758,_1493760,_1492400,_1493784),'blocked_LaTeX.latexBase._\'23caseor0_1'(_1493760,_1478776,_1478794,_1492394,_1493784,_1492406).

'blocked_LaTeX.latexBase._\'23caseor0_1'(_1494210,_1494212,_1494214,_1494216,_1494218,_1494220):-freeze(_1494218,freeze(_1494210,'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'(_1494210,_1494212,_1494214,_1494216,_1494218,_1494220))).
'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'('RadExpr.Root'(_1478910,_1478928),_1478776,_1478794,_1494632,_1494638,_1494644):-hnf('Prelude.++'([^\,'^l','^e','^f','^t','^('],'Prelude.++'('LaTeX.latexPrec'(0,_1478776),[^\,'^r','^i','^g','^h','^t','^)'])),_1494632,_1494638,_1494644).
'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'('RadExpr.Add'(_1481536,_1481554),_1478776,_1478794,_1499190,_1499196,_1499202):-hnf('Prelude.++'([^\,'^l','^e','^f','^t','^('],'Prelude.++'('LaTeX.latexPrec'(0,_1478776),[^\,'^r','^i','^g','^h','^t','^)'])),_1499190,_1499196,_1499202).
'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'('RadExpr.Mul'(_1484162,_1484180),_1478776,_1478794,_1503748,_1503754,_1503760):-hnf('Prelude.++'([^\,'^l','^e','^f','^t','^('],'Prelude.++'('LaTeX.latexPrec'(0,_1478776),[^\,'^r','^i','^g','^h','^t','^)'])),_1503748,_1503754,_1503760).
'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'('RadExpr.Neg'(_1486788),_1478776,_1478794,_1508298,_1508304,_1508310):-hnf('Prelude.++'([^\,'^l','^e','^f','^t','^('],'Prelude.++'('LaTeX.latexPrec'(0,_1478776),[^\,'^r','^i','^g','^h','^t','^)'])),_1508298,_1508304,_1508310).
'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'('RadExpr.Inv'(_1489390),_1478776,_1478794,_1512782,_1512788,_1512794):-hnf('Prelude.++'([^\,'^l','^e','^f','^t','^('],'Prelude.++'('LaTeX.latexPrec'(0,_1478776),[^\,'^r','^i','^g','^h','^t','^)'])),_1512782,_1512788,_1512794).
'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'('RadExpr.Lit'(_1491992),_1478776,_1478794,_1517266,_1517272,_1517278):-hnf(_1478794,_1517266,_1517272,_1517278).
'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'('RadExpr.Pow'(_1492102,_1492120),_1478776,_1478794,_1518176,_1518182,_1518188):-!,hnf(_1478794,_1518176,_1518182,_1518188).
'blocked_blocked_LaTeX.latexBase._\'23caseor0_1'('FAIL'(_1518866),_1478776,_1478794,'FAIL'(_1518866),_1518880,_1518880).

'LaTeX.latexRadicand._\'23caseor0._\'23caseor0'(_1520614,_1520616,_1520618,_1520620,_1520622,_1520624):-freeze(_1520622,'blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0'(_1520614,_1520616,_1520618,_1520620,_1520622,_1520624)).
'blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0'(_1520720,_1520738,_1520756,_1526976,_1526982,_1526988):-hnf(_1520720,_1528918,_1526982,_1528942),'blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1'(_1528918,_1520738,_1520756,_1526976,_1528942,_1526988).

'blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1'(_1529464,_1529466,_1529468,_1529470,_1529472,_1529474):-freeze(_1529472,freeze(_1529464,'blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1'(_1529464,_1529466,_1529468,_1529470,_1529472,_1529474))).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1'('Prelude.True',_1520738,_1520756,_1529868,_1529874,_1529880):-hnf('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1520738),_1529868,_1529874,_1529880).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1'('Prelude.False',_1520738,_1520756,_1534252,_1534258,_1534264):-!,makeShare(_1520738,_1531678),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_1531678),0),_1537076,_1534258,_1537042),'blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_1537076,_1531678,_1520756,_1534252,_1537042,_1534264).

'blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_1537798,_1537800,_1537802,_1537804,_1537806,_1537808):-freeze(_1537806,freeze(_1537798,'blocked_blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_1537798,_1537800,_1537802,_1537804,_1537806,_1537808))).
'blocked_blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_1531678,_1520756,_1538202,_1538208,_1538214):-hnf('Prelude.++'([^-,^\,'^f','^r','^a','^c','^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23','LaTeX.absInt'(_1531678)),'Prelude.++'(['^}','^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1520756),['^}'])))),_1538202,_1538208,_1538214).
'blocked_blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_1531678,_1520756,_1544130,_1544136,_1544142):-!,hnf('Prelude.++'([^\,'^f','^r','^a','^c','^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1531678),'Prelude.++'(['^}','^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1520756),['^}'])))),_1544130,_1544136,_1544142).
'blocked_blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_1549382),_1531678,_1520756,'FAIL'(_1549382),_1549396,_1549396).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0._\'23caseor0_1'('FAIL'(_1549464),_1520738,_1520756,'FAIL'(_1549464),_1549478,_1549478).

'LaTeX.latexRadicand._\'23caseor0'(_1550804,_1550806,_1550808,_1550810,_1550812):-freeze(_1550810,'blocked_LaTeX.latexRadicand._\'23caseor0'(_1550804,_1550806,_1550808,_1550810,_1550812)).
'blocked_LaTeX.latexRadicand._\'23caseor0'(_1550900,_1550918,_1553302,_1553308,_1553314):-hnf(_1550900,_1554804,_1553308,_1554822),'blocked_LaTeX.latexRadicand._\'23caseor0_1'(_1554804,_1550918,_1553302,_1554822,_1553314).

'blocked_LaTeX.latexRadicand._\'23caseor0_1'(_1555264,_1555266,_1555268,_1555270,_1555272):-freeze(_1555270,freeze(_1555264,'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'(_1555264,_1555266,_1555268,_1555270,_1555272))).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'('RadExpr.Lit'(_1551034),_1550918,_1555656,_1555662,_1555668):-makeShare(_1551068,_1555932),makeShare(_1551034,_1555952),makeShare(_1551086,_1555972),hnf('Prelude.cond'(letrec4PAKCS(_1555932,'Rational.numerator'(_1555952)),'Prelude.cond'(letrec4PAKCS(_1555972,'Rational.denominator'(_1555952)),'LaTeX.latexRadicand._\'23caseor0._\'23caseor0'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_1555972,1),_1555932,_1555972))),_1555656,_1555662,_1555668).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'('RadExpr.Neg'(_1552364),_1550918,_1560078,_1560084,_1560090):-hnf(_1550918,_1560078,_1560084,_1560090).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'('RadExpr.Add'(_1552474,_1552492),_1550918,_1560938,_1560944,_1560950):-hnf(_1550918,_1560938,_1560944,_1560950).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'('RadExpr.Mul'(_1552608,_1552626),_1550918,_1561864,_1561870,_1561876):-hnf(_1550918,_1561864,_1561870,_1561876).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'('RadExpr.Inv'(_1552742),_1550918,_1562782,_1562788,_1562794):-hnf(_1550918,_1562782,_1562788,_1562794).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'('RadExpr.Root'(_1552852,_1552870),_1550918,_1563654,_1563660,_1563666):-hnf(_1550918,_1563654,_1563660,_1563666).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'('RadExpr.Pow'(_1552986,_1553004),_1550918,_1564580,_1564586,_1564592):-!,hnf(_1550918,_1564580,_1564586,_1564592).
'blocked_blocked_LaTeX.latexRadicand._\'23caseor0_1'('FAIL'(_1565220),_1550918,'FAIL'(_1565220),_1565234,_1565234).

'LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0'(_1567254,_1567256,_1567258,_1567260,_1567262,_1567264,_1567266):-freeze(_1567264,'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0'(_1567254,_1567256,_1567258,_1567260,_1567262,_1567264,_1567266)).
'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0'(_1567370,_1567388,_1567406,_1567424,_1570220,_1570226,_1570232):-hnf(_1567370,_1572494,_1570226,_1572524),'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1572494,_1567388,_1567406,_1567424,_1570220,_1572524,_1570232).

'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1573108,_1573110,_1573112,_1573114,_1573116,_1573118,_1573120):-freeze(_1573118,freeze(_1573108,'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1573108,_1573110,_1573112,_1573114,_1573116,_1573118,_1573120))).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_1567540),_1567388,_1567406,_1567424,_1575424,_1575430,_1575436):-makeShare(_1567540,_1573670),hnf('Rational.ratLt'(_1573670,'Rational.fromInt'(0)),_1578532,_1575430,_1578486),'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1578532,_1573670,_1567388,_1567406,_1567424,_1575424,_1578486,_1575436).

'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1579282,_1579284,_1579286,_1579288,_1579290,_1579292,_1579294,_1579296):-freeze(_1579294,freeze(_1579282,'blocked_blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1579282,_1579284,_1579286,_1579288,_1579290,_1579292,_1579294,_1579296))).
'blocked_blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_1573670,_1567388,_1567406,_1567424,['Prelude.(,)'('Prelude.False','LaTeX.rebuildMul'(['RadExpr.Lit'('Rational.ratNeg'(_1573670))|_1567388]))],_1579712,_1579712).
'blocked_blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_1573670,_1567388,_1567406,_1567424,['Prelude.(,)'('Prelude.True',_1567406)],_1582392,_1582392):-!.
'blocked_blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_1583966),_1573670,_1567388,_1567406,_1567424,'FAIL'(_1583966),_1583980,_1583980).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_1569156),_1567388,_1567406,_1567424,_1584360,_1584366,_1584372):-hnf(_1567424,_1584360,_1584366,_1584372).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_1569266,_1569284),_1567388,_1567406,_1567424,_1585494,_1585500,_1585506):-hnf(_1567424,_1585494,_1585500,_1585506).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_1569400,_1569418),_1567388,_1567406,_1567424,_1586694,_1586700,_1586706):-hnf(_1567424,_1586694,_1586700,_1586706).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_1569534),_1567388,_1567406,_1567424,_1587886,_1587892,_1587898):-hnf(_1567424,_1587886,_1587892,_1587898).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_1569644,_1569662),_1567388,_1567406,_1567424,_1589032,_1589038,_1589044):-hnf(_1567424,_1589032,_1589038,_1589044).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_1569778,_1569796),_1567388,_1567406,_1567424,_1590232,_1590238,_1590244):-!,hnf(_1567424,_1590232,_1590238,_1590244).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_1591146),_1567388,_1567406,_1567424,'FAIL'(_1591146),_1591160,_1591160).

'LaTeX.flattenAdd._\'23caseor0._\'23caseor0'(_1592788,_1592790,_1592792,_1592794,_1592796):-freeze(_1592794,'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0'(_1592788,_1592790,_1592792,_1592794,_1592796)).
'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0'(_1592884,_1592902,_1594782,_1594788,_1594794):-hnf(_1592884,_1596608,_1594788,_1596626),'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0_1'(_1596608,_1592902,_1594782,_1596626,_1594794).

'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0_1'(_1597122,_1597124,_1597126,_1597128,_1597130):-freeze(_1597128,freeze(_1597122,'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0_1'(_1597122,_1597124,_1597126,_1597128,_1597130))).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0_1'([_1593018|_1593036],_1592902,_1597408,_1597414,_1597420):-makeShare(_1593076,_1597654),makeShare(_1592902,_1597674),hnf('Prelude.cond'(letrec4PAKCS(_1597654,['Prelude.(,)'('Prelude.True',_1597674)]),'LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0'(_1593018,_1593036,_1597674,_1597654)),_1597408,_1597414,_1597420).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0_1'([],_1592902,['Prelude.(,)'('Prelude.True',_1592902)],_1600714,_1600714):-!.
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0_1'('FAIL'(_1601802),_1592902,'FAIL'(_1601802),_1601816,_1601816).

'LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_1604244,_1604246,_1604248,_1604250,_1604252,_1604254):-freeze(_1604252,'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_1604244,_1604246,_1604248,_1604250,_1604252,_1604254)).
'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_1604350,_1604368,_1604386,_1606238,_1606244,_1606250):-hnf(_1604350,_1608936,_1606244,_1608960),'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1608936,_1604368,_1604386,_1606238,_1608960,_1606250).

'blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1609608,_1609610,_1609612,_1609614,_1609616,_1609618):-freeze(_1609616,freeze(_1609608,'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'(_1609608,_1609610,_1609612,_1609614,_1609616,_1609618))).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Neg'(_1604502),_1604368,_1604386,['Prelude.(,)'('Prelude.False','RadExpr.Mul'(_1604502,_1604368))],_1610016,_1610016).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Lit'(_1605102),_1604368,_1604386,_1611958,_1611964,_1611970):-hnf(_1604386,_1611958,_1611964,_1611970).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Add'(_1605212,_1605230),_1604368,_1604386,_1613090,_1613096,_1613102):-hnf(_1604386,_1613090,_1613096,_1613102).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Mul'(_1605346,_1605364),_1604368,_1604386,_1614288,_1614294,_1614300):-hnf(_1604386,_1614288,_1614294,_1614300).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Inv'(_1605480),_1604368,_1604386,_1615478,_1615484,_1615490):-hnf(_1604386,_1615478,_1615484,_1615490).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Root'(_1605590,_1605608),_1604368,_1604386,_1616622,_1616628,_1616634):-hnf(_1604386,_1616622,_1616628,_1616634).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('RadExpr.Pow'(_1605724,_1605742),_1604368,_1604386,_1617820,_1617826,_1617832):-!,hnf(_1604386,_1617820,_1617826,_1617832).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0_1'('FAIL'(_1618732),_1604368,_1604386,'FAIL'(_1618732),_1618746,_1618746).

'LaTeX.flattenAdd._\'23caseor0'(_1619958,_1619960,_1619962,_1619964,_1619966,_1619968):-freeze(_1619966,'blocked_LaTeX.flattenAdd._\'23caseor0'(_1619958,_1619960,_1619962,_1619964,_1619966,_1619968)).
'blocked_LaTeX.flattenAdd._\'23caseor0'(_1620064,_1620082,_1620100,_1623976,_1623982,_1623988):-hnf(_1620064,_1625378,_1623982,_1625402),'blocked_LaTeX.flattenAdd._\'23caseor0_1'(_1625378,_1620082,_1620100,_1623976,_1625402,_1623988).

'blocked_LaTeX.flattenAdd._\'23caseor0_1'(_1625834,_1625836,_1625838,_1625840,_1625842,_1625844):-freeze(_1625842,freeze(_1625834,'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'(_1625834,_1625836,_1625838,_1625840,_1625842,_1625844))).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'('RadExpr.Add'(_1620216,_1620234),_1620082,_1620100,_1626244,_1626250,_1626256):-hnf('Prelude.++'('LaTeX.flattenAdd'(_1620216),'LaTeX.flattenAdd'(_1620234)),_1626244,_1626250,_1626256).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'('RadExpr.Neg'(_1620672),_1620082,_1620100,_1627968,_1627974,_1627980):-hnf('Prelude.map'(partcall(1,'LaTeX.negTerm',[]),'LaTeX.flattenAdd'(_1620672)),_1627968,_1627974,_1627980).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'('RadExpr.Lit'(_1621034),_1620082,_1620100,_1631066,_1631072,_1631078):-makeShare(_1621034,_1629546),hnf('Rational.ratLt'(_1629546,'Rational.fromInt'(0)),_1633296,_1631072,_1633256),'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1633296,_1629546,_1620082,_1620100,_1631066,_1633256,_1631078).

'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1633900,_1633902,_1633904,_1633906,_1633908,_1633910,_1633912):-freeze(_1633910,freeze(_1633900,'blocked_blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1633900,_1633902,_1633904,_1633906,_1633908,_1633910,_1633912))).
'blocked_blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_1629546,_1620082,_1620100,['Prelude.(,)'('Prelude.False','RadExpr.Lit'('Rational.ratNeg'(_1629546)))],_1634320,_1634320).
'blocked_blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_1629546,_1620082,_1620100,['Prelude.(,)'('Prelude.True','RadExpr.Lit'(_1629546))],_1636362,_1636362):-!.
'blocked_blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_1637894),_1629546,_1620082,_1620100,'FAIL'(_1637894),_1637908,_1637908).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'('RadExpr.Mul'(_1622496,_1622514),_1620082,_1620100,_1638288,_1638294,_1638300):-makeShare(_1622554,_1638538),makeShare(_1620082,_1638558),hnf('Prelude.cond'(letrec4PAKCS(_1638538,'LaTeX.flattenAdd._\'23caseor0._\'23caseor0'('LaTeX.flattenMul'(_1638558),_1638558)),'LaTeX.flattenAdd._\'23caseor0._\'23caseor0._\'23caseor0._\'23caseor0'(_1622496,_1622514,_1638538)),_1638288,_1638294,_1638300).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'('RadExpr.Inv'(_1623434),_1620082,_1620100,_1641802,_1641808,_1641814):-hnf(_1620100,_1641802,_1641808,_1641814).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'('RadExpr.Root'(_1623544,_1623562),_1620082,_1620100,_1642730,_1642736,_1642742):-hnf(_1620100,_1642730,_1642736,_1642742).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'('RadExpr.Pow'(_1623678,_1623696),_1620082,_1620100,_1643712,_1643718,_1643724):-!,hnf(_1620100,_1643712,_1643718,_1643724).
'blocked_blocked_LaTeX.flattenAdd._\'23caseor0_1'('FAIL'(_1644408),_1620082,_1620100,'FAIL'(_1644408),_1644422,_1644422).

'LaTeX.flattenMul._\'23caseor0'(_1645634,_1645636,_1645638,_1645640,_1645642):-freeze(_1645640,'blocked_LaTeX.flattenMul._\'23caseor0'(_1645634,_1645636,_1645638,_1645640,_1645642)).
'blocked_LaTeX.flattenMul._\'23caseor0'(_1645730,_1645748,_1647216,_1647222,_1647228):-hnf(_1645730,_1648610,_1647222,_1648628),'blocked_LaTeX.flattenMul._\'23caseor0_1'(_1648610,_1645748,_1647216,_1648628,_1647228).

'blocked_LaTeX.flattenMul._\'23caseor0_1'(_1649052,_1649054,_1649056,_1649058,_1649060):-freeze(_1649058,freeze(_1649052,'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'(_1649052,_1649054,_1649056,_1649058,_1649060))).
'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'('RadExpr.Mul'(_1645864,_1645882),_1645748,_1649452,_1649458,_1649464):-hnf('Prelude.++'('LaTeX.flattenMul'(_1645864),'LaTeX.flattenMul'(_1645882)),_1649452,_1649458,_1649464).
'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'('RadExpr.Lit'(_1646320),_1645748,_1651102,_1651108,_1651114):-hnf(_1645748,_1651102,_1651108,_1651114).
'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'('RadExpr.Neg'(_1646430),_1645748,_1651936,_1651942,_1651948):-hnf(_1645748,_1651936,_1651942,_1651948).
'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'('RadExpr.Add'(_1646540,_1646558),_1645748,_1652778,_1652784,_1652790):-hnf(_1645748,_1652778,_1652784,_1652790).
'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'('RadExpr.Inv'(_1646674),_1645748,_1653678,_1653684,_1653690):-hnf(_1645748,_1653678,_1653684,_1653690).
'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'('RadExpr.Root'(_1646784,_1646802),_1645748,_1654532,_1654538,_1654544):-hnf(_1645748,_1654532,_1654538,_1654544).
'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'('RadExpr.Pow'(_1646918,_1646936),_1645748,_1655440,_1655446,_1655452):-!,hnf(_1645748,_1655440,_1655446,_1655452).
'blocked_blocked_LaTeX.flattenMul._\'23caseor0_1'('FAIL'(_1656062),_1645748,'FAIL'(_1656062),_1656076,_1656076).

'LaTeX.renderTerms._\'23caseor0'(_1657318,_1657320,_1657322,_1657324,_1657326):-freeze(_1657324,'blocked_LaTeX.renderTerms._\'23caseor0'(_1657318,_1657320,_1657322,_1657324,_1657326)).
'blocked_LaTeX.renderTerms._\'23caseor0'(_1657414,_1657432,_1658548,_1658554,_1658560):-hnf(_1657414,_1659978,_1658554,_1659996),'blocked_LaTeX.renderTerms._\'23caseor0_1'(_1659978,_1657432,_1658548,_1659996,_1658560).

'blocked_LaTeX.renderTerms._\'23caseor0_1'(_1660426,_1660428,_1660430,_1660432,_1660434):-freeze(_1660432,freeze(_1660426,'blocked_blocked_LaTeX.renderTerms._\'23caseor0_1'(_1660426,_1660428,_1660430,_1660432,_1660434))).
'blocked_blocked_LaTeX.renderTerms._\'23caseor0_1'('Prelude.True',_1657432,_1660820,_1660826,_1660832):-hnf('LaTeX.latexPrec'('LaTeX.precAdd',_1657432),_1660820,_1660826,_1660832).
'blocked_blocked_LaTeX.renderTerms._\'23caseor0_1'('Prelude.False',_1657432,_1661994,_1662000,_1662006):-!,hnf('Prelude.++'([^-],'LaTeX.latexPrec'('LaTeX.precMul',_1657432)),_1661994,_1662000,_1662006).
'blocked_blocked_LaTeX.renderTerms._\'23caseor0_1'('FAIL'(_1663416),_1657432,'FAIL'(_1663416),_1663430,_1663430).

'LaTeX.renderFactors._\'23caseor0'(_1664748,_1664750,_1664752,_1664754,_1664756,_1664758):-freeze(_1664756,'blocked_LaTeX.renderFactors._\'23caseor0'(_1664748,_1664750,_1664752,_1664754,_1664756,_1664758)).
'blocked_LaTeX.renderFactors._\'23caseor0'(_1664854,_1664872,_1664890,_1669808,_1669814,_1669820):-hnf(_1664854,_1671318,_1669814,_1671342),'blocked_LaTeX.renderFactors._\'23caseor0_1'(_1671318,_1664872,_1664890,_1669808,_1671342,_1669820).

'blocked_LaTeX.renderFactors._\'23caseor0_1'(_1671792,_1671794,_1671796,_1671798,_1671800,_1671802):-freeze(_1671800,freeze(_1671792,'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'(_1671792,_1671794,_1671796,_1671798,_1671800,_1671802))).
'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'('RadExpr.Lit'(_1665006),_1664872,_1664890,_1674486,_1674492,_1674498):-makeShare(_1665006,_1672426),hnf('Rational.ratEq'(_1672426,'Rational.fromInt'(1)),_1676824,_1674492,_1676784),'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1676824,_1672426,_1664872,_1664890,_1674486,_1676784,_1674498).

'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1677446,_1677448,_1677450,_1677452,_1677454,_1677456,_1677458):-freeze(_1677456,freeze(_1677446,'blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase'(_1677446,_1677448,_1677450,_1677452,_1677454,_1677456,_1677458))).
'blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.True',_1672426,_1664872,_1664890,_1677860,_1677866,_1677872):-hnf('LaTeX.joinMul'('Prelude.map'(partcall(1,'LaTeX.latexPrec',['LaTeX.precPow']),_1664872)),_1677860,_1677866,_1677872).
'blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase'('Prelude.False',_1672426,_1664872,_1664890,_1682336,_1682342,_1682348):-!,makeShare(_1672426,_1679932),hnf('Rational.ratEq'(_1679932,'Rational.fromInt'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1))),_1685894,_1682342,_1685854),'blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_1685894,_1679932,_1664872,_1664890,_1682336,_1685854,_1682348).

'blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_1686738,_1686740,_1686742,_1686744,_1686746,_1686748,_1686750):-freeze(_1686748,freeze(_1686738,'blocked_blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'(_1686738,_1686740,_1686742,_1686744,_1686746,_1686748,_1686750))).
'blocked_blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_1679932,_1664872,_1664890,_1687152,_1687158,_1687164):-hnf('Prelude.++'([^-],'LaTeX.joinMul'('Prelude.map'(partcall(1,'LaTeX.latexPrec',['LaTeX.precPow']),_1664872))),_1687152,_1687158,_1687164).
'blocked_blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_1679932,_1664872,_1664890,_1689792,_1689798,_1689804):-!,hnf('Prelude.++'('LaTeX.latexRat'(_1679932),'Prelude.++'(['^ ',^\,'^c','^d','^o','^t','^ '],'LaTeX.joinMul'('Prelude.map'(partcall(1,'LaTeX.latexPrec',['LaTeX.precPow']),_1664872)))),_1689792,_1689798,_1689804).
'blocked_blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_1693884),_1679932,_1664872,_1664890,'FAIL'(_1693884),_1693898,_1693898).
'blocked_blocked_blocked_LaTeX.renderFactors._\'23caseor0_1_RadExpr.Lit_ComplexCase'('FAIL'(_1693974),_1672426,_1664872,_1664890,'FAIL'(_1693974),_1693988,_1693988).
'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'('RadExpr.Neg'(_1668870),_1664872,_1664890,_1694360,_1694366,_1694372):-hnf(_1664890,_1694360,_1694366,_1694372).
'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'('RadExpr.Add'(_1668980,_1668998),_1664872,_1664890,_1695294,_1695300,_1695306):-hnf(_1664890,_1695294,_1695300,_1695306).
'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'('RadExpr.Mul'(_1669114,_1669132),_1664872,_1664890,_1696294,_1696300,_1696306):-hnf(_1664890,_1696294,_1696300,_1696306).
'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'('RadExpr.Inv'(_1669248),_1664872,_1664890,_1697286,_1697292,_1697298):-hnf(_1664890,_1697286,_1697292,_1697298).
'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'('RadExpr.Root'(_1669358,_1669376),_1664872,_1664890,_1698232,_1698238,_1698244):-hnf(_1664890,_1698232,_1698238,_1698244).
'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'('RadExpr.Pow'(_1669492,_1669510),_1664872,_1664890,_1699232,_1699238,_1699244):-!,hnf(_1664890,_1699232,_1699238,_1699244).
'blocked_blocked_LaTeX.renderFactors._\'23caseor0_1'('FAIL'(_1699946),_1664872,_1664890,'FAIL'(_1699946),_1699960,_1699960).

'LaTeX.latexRat._\'23caseor0'(_1701096,_1701098,_1701100,_1701102,_1701104,_1701106):-freeze(_1701104,'blocked_LaTeX.latexRat._\'23caseor0'(_1701096,_1701098,_1701100,_1701102,_1701104,_1701106)).
'blocked_LaTeX.latexRat._\'23caseor0'(_1701202,_1701220,_1701238,_1707356,_1707362,_1707368):-hnf(_1701202,_1708686,_1707362,_1708710),'blocked_LaTeX.latexRat._\'23caseor0_1'(_1708686,_1701220,_1701238,_1707356,_1708710,_1707368).

'blocked_LaTeX.latexRat._\'23caseor0_1'(_1709130,_1709132,_1709134,_1709136,_1709138,_1709140):-freeze(_1709138,freeze(_1709130,'blocked_blocked_LaTeX.latexRat._\'23caseor0_1'(_1709130,_1709132,_1709134,_1709136,_1709138,_1709140))).
'blocked_blocked_LaTeX.latexRat._\'23caseor0_1'('Prelude.True',_1701220,_1701238,_1709534,_1709540,_1709546):-hnf('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1701220),_1709534,_1709540,_1709546).
'blocked_blocked_LaTeX.latexRat._\'23caseor0_1'('Prelude.False',_1701220,_1701238,_1713714,_1713720,_1713726):-!,makeShare(_1701220,_1711242),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_1711242),0),_1715926,_1713720,_1715892),'blocked_blocked_LaTeX.latexRat._\'23caseor0_1_Prelude.False_ComplexCase'(_1715926,_1711242,_1701238,_1713714,_1715892,_1713726).

'blocked_blocked_LaTeX.latexRat._\'23caseor0_1_Prelude.False_ComplexCase'(_1716546,_1716548,_1716550,_1716552,_1716554,_1716556):-freeze(_1716554,freeze(_1716546,'blocked_blocked_blocked_LaTeX.latexRat._\'23caseor0_1_Prelude.False_ComplexCase'(_1716546,_1716548,_1716550,_1716552,_1716554,_1716556))).
'blocked_blocked_blocked_LaTeX.latexRat._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_1711242,_1701238,_1716950,_1716956,_1716962):-hnf('Prelude.++'([^-,^\,'^f','^r','^a','^c','^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23','LaTeX.absInt'(_1711242)),'Prelude.++'(['^}','^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1701238),['^}'])))),_1716950,_1716956,_1716962).
'blocked_blocked_blocked_LaTeX.latexRat._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_1711242,_1701238,_1722776,_1722782,_1722788):-!,hnf('Prelude.++'([^\,'^f','^r','^a','^c','^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1711242),'Prelude.++'(['^}','^{'],'Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_1701238),['^}'])))),_1722776,_1722782,_1722788).
'blocked_blocked_blocked_LaTeX.latexRat._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_1727926),_1711242,_1701238,'FAIL'(_1727926),_1727940,_1727940).
'blocked_blocked_LaTeX.latexRat._\'23caseor0_1'('FAIL'(_1728008),_1701220,_1701238,'FAIL'(_1728008),_1728022,_1728022).

:-costCenters(['']).




%%%%% Number of shared variables: 44

