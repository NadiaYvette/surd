%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Positive').
:-importModule('Prelude').

:-curryModule('PrimeFactors').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('PrimeFactors.factorise',factorise,1,'PrimeFactors.factorise',nofix,notype).
functiontype('PrimeFactors.trialDivide','PrimeFactors.trialDivide',2,'PrimeFactors.trialDivide',nofix,notype).
functiontype('PrimeFactors.trialDivide._\'23selFP2\'23q','PrimeFactors.trialDivide._#selFP2#q',1,'PrimeFactors.trialDivide._\'23selFP2\'23q',nofix,notype).
functiontype('PrimeFactors.trialDivide._\'23selFP3\'23r','PrimeFactors.trialDivide._#selFP3#r',1,'PrimeFactors.trialDivide._\'23selFP3\'23r',nofix,notype).
functiontype('PrimeFactors.groupFactors','PrimeFactors.groupFactors',1,'PrimeFactors.groupFactors',nofix,notype).
functiontype('PrimeFactors.groupFactors._\'23selFP5\'23same','PrimeFactors.groupFactors._#selFP5#same',1,'PrimeFactors.groupFactors._\'23selFP5\'23same',nofix,notype).
functiontype('PrimeFactors.groupFactors._\'23selFP6\'23rest','PrimeFactors.groupFactors._#selFP6#rest',1,'PrimeFactors.groupFactors._\'23selFP6\'23rest',nofix,notype).
functiontype('PrimeFactors.primeFactors',primeFactors,0,'PrimeFactors.primeFactors',nofix,notype).
functiontype('PrimeFactors.isPrime',isPrime,1,'PrimeFactors.isPrime',nofix,notype).
functiontype('PrimeFactors.isPrime._\'23lambda2','PrimeFactors.isPrime._#lambda2',2,'PrimeFactors.isPrime._\'23lambda2',nofix,notype).
functiontype('PrimeFactors.isPrime._\'23lambda3','PrimeFactors.isPrime._#lambda3',2,'PrimeFactors.isPrime._\'23lambda3',nofix,notype).
functiontype('PrimeFactors.primes',primes,0,'PrimeFactors.primes',nofix,notype).
functiontype('PrimeFactors.factorise._\'23caseor0','PrimeFactors.factorise._#caseor0',2,'PrimeFactors.factorise._\'23caseor0',nofix,notype).
functiontype('PrimeFactors.trialDivide._\'23caseor0','PrimeFactors.trialDivide._#caseor0',6,'PrimeFactors.trialDivide._\'23caseor0',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'PrimeFactors.factorise'(_5851866,_5851868,_5851870,_5851872):-freeze(_5851870,'blocked_PrimeFactors.factorise'(_5851866,_5851868,_5851870,_5851872)).
'blocked_PrimeFactors.factorise'(_5851952,_5852702,_5852708,_5852714):-makeShare(_5851980,_5852838),hnf('Prelude.cond'(letrec4PAKCS(_5852838,'Positive.unPositive'(_5851952)),'PrimeFactors.factorise._\'23caseor0'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5852838,1),_5852838)),_5852702,_5852708,_5852714).

'PrimeFactors.trialDivide'(_5856132,_5856134,_5856136,_5856138,_5856140):-freeze(_5856138,'blocked_PrimeFactors.trialDivide'(_5856132,_5856134,_5856136,_5856138,_5856140)).
'blocked_PrimeFactors.trialDivide'(_5856228,_5856246,_5865716,_5865722,_5865728):-makeShare(_5856228,_5863112),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5863112,1),_5867068,_5865722,_5867034),'blocked_PrimeFactors.trialDivide_ComplexCase'(_5867068,_5863112,_5856246,_5865716,_5867034,_5865728).

'blocked_PrimeFactors.trialDivide_ComplexCase'(_5867514,_5867516,_5867518,_5867520,_5867522,_5867524):-freeze(_5867522,freeze(_5867514,'blocked_blocked_PrimeFactors.trialDivide_ComplexCase'(_5867514,_5867516,_5867518,_5867520,_5867522,_5867524))).
'blocked_blocked_PrimeFactors.trialDivide_ComplexCase'('Prelude.True',_5863112,_5856246,[],_5867924,_5867924).
'blocked_blocked_PrimeFactors.trialDivide_ComplexCase'('Prelude.False',_5863112,_5856246,_5871648,_5871654,_5871660):-!,hnf('Prelude.otherwise',_5874148,_5871654,_5874114),'blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase'(_5874148,_5863112,_5856246,_5871648,_5874114,_5871660).

'blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase'(_5874804,_5874806,_5874808,_5874810,_5874812,_5874814):-freeze(_5874812,freeze(_5874804,'blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase'(_5874804,_5874806,_5874808,_5874810,_5874812,_5874814))).
'blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5863112,_5856246,_5875762,_5875768,_5875774):-hnf(_5856246,_5879354,_5875768,_5879372),'blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase_Prelude.True_2'(_5879354,_5863112,_5875762,_5879372,_5875774).

'blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase_Prelude.True_2'(_5880174,_5880176,_5880178,_5880180,_5880182):-freeze(_5880180,freeze(_5880174,'blocked_blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase_Prelude.True_2'(_5880174,_5880176,_5880178,_5880180,_5880182))).
'blocked_blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase_Prelude.True_2'([],_5863112,_5880432,_5880438,_5880444):-hnf('Prelude.error'(['^p','^r','^i','^m','^e','^s','^ ','^e','^x','^h','^a','^u','^s','^t','^e','^d']),_5880432,_5880438,_5880444).
'blocked_blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase_Prelude.True_2'([_5859474|_5859492],_5863112,_5884904,_5884910,_5884916):-!,makeShare(_5859532,_5885566),makeShare(_5863112,_5885586),makeShare(_5859474,_5885606),makeShare(_5859550,_5885626),makeShare(_5859568,_5885646),hnf('Prelude.cond'(letrec4PAKCS(_5885566,'Prelude._impl\'23divMod\'23Prelude.Integral\'23Prelude.Int\'23'(_5885586,_5885606)),'Prelude.cond'(letrec4PAKCS(_5885626,'PrimeFactors.trialDivide._\'23selFP2\'23q'(_5885566)),'Prelude.cond'(letrec4PAKCS(_5885646,'PrimeFactors.trialDivide._\'23selFP3\'23r'(_5885566)),'PrimeFactors.trialDivide._\'23caseor0'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23','Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_5885606,_5885606)),_5885586),_5885646,_5885626,_5885606,_5885586,_5859492)))),_5884904,_5884910,_5884916).
'blocked_blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase_Prelude.True_2'('FAIL'(_5892740),_5863112,'FAIL'(_5892740),_5892754,_5892754).
'blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5863112,_5856246,_5893124,_5893130,_5893136):-!,hnf(reportFailure4PAKCS('PrimeFactors.trialDivide',['Prelude.False']),_5893124,_5893130,_5893136).
'blocked_blocked_blocked_PrimeFactors.trialDivide_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5894710),_5863112,_5856246,'FAIL'(_5894710),_5894724,_5894724).
'blocked_blocked_PrimeFactors.trialDivide_ComplexCase'('FAIL'(_5894792),_5863112,_5856246,'FAIL'(_5894792),_5894806,_5894806).

'PrimeFactors.trialDivide._\'23selFP2\'23q'(_5896402,_5896404,_5896406,_5896408):-freeze(_5896406,'blocked_PrimeFactors.trialDivide._\'23selFP2\'23q'(_5896402,_5896404,_5896406,_5896408)).
'blocked_PrimeFactors.trialDivide._\'23selFP2\'23q'(_5896488,_5896968,_5896974,_5896980):-hnf(_5896488,_5898750,_5896974,_5898762),'blocked_PrimeFactors.trialDivide._\'23selFP2\'23q_1'(_5898750,_5896968,_5898762,_5896980).

'blocked_PrimeFactors.trialDivide._\'23selFP2\'23q_1'(_5899238,_5899240,_5899242,_5899244):-freeze(_5899242,'blocked_blocked_PrimeFactors.trialDivide._\'23selFP2\'23q_1'(_5899238,_5899240,_5899242,_5899244)).
'blocked_blocked_PrimeFactors.trialDivide._\'23selFP2\'23q_1'('Prelude.(,)'(_5896604,_5896622),_5899608,_5899614,_5899620):-!,hnf(_5896604,_5899608,_5899614,_5899620).
'blocked_blocked_PrimeFactors.trialDivide._\'23selFP2\'23q_1'('FAIL'(_5900222),'FAIL'(_5900222),_5900236,_5900236):-nonvar(_5900222).

'PrimeFactors.trialDivide._\'23selFP3\'23r'(_5901820,_5901822,_5901824,_5901826):-freeze(_5901824,'blocked_PrimeFactors.trialDivide._\'23selFP3\'23r'(_5901820,_5901822,_5901824,_5901826)).
'blocked_PrimeFactors.trialDivide._\'23selFP3\'23r'(_5901906,_5902386,_5902392,_5902398):-hnf(_5901906,_5904168,_5902392,_5904180),'blocked_PrimeFactors.trialDivide._\'23selFP3\'23r_1'(_5904168,_5902386,_5904180,_5902398).

'blocked_PrimeFactors.trialDivide._\'23selFP3\'23r_1'(_5904656,_5904658,_5904660,_5904662):-freeze(_5904660,'blocked_blocked_PrimeFactors.trialDivide._\'23selFP3\'23r_1'(_5904656,_5904658,_5904660,_5904662)).
'blocked_blocked_PrimeFactors.trialDivide._\'23selFP3\'23r_1'('Prelude.(,)'(_5902022,_5902040),_5905026,_5905032,_5905038):-!,hnf(_5902040,_5905026,_5905032,_5905038).
'blocked_blocked_PrimeFactors.trialDivide._\'23selFP3\'23r_1'('FAIL'(_5905640),'FAIL'(_5905640),_5905654,_5905654):-nonvar(_5905640).

'PrimeFactors.groupFactors'(_5906788,_5906790,_5906792,_5906794):-freeze(_5906792,'blocked_PrimeFactors.groupFactors'(_5906788,_5906790,_5906792,_5906794)).
'blocked_PrimeFactors.groupFactors'(_5906874,_5909496,_5909502,_5909508):-hnf(_5906874,_5910774,_5909502,_5910786),'blocked_PrimeFactors.groupFactors_1'(_5910774,_5909496,_5910786,_5909508).

'blocked_PrimeFactors.groupFactors_1'(_5911178,_5911180,_5911182,_5911184):-freeze(_5911182,'blocked_blocked_PrimeFactors.groupFactors_1'(_5911178,_5911180,_5911182,_5911184)).
'blocked_blocked_PrimeFactors.groupFactors_1'([],[],_5911426,_5911426).
'blocked_blocked_PrimeFactors.groupFactors_1'([_5907090|_5907108],_5911984,_5911990,_5911996):-!,makeShare(_5907148,_5912430),makeShare(_5907090,_5912450),makeShare(_5907166,_5912470),makeShare(_5907184,_5912490),hnf('Prelude.cond'(letrec4PAKCS(_5912430,'Prelude.span'(partcall(1,'Prelude.flip',[_5912450,partcall(2,'Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23',[])]),_5907108)),'Prelude.cond'(letrec4PAKCS(_5912470,'PrimeFactors.groupFactors._\'23selFP5\'23same'(_5912430)),'Prelude.cond'(letrec4PAKCS(_5912490,'PrimeFactors.groupFactors._\'23selFP6\'23rest'(_5912430)),['Prelude.(,)'(_5912450,'Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(1,'Prelude.length'(_5912470)))|'PrimeFactors.groupFactors'(_5912490)]))),_5911984,_5911990,_5911996).
'blocked_blocked_PrimeFactors.groupFactors_1'('FAIL'(_5918582),'FAIL'(_5918582),_5918596,_5918596):-nonvar(_5918582).

'PrimeFactors.groupFactors._\'23selFP5\'23same'(_5920318,_5920320,_5920322,_5920324):-freeze(_5920322,'blocked_PrimeFactors.groupFactors._\'23selFP5\'23same'(_5920318,_5920320,_5920322,_5920324)).
'blocked_PrimeFactors.groupFactors._\'23selFP5\'23same'(_5920404,_5920908,_5920914,_5920920):-hnf(_5920404,_5922834,_5920914,_5922846),'blocked_PrimeFactors.groupFactors._\'23selFP5\'23same_1'(_5922834,_5920908,_5922846,_5920920).

'blocked_PrimeFactors.groupFactors._\'23selFP5\'23same_1'(_5923346,_5923348,_5923350,_5923352):-freeze(_5923350,'blocked_blocked_PrimeFactors.groupFactors._\'23selFP5\'23same_1'(_5923346,_5923348,_5923350,_5923352)).
'blocked_blocked_PrimeFactors.groupFactors._\'23selFP5\'23same_1'('Prelude.(,)'(_5920520,_5920538),_5923716,_5923722,_5923728):-!,hnf(_5920520,_5923716,_5923722,_5923728).
'blocked_blocked_PrimeFactors.groupFactors._\'23selFP5\'23same_1'('FAIL'(_5924354),'FAIL'(_5924354),_5924368,_5924368):-nonvar(_5924354).

'PrimeFactors.groupFactors._\'23selFP6\'23rest'(_5926090,_5926092,_5926094,_5926096):-freeze(_5926094,'blocked_PrimeFactors.groupFactors._\'23selFP6\'23rest'(_5926090,_5926092,_5926094,_5926096)).
'blocked_PrimeFactors.groupFactors._\'23selFP6\'23rest'(_5926176,_5926680,_5926686,_5926692):-hnf(_5926176,_5928606,_5926686,_5928618),'blocked_PrimeFactors.groupFactors._\'23selFP6\'23rest_1'(_5928606,_5926680,_5928618,_5926692).

'blocked_PrimeFactors.groupFactors._\'23selFP6\'23rest_1'(_5929118,_5929120,_5929122,_5929124):-freeze(_5929122,'blocked_blocked_PrimeFactors.groupFactors._\'23selFP6\'23rest_1'(_5929118,_5929120,_5929122,_5929124)).
'blocked_blocked_PrimeFactors.groupFactors._\'23selFP6\'23rest_1'('Prelude.(,)'(_5926292,_5926310),_5929488,_5929494,_5929500):-!,hnf(_5926310,_5929488,_5929494,_5929500).
'blocked_blocked_PrimeFactors.groupFactors._\'23selFP6\'23rest_1'('FAIL'(_5930126),'FAIL'(_5930126),_5930140,_5930140):-nonvar(_5930126).

'PrimeFactors.primeFactors'(_5931274,_5931276,_5931278):-freeze(_5931276,'blocked_PrimeFactors.primeFactors'(_5931274,_5931276,_5931278)).
'blocked_PrimeFactors.primeFactors'(_5931614,_5931620,_5931626):-hnf('Prelude..'(partcall(1,'Prelude.map',[partcall(1,'Prelude.fst',[])]),partcall(1,'PrimeFactors.factorise',[])),_5931614,_5931620,_5931626).

'PrimeFactors.isPrime'(_5933530,_5933532,_5933534,_5933536):-freeze(_5933534,'blocked_PrimeFactors.isPrime'(_5933530,_5933532,_5933534,_5933536)).
'blocked_PrimeFactors.isPrime'(_5933616,_5938014,_5938020,_5938026):-makeShare(_5933616,_5935944),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_5935944),2),_5939208,_5938020,_5939180),'blocked_PrimeFactors.isPrime_ComplexCase'(_5939208,_5935944,_5938014,_5939180,_5938026).

'blocked_PrimeFactors.isPrime_ComplexCase'(_5939628,_5939630,_5939632,_5939634,_5939636):-freeze(_5939634,freeze(_5939628,'blocked_blocked_PrimeFactors.isPrime_ComplexCase'(_5939628,_5939630,_5939632,_5939634,_5939636))).
'blocked_blocked_PrimeFactors.isPrime_ComplexCase'('Prelude.True',_5935944,'Prelude.False',_5940028,_5940028).
'blocked_blocked_PrimeFactors.isPrime_ComplexCase'('Prelude.False',_5935944,_5942962,_5942968,_5942974):-!,makeShare(_5935944,_5940998),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_5940998),4),_5945304,_5942968,_5945276),'blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase'(_5945304,_5940998,_5942962,_5945276,_5942974).

'blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase'(_5945946,_5945948,_5945950,_5945952,_5945954):-freeze(_5945952,freeze(_5945946,'blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase'(_5945946,_5945948,_5945950,_5945952,_5945954))).
'blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5940998,'Prelude.True',_5946346,_5946346).
'blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5940998,_5948516,_5948522,_5948528):-!,hnf('Prelude.otherwise',_5952082,_5948522,_5952054),'blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5952082,_5940998,_5948516,_5952054,_5948528).

'blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5952916,_5952918,_5952920,_5952922,_5952924):-freeze(_5952922,freeze(_5952916,'blocked_blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_5952916,_5952918,_5952920,_5952922,_5952924))).
'blocked_blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_5940998,_5953310,_5953316,_5953322):-makeShare(_5940998,_5953414),hnf('Prelude.apply'('Prelude.all'(partcall(1,'PrimeFactors.isPrime._\'23lambda2',[_5953414])),'Prelude.takeWhile'(partcall(1,'PrimeFactors.isPrime._\'23lambda3',[_5953414]),'PrimeFactors.primes')),_5953310,_5953316,_5953322).
'blocked_blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_5940998,_5956256,_5956262,_5956268):-!,hnf(reportFailure4PAKCS('PrimeFactors.isPrime',['Prelude.False']),_5956256,_5956262,_5956268).
'blocked_blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5957924),_5940998,'FAIL'(_5957924),_5957938,_5957938).
'blocked_blocked_blocked_PrimeFactors.isPrime_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_5957998),_5940998,'FAIL'(_5957998),_5958012,_5958012).
'blocked_blocked_PrimeFactors.isPrime_ComplexCase'('FAIL'(_5958072),_5935944,'FAIL'(_5958072),_5958086,_5958086).

'PrimeFactors.isPrime._\'23lambda2'(_5959442,_5959444,_5959446,_5959448,_5959450):-freeze(_5959448,'blocked_PrimeFactors.isPrime._\'23lambda2'(_5959442,_5959444,_5959446,_5959448,_5959450)).
'blocked_PrimeFactors.isPrime._\'23lambda2'(_5959538,_5959556,_5960216,_5960222,_5960228):-hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'2F\'3D\'23Prelude.Eq\'23Prelude.Int\'23','Prelude.apply'('Prelude.apply'('Prelude._impl\'23mod\'23Prelude.Integral\'23Prelude.Int\'23',_5959538),_5959556)),0),_5960216,_5960222,_5960228).

'PrimeFactors.isPrime._\'23lambda3'(_5963816,_5963818,_5963820,_5963822,_5963824):-freeze(_5963822,'blocked_PrimeFactors.isPrime._\'23lambda3'(_5963816,_5963818,_5963820,_5963822,_5963824)).
'blocked_PrimeFactors.isPrime._\'23lambda3'(_5963912,_5963930,_5964254,_5964260,_5964266):-makeShare(_5963930,_5964376),hnf('Prelude._impl\'23\'3C\'3D\'23Prelude.Ord\'23Prelude.Int\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_5964376,_5964376),_5963912),_5964254,_5964260,_5964266).

'PrimeFactors.primes'(_5966886,_5966888,_5966890):-freeze(_5966888,'blocked_PrimeFactors.primes'(_5966886,_5966888,_5966890)).
'blocked_PrimeFactors.primes'([2|'Prelude.filter'(partcall(1,'PrimeFactors.isPrime',[]),'Prelude._impl\'23enumFromThen\'23Prelude.Enum\'23Prelude.Int\'23'(3,5))],_5967442,_5967442).

'PrimeFactors.factorise._\'23caseor0'(_5970224,_5970226,_5970228,_5970230,_5970232):-freeze(_5970230,'blocked_PrimeFactors.factorise._\'23caseor0'(_5970224,_5970226,_5970228,_5970230,_5970232)).
'blocked_PrimeFactors.factorise._\'23caseor0'(_5970320,_5970338,_5971092,_5971098,_5971104):-hnf(_5970320,_5972702,_5971098,_5972720),'blocked_PrimeFactors.factorise._\'23caseor0_1'(_5972702,_5970338,_5971092,_5972720,_5971104).

'blocked_PrimeFactors.factorise._\'23caseor0_1'(_5973180,_5973182,_5973184,_5973186,_5973188):-freeze(_5973186,freeze(_5973180,'blocked_blocked_PrimeFactors.factorise._\'23caseor0_1'(_5973180,_5973182,_5973184,_5973186,_5973188))).
'blocked_blocked_PrimeFactors.factorise._\'23caseor0_1'('Prelude.True',_5970338,[],_5973580,_5973580).
'blocked_blocked_PrimeFactors.factorise._\'23caseor0_1'('Prelude.False',_5970338,_5974386,_5974392,_5974398):-!,hnf('PrimeFactors.groupFactors'('PrimeFactors.trialDivide'(_5970338,'PrimeFactors.primes')),_5974386,_5974392,_5974398).
'blocked_blocked_PrimeFactors.factorise._\'23caseor0_1'('FAIL'(_5975648),_5970338,'FAIL'(_5975648),_5975662,_5975662).

'PrimeFactors.trialDivide._\'23caseor0'(_5977170,_5977172,_5977174,_5977176,_5977178,_5977180,_5977182,_5977184,_5977186):-freeze(_5977184,'blocked_PrimeFactors.trialDivide._\'23caseor0'(_5977170,_5977172,_5977174,_5977176,_5977178,_5977180,_5977182,_5977184,_5977186)).
'blocked_PrimeFactors.trialDivide._\'23caseor0'(_5977306,_5977324,_5977342,_5977360,_5977378,_5977396,_5979024,_5979030,_5979036):-hnf(_5977306,_5980738,_5979030,_5980780),'blocked_PrimeFactors.trialDivide._\'23caseor0_1'(_5980738,_5977324,_5977342,_5977360,_5977378,_5977396,_5979024,_5980780,_5979036).

'blocked_PrimeFactors.trialDivide._\'23caseor0_1'(_5981284,_5981286,_5981288,_5981290,_5981292,_5981294,_5981296,_5981298,_5981300):-freeze(_5981298,freeze(_5981284,'blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1'(_5981284,_5981286,_5981288,_5981290,_5981292,_5981294,_5981296,_5981298,_5981300))).
'blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1'('Prelude.True',_5977324,_5977342,_5977360,_5977378,_5977396,[_5977378],_5981724,_5981724).
'blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1'('Prelude.False',_5977324,_5977342,_5977360,_5977378,_5977396,_5984790,_5984796,_5984802):-!,hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_5977324,0),_5987404,_5984796,_5987352),'blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1_Prelude.False_ComplexCase'(_5987404,_5977324,_5977342,_5977360,_5977378,_5977396,_5984790,_5987352,_5984802).

'blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1_Prelude.False_ComplexCase'(_5988078,_5988080,_5988082,_5988084,_5988086,_5988088,_5988090,_5988092,_5988094):-freeze(_5988092,freeze(_5988078,'blocked_blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1_Prelude.False_ComplexCase'(_5988078,_5988080,_5988082,_5988084,_5988086,_5988088,_5988090,_5988092,_5988094))).
'blocked_blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_5977324,_5977342,_5977360,_5977378,_5977396,[_5988706|'PrimeFactors.trialDivide'(_5977342,[_5988706|_5977396])],_5988518,_5988524):-makeShare(_5977360,_5988706),_5988518=_5988524.
'blocked_blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_5977324,_5977342,_5977360,_5977378,_5977396,_5990862,_5990868,_5990874):-!,hnf('PrimeFactors.trialDivide'(_5977378,_5977396),_5990862,_5990868,_5990874).
'blocked_blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_5992258),_5977324,_5977342,_5977360,_5977378,_5977396,'FAIL'(_5992258),_5992272,_5992272).
'blocked_blocked_PrimeFactors.trialDivide._\'23caseor0_1'('FAIL'(_5992364),_5977324,_5977342,_5977360,_5977378,_5977396,'FAIL'(_5992364),_5992378,_5992378).

:-costCenters(['']).




%%%%% Number of shared variables: 16

