%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').

:-curryModule('Rational').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23','_inst#Prelude.Eq#Rational.Rational#',1,'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23','_impl#==#Prelude.Eq#Rational.Rational#',0,'Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23\'2F\'3D\'23Prelude.Eq\'23Rational.Rational\'23','_impl#/=#Prelude.Eq#Rational.Rational#',0,'Rational._impl\'23\'2F\'3D\'23Prelude.Eq\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23','_inst#Prelude.Ord#Rational.Rational#',1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23compare\'23Prelude.Ord\'23Rational.Rational\'23','_impl#compare#Prelude.Ord#Rational.Rational#',0,'Rational._impl\'23compare\'23Prelude.Ord\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23\'3C\'23Prelude.Ord\'23Rational.Rational\'23','_impl#<#Prelude.Ord#Rational.Rational#',0,'Rational._impl\'23\'3C\'23Prelude.Ord\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23\'3E\'23Prelude.Ord\'23Rational.Rational\'23','_impl#>#Prelude.Ord#Rational.Rational#',0,'Rational._impl\'23\'3E\'23Prelude.Ord\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23\'3C\'3D\'23Prelude.Ord\'23Rational.Rational\'23','_impl#<=#Prelude.Ord#Rational.Rational#',0,'Rational._impl\'23\'3C\'3D\'23Prelude.Ord\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23\'3E\'3D\'23Prelude.Ord\'23Rational.Rational\'23','_impl#>=#Prelude.Ord#Rational.Rational#',0,'Rational._impl\'23\'3E\'3D\'23Prelude.Ord\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23min\'23Prelude.Ord\'23Rational.Rational\'23','_impl#min#Prelude.Ord#Rational.Rational#',0,'Rational._impl\'23min\'23Prelude.Ord\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23max\'23Prelude.Ord\'23Rational.Rational\'23','_impl#max#Prelude.Ord#Rational.Rational#',0,'Rational._impl\'23max\'23Prelude.Ord\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._inst\'23Prelude.Show\'23Rational.Rational\'23','_inst#Prelude.Show#Rational.Rational#',1,'Rational._inst\'23Prelude.Show\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23','_impl#show#Prelude.Show#Rational.Rational#',0,'Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23showsPrec\'23Prelude.Show\'23Rational.Rational\'23','_impl#showsPrec#Prelude.Show#Rational.Rational#',0,'Rational._impl\'23showsPrec\'23Prelude.Show\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23showList\'23Prelude.Show\'23Rational.Rational\'23','_impl#showList#Prelude.Show#Rational.Rational#',0,'Rational._impl\'23showList\'23Prelude.Show\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._inst\'23Prelude.Data\'23Rational.Rational\'23','_inst#Prelude.Data#Rational.Rational#',1,'Rational._inst\'23Prelude.Data\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23','_impl#===#Prelude.Data#Rational.Rational#',2,'Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational._impl\'23aValue\'23Prelude.Data\'23Rational.Rational\'23','_impl#aValue#Prelude.Data#Rational.Rational#',0,'Rational._impl\'23aValue\'23Prelude.Data\'23Rational.Rational\'23',nofix,notype).
functiontype('Rational.mkRat',mkRat,2,'Rational.mkRat',nofix,notype).
functiontype('Rational.rat',rat,0,'Rational.rat',nofix,notype).
functiontype('Rational.numerator',numerator,1,'Rational.numerator',nofix,notype).
functiontype('Rational.denominator',denominator,1,'Rational.denominator',nofix,notype).
functiontype('Rational.fromInt',fromInt,1,'Rational.fromInt',nofix,notype).
functiontype('Rational.gcdInt','Rational.gcdInt',2,'Rational.gcdInt',nofix,notype).
functiontype('Rational.absInt','Rational.absInt',1,'Rational.absInt',nofix,notype).
functiontype('Rational.ratAdd',ratAdd,2,'Rational.ratAdd',nofix,notype).
functiontype('Rational.ratSub',ratSub,2,'Rational.ratSub',nofix,notype).
functiontype('Rational.ratMul',ratMul,2,'Rational.ratMul',nofix,notype).
functiontype('Rational.ratDiv',ratDiv,2,'Rational.ratDiv',nofix,notype).
functiontype('Rational.ratNeg',ratNeg,1,'Rational.ratNeg',nofix,notype).
functiontype('Rational.ratAbs',ratAbs,1,'Rational.ratAbs',nofix,notype).
functiontype('Rational.ratSignum',ratSignum,1,'Rational.ratSignum',nofix,notype).
functiontype('Rational.ratInv',ratInv,1,'Rational.ratInv',nofix,notype).
functiontype('Rational.ratPow',ratPow,2,'Rational.ratPow',nofix,notype).
functiontype('Rational.ratEq',ratEq,2,'Rational.ratEq',nofix,notype).
functiontype('Rational.ratCompare',ratCompare,2,'Rational.ratCompare',nofix,notype).
functiontype('Rational.ratLe',ratLe,2,'Rational.ratLe',nofix,notype).
functiontype('Rational.ratLt',ratLt,2,'Rational.ratLt',nofix,notype).
functiontype('Rational.ratGe',ratGe,2,'Rational.ratGe',nofix,notype).
functiontype('Rational.ratGt',ratGt,2,'Rational.ratGt',nofix,notype).
functiontype('Rational.ratMin',ratMin,2,'Rational.ratMin',nofix,notype).
functiontype('Rational.ratMax',ratMax,2,'Rational.ratMax',nofix,notype).
functiontype('Rational.ratFloor',ratFloor,1,'Rational.ratFloor',nofix,notype).
functiontype('Rational.ratCeiling',ratCeiling,1,'Rational.ratCeiling',nofix,notype).
functiontype('Rational.showRat',showRat,1,'Rational.showRat',nofix,notype).
functiontype('Rational.mkRat._\'23caseor0','Rational.mkRat._#caseor0',3,'Rational.mkRat._\'23caseor0',nofix,notype).
functiontype('Rational.ratFloor._\'23caseor0','Rational.ratFloor._#caseor0',2,'Rational.ratFloor._\'23caseor0',nofix,notype).
functiontype('Rational.ratCeiling._\'23caseor0','Rational.ratCeiling._#caseor0',2,'Rational.ratCeiling._\'23caseor0',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.
constructortype('Rational.Rat','Rat',2,'Rat',0,notype,[]).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23'(_6738064,_6738066,_6738068,_6738070):-freeze(_6738068,'blocked_Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23'(_6738064,_6738066,_6738068,_6738070)).
'blocked_Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23'(_6738150,_6738830,_6738836,_6738842):-hnf(_6738150,_6741008,_6738836,_6741020),'blocked_Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23_1'(_6741008,_6738830,_6741020,_6738842).

'blocked_Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23_1'(_6741562,_6741564,_6741566,_6741568):-freeze(_6741566,'blocked_blocked_Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23_1'(_6741562,_6741564,_6741566,_6741568)).
'blocked_blocked_Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23_1'('Prelude.()','Prelude._Dict\'23Eq'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23','Rational._impl\'23\'2F\'3D\'23Prelude.Eq\'23Rational.Rational\'23'),_6741914,_6741914):-!.
'blocked_blocked_Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23_1'('FAIL'(_6743494),'FAIL'(_6743494),_6743508,_6743508):-nonvar(_6743494).

'Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23'(_6745678,_6745680,_6745682):-freeze(_6745680,'blocked_Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23'(_6745678,_6745680,_6745682)).
'blocked_Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23'(_6745766,_6745772,_6745778):-hnf(partcall(2,'Rational.ratEq',[]),_6745766,_6745772,_6745778).

'Rational._impl\'23\'2F\'3D\'23Prelude.Eq\'23Rational.Rational\'23'(_6748500,_6748502,_6748504):-freeze(_6748502,'blocked_Rational._impl\'23\'2F\'3D\'23Prelude.Eq\'23Rational.Rational\'23'(_6748500,_6748502,_6748504)).
'blocked_Rational._impl\'23\'2F\'3D\'23Prelude.Eq\'23Rational.Rational\'23'(_6748672,_6748678,_6748684):-hnf(partcall(2,'Prelude._def\'23\'2F\'3D\'23Prelude.Eq',[partcall(1,'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23',[])]),_6748672,_6748678,_6748684).

'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23'(_6751790,_6751792,_6751794,_6751796):-freeze(_6751794,'blocked_Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23'(_6751790,_6751792,_6751794,_6751796)).
'blocked_Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23'(_6751876,_6753066,_6753072,_6753078):-hnf(_6751876,_6755280,_6753072,_6755292),'blocked_Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23_1'(_6755280,_6753066,_6755292,_6753078).

'blocked_Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23_1'(_6755840,_6755842,_6755844,_6755846):-freeze(_6755844,'blocked_blocked_Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23_1'(_6755840,_6755842,_6755844,_6755846)).
'blocked_blocked_Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23_1'('Prelude.()','Prelude._Dict\'23Ord'(partcall(1,'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23',[]),'Rational._impl\'23compare\'23Prelude.Ord\'23Rational.Rational\'23','Rational._impl\'23\'3C\'23Prelude.Ord\'23Rational.Rational\'23','Rational._impl\'23\'3E\'23Prelude.Ord\'23Rational.Rational\'23','Rational._impl\'23\'3C\'3D\'23Prelude.Ord\'23Rational.Rational\'23','Rational._impl\'23\'3E\'3D\'23Prelude.Ord\'23Rational.Rational\'23','Rational._impl\'23min\'23Prelude.Ord\'23Rational.Rational\'23','Rational._impl\'23max\'23Prelude.Ord\'23Rational.Rational\'23'),_6756192,_6756192):-!.
'blocked_blocked_Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23_1'('FAIL'(_6760506),'FAIL'(_6760506),_6760520,_6760520):-nonvar(_6760506).

'Rational._impl\'23compare\'23Prelude.Ord\'23Rational.Rational\'23'(_6762862,_6762864,_6762866):-freeze(_6762864,'blocked_Rational._impl\'23compare\'23Prelude.Ord\'23Rational.Rational\'23'(_6762862,_6762864,_6762866)).
'blocked_Rational._impl\'23compare\'23Prelude.Ord\'23Rational.Rational\'23'(_6762950,_6762956,_6762962):-hnf(partcall(2,'Rational.ratCompare',[]),_6762950,_6762956,_6762962).

'Rational._impl\'23\'3C\'23Prelude.Ord\'23Rational.Rational\'23'(_6765698,_6765700,_6765702):-freeze(_6765700,'blocked_Rational._impl\'23\'3C\'23Prelude.Ord\'23Rational.Rational\'23'(_6765698,_6765700,_6765702)).
'blocked_Rational._impl\'23\'3C\'23Prelude.Ord\'23Rational.Rational\'23'(_6765870,_6765876,_6765882):-hnf(partcall(2,'Prelude._def\'23\'3C\'23Prelude.Ord',[partcall(1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',[])]),_6765870,_6765876,_6765882).

'Rational._impl\'23\'3E\'23Prelude.Ord\'23Rational.Rational\'23'(_6769102,_6769104,_6769106):-freeze(_6769104,'blocked_Rational._impl\'23\'3E\'23Prelude.Ord\'23Rational.Rational\'23'(_6769102,_6769104,_6769106)).
'blocked_Rational._impl\'23\'3E\'23Prelude.Ord\'23Rational.Rational\'23'(_6769274,_6769280,_6769286):-hnf(partcall(2,'Prelude._def\'23\'3E\'23Prelude.Ord',[partcall(1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',[])]),_6769274,_6769280,_6769286).

'Rational._impl\'23\'3C\'3D\'23Prelude.Ord\'23Rational.Rational\'23'(_6772572,_6772574,_6772576):-freeze(_6772574,'blocked_Rational._impl\'23\'3C\'3D\'23Prelude.Ord\'23Rational.Rational\'23'(_6772572,_6772574,_6772576)).
'blocked_Rational._impl\'23\'3C\'3D\'23Prelude.Ord\'23Rational.Rational\'23'(_6772660,_6772666,_6772672):-hnf(partcall(2,'Rational.ratLe',[]),_6772660,_6772666,_6772672).

'Rational._impl\'23\'3E\'3D\'23Prelude.Ord\'23Rational.Rational\'23'(_6775438,_6775440,_6775442):-freeze(_6775440,'blocked_Rational._impl\'23\'3E\'3D\'23Prelude.Ord\'23Rational.Rational\'23'(_6775438,_6775440,_6775442)).
'blocked_Rational._impl\'23\'3E\'3D\'23Prelude.Ord\'23Rational.Rational\'23'(_6775610,_6775616,_6775622):-hnf(partcall(2,'Prelude._def\'23\'3E\'3D\'23Prelude.Ord',[partcall(1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',[])]),_6775610,_6775616,_6775622).

'Rational._impl\'23min\'23Prelude.Ord\'23Rational.Rational\'23'(_6778926,_6778928,_6778930):-freeze(_6778928,'blocked_Rational._impl\'23min\'23Prelude.Ord\'23Rational.Rational\'23'(_6778926,_6778928,_6778930)).
'blocked_Rational._impl\'23min\'23Prelude.Ord\'23Rational.Rational\'23'(_6779098,_6779104,_6779110):-hnf(partcall(2,'Prelude._def\'23min\'23Prelude.Ord',[partcall(1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',[])]),_6779098,_6779104,_6779110).

'Rational._impl\'23max\'23Prelude.Ord\'23Rational.Rational\'23'(_6782378,_6782380,_6782382):-freeze(_6782380,'blocked_Rational._impl\'23max\'23Prelude.Ord\'23Rational.Rational\'23'(_6782378,_6782380,_6782382)).
'blocked_Rational._impl\'23max\'23Prelude.Ord\'23Rational.Rational\'23'(_6782550,_6782556,_6782562):-hnf(partcall(2,'Prelude._def\'23max\'23Prelude.Ord',[partcall(1,'Rational._inst\'23Prelude.Ord\'23Rational.Rational\'23',[])]),_6782550,_6782556,_6782562).

'Rational._inst\'23Prelude.Show\'23Rational.Rational\'23'(_6785688,_6785690,_6785692,_6785694):-freeze(_6785692,'blocked_Rational._inst\'23Prelude.Show\'23Rational.Rational\'23'(_6785688,_6785690,_6785692,_6785694)).
'blocked_Rational._inst\'23Prelude.Show\'23Rational.Rational\'23'(_6785774,_6786550,_6786556,_6786562):-hnf(_6785774,_6788800,_6786556,_6788812),'blocked_Rational._inst\'23Prelude.Show\'23Rational.Rational\'23_1'(_6788800,_6786550,_6788812,_6786562).

'blocked_Rational._inst\'23Prelude.Show\'23Rational.Rational\'23_1'(_6789366,_6789368,_6789370,_6789372):-freeze(_6789370,'blocked_blocked_Rational._inst\'23Prelude.Show\'23Rational.Rational\'23_1'(_6789366,_6789368,_6789370,_6789372)).
'blocked_blocked_Rational._inst\'23Prelude.Show\'23Rational.Rational\'23_1'('Prelude.()','Prelude._Dict\'23Show'('Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23','Rational._impl\'23showsPrec\'23Prelude.Show\'23Rational.Rational\'23','Rational._impl\'23showList\'23Prelude.Show\'23Rational.Rational\'23'),_6789718,_6789718):-!.
'blocked_blocked_Rational._inst\'23Prelude.Show\'23Rational.Rational\'23_1'('FAIL'(_6791842),'FAIL'(_6791842),_6791856,_6791856):-nonvar(_6791842).

'Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23'(_6794122,_6794124,_6794126):-freeze(_6794124,'blocked_Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23'(_6794122,_6794124,_6794126)).
'blocked_Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23'(_6794210,_6794216,_6794222):-hnf(partcall(1,'Rational.showRat',[]),_6794210,_6794216,_6794222).

'Rational._impl\'23showsPrec\'23Prelude.Show\'23Rational.Rational\'23'(_6797242,_6797244,_6797246):-freeze(_6797244,'blocked_Rational._impl\'23showsPrec\'23Prelude.Show\'23Rational.Rational\'23'(_6797242,_6797244,_6797246)).
'blocked_Rational._impl\'23showsPrec\'23Prelude.Show\'23Rational.Rational\'23'(_6797414,_6797420,_6797426):-hnf(partcall(3,'Prelude._def\'23showsPrec\'23Prelude.Show',[partcall(1,'Rational._inst\'23Prelude.Show\'23Rational.Rational\'23',[])]),_6797414,_6797420,_6797426).

'Rational._impl\'23showList\'23Prelude.Show\'23Rational.Rational\'23'(_6801012,_6801014,_6801016):-freeze(_6801014,'blocked_Rational._impl\'23showList\'23Prelude.Show\'23Rational.Rational\'23'(_6801012,_6801014,_6801016)).
'blocked_Rational._impl\'23showList\'23Prelude.Show\'23Rational.Rational\'23'(_6801184,_6801190,_6801196):-hnf('Prelude._def\'23showList\'23Prelude.Show'(partcall(1,'Rational._inst\'23Prelude.Show\'23Rational.Rational\'23',[])),_6801184,_6801190,_6801196).

'Rational._inst\'23Prelude.Data\'23Rational.Rational\'23'(_6804394,_6804396,_6804398,_6804400):-freeze(_6804398,'blocked_Rational._inst\'23Prelude.Data\'23Rational.Rational\'23'(_6804394,_6804396,_6804398,_6804400)).
'blocked_Rational._inst\'23Prelude.Data\'23Rational.Rational\'23'(_6804480,_6805172,_6805178,_6805184):-hnf(_6804480,_6807422,_6805178,_6807434),'blocked_Rational._inst\'23Prelude.Data\'23Rational.Rational\'23_1'(_6807422,_6805172,_6807434,_6805184).

'blocked_Rational._inst\'23Prelude.Data\'23Rational.Rational\'23_1'(_6807988,_6807990,_6807992,_6807994):-freeze(_6807992,'blocked_blocked_Rational._inst\'23Prelude.Data\'23Rational.Rational\'23_1'(_6807988,_6807990,_6807992,_6807994)).
'blocked_blocked_Rational._inst\'23Prelude.Data\'23Rational.Rational\'23_1'('Prelude.()','Prelude._Dict\'23Data'(partcall(2,'Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23',[]),'Rational._impl\'23aValue\'23Prelude.Data\'23Rational.Rational\'23'),_6808340,_6808340):-!.
'blocked_blocked_Rational._inst\'23Prelude.Data\'23Rational.Rational\'23_1'('FAIL'(_6809990),'FAIL'(_6809990),_6810004,_6810004):-nonvar(_6809990).

'Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23'(_6812316,_6812318,_6812320,_6812322,_6812324):-freeze(_6812322,'blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23'(_6812316,_6812318,_6812320,_6812322,_6812324)).
'blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23'(_6812412,_6812430,_6814010,_6814016,_6814022):-hnf(_6812412,_6816700,_6814016,_6816718),'blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1'(_6816700,_6812430,_6814010,_6816718,_6814022).

'blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1'(_6817352,_6817354,_6817356,_6817358,_6817360):-freeze(_6817358,'blocked_blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1'(_6817352,_6817354,_6817356,_6817358,_6817360)).
'blocked_blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1'('Rational.Rat'(_6812546,_6812564),_6812430,_6818296,_6818302,_6818308):-!,hnf(_6812430,_6821736,_6818302,_6821760),'blocked_blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1_Rational.Rat_3'(_6821736,_6812546,_6812564,_6818296,_6821760,_6818308).

'blocked_blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1_Rational.Rat_3'(_6822558,_6822560,_6822562,_6822564,_6822566,_6822568):-freeze(_6822566,'blocked_blocked_blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1_Rational.Rat_3'(_6822558,_6822560,_6822562,_6822564,_6822566,_6822568)).
'blocked_blocked_blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1_Rational.Rat_3'('Rational.Rat'(_6812692,_6812710),_6812546,_6812564,_6822974,_6822980,_6822986):-!,hnf('Prelude.&&'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Prelude.Int\'23',_6812546),_6812692),'Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Prelude.Int\'23',_6812564),_6812710)),_6822974,_6822980,_6822986).
'blocked_blocked_blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1_Rational.Rat_3'('FAIL'(_6826266),_6812546,_6812564,'FAIL'(_6826266),_6826280,_6826280):-nonvar(_6826266).
'blocked_blocked_Rational._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Rational.Rational\'23_1'('FAIL'(_6826352),_6812430,'FAIL'(_6826352),_6826366,_6826366):-nonvar(_6826352).

'Rational._impl\'23aValue\'23Prelude.Data\'23Rational.Rational\'23'(_6828716,_6828718,_6828720):-freeze(_6828718,'blocked_Rational._impl\'23aValue\'23Prelude.Data\'23Rational.Rational\'23'(_6828716,_6828718,_6828720)).
'blocked_Rational._impl\'23aValue\'23Prelude.Data\'23Rational.Rational\'23'('Rational.Rat'('Prelude._impl\'23aValue\'23Prelude.Data\'23Prelude.Int\'23','Prelude._impl\'23aValue\'23Prelude.Data\'23Prelude.Int\'23'),_6828978,_6828978).

'Rational.mkRat'(_6831104,_6831106,_6831108,_6831110,_6831112):-freeze(_6831110,'blocked_Rational.mkRat'(_6831104,_6831106,_6831108,_6831110,_6831112)).
'blocked_Rational.mkRat'(_6831200,_6831218,_6842308,_6842314,_6842320):-makeShare(_6831218,_6839502),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6839502,0),_6843300,_6842314,_6843266),'blocked_Rational.mkRat_ComplexCase'(_6843300,_6831200,_6839502,_6842308,_6843266,_6842320).

'blocked_Rational.mkRat_ComplexCase'(_6843686,_6843688,_6843690,_6843692,_6843694,_6843696):-freeze(_6843694,freeze(_6843686,'blocked_blocked_Rational.mkRat_ComplexCase'(_6843686,_6843688,_6843690,_6843692,_6843694,_6843696))).
'blocked_blocked_Rational.mkRat_ComplexCase'('Prelude.True',_6831200,_6839502,_6844090,_6844096,_6844102):-hnf('Prelude.error'(['^m','^k','^R','^a','^t',^:,'^ ','^z','^e','^r','^o','^ ','^d','^e','^n','^o','^m','^i','^n','^a','^t','^o','^r']),_6844090,_6844096,_6844102).
'blocked_blocked_Rational.mkRat_ComplexCase'('Prelude.False',_6831200,_6839502,_6852504,_6852510,_6852516):-!,makeShare(_6831200,_6850342),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6850342,0),_6854644,_6852510,_6854610),'blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase'(_6854644,_6850342,_6839502,_6852504,_6854610,_6852516).

'blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase'(_6855252,_6855254,_6855256,_6855258,_6855260,_6855262):-freeze(_6855260,freeze(_6855252,'blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase'(_6855252,_6855254,_6855256,_6855258,_6855260,_6855262))).
'blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6850342,_6839502,'Rational.Rat'(0,1),_6855662,_6855662).
'blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6850342,_6839502,_6859024,_6859030,_6859036):-!,hnf('Prelude.otherwise',_6862388,_6859030,_6862354),'blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6862388,_6850342,_6839502,_6859024,_6862354,_6859036).

'blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6863188,_6863190,_6863192,_6863194,_6863196,_6863198):-freeze(_6863196,freeze(_6863188,'blocked_blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6863188,_6863190,_6863192,_6863194,_6863196,_6863198))).
'blocked_blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6850342,_6839502,_6863592,_6863598,_6863604):-makeShare(_6835828,_6864160),makeShare(_6850342,_6864180),makeShare(_6839502,_6864200),makeShare(_6835846,_6864220),makeShare(_6835864,_6864240),hnf('Prelude.cond'(letrec4PAKCS(_6864160,'Rational.gcdInt'('Rational.absInt'(_6864180),'Rational.absInt'(_6864200))),'Prelude.cond'(letrec4PAKCS(_6864220,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23div\'23Prelude.Integral\'23Prelude.Int\'23',_6864180),_6864160)),'Prelude.cond'(letrec4PAKCS(_6864240,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23div\'23Prelude.Integral\'23Prelude.Int\'23',_6864200),_6864160)),'Rational.mkRat._\'23caseor0'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_6864240),0),_6864220,_6864240)))),_6863592,_6863598,_6863604).
'blocked_blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6850342,_6839502,_6872006,_6872012,_6872018):-!,hnf(reportFailure4PAKCS('Rational.mkRat',['Prelude.False']),_6872006,_6872012,_6872018).
'blocked_blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6873676),_6850342,_6839502,'FAIL'(_6873676),_6873690,_6873690).
'blocked_blocked_blocked_Rational.mkRat_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6873758),_6850342,_6839502,'FAIL'(_6873758),_6873772,_6873772).
'blocked_blocked_Rational.mkRat_ComplexCase'('FAIL'(_6873840),_6831200,_6839502,'FAIL'(_6873840),_6873854,_6873854).

'Rational.rat'(_6874506,_6874508,_6874510):-freeze(_6874508,'blocked_Rational.rat'(_6874506,_6874508,_6874510)).
'blocked_Rational.rat'(_6874594,_6874600,_6874606):-hnf(partcall(2,'Rational.mkRat',[]),_6874594,_6874600,_6874606).

'Rational.numerator'(_6875744,_6875746,_6875748,_6875750):-freeze(_6875748,'blocked_Rational.numerator'(_6875744,_6875746,_6875748,_6875750)).
'blocked_Rational.numerator'(_6875830,_6876184,_6876190,_6876196):-hnf(_6875830,_6877210,_6876190,_6877222),'blocked_Rational.numerator_1'(_6877210,_6876184,_6877222,_6876196).

'blocked_Rational.numerator_1'(_6877572,_6877574,_6877576,_6877578):-freeze(_6877576,'blocked_blocked_Rational.numerator_1'(_6877572,_6877574,_6877576,_6877578)).
'blocked_blocked_Rational.numerator_1'('Rational.Rat'(_6875946,_6875964),_6877968,_6877974,_6877980):-!,hnf(_6875946,_6877968,_6877974,_6877980).
'blocked_blocked_Rational.numerator_1'('FAIL'(_6878456),'FAIL'(_6878456),_6878470,_6878470):-nonvar(_6878456).

'Rational.denominator'(_6879414,_6879416,_6879418,_6879420):-freeze(_6879418,'blocked_Rational.denominator'(_6879414,_6879416,_6879418,_6879420)).
'blocked_Rational.denominator'(_6879500,_6879866,_6879872,_6879878):-hnf(_6879500,_6880964,_6879872,_6880976),'blocked_Rational.denominator_1'(_6880964,_6879866,_6880976,_6879878).

'blocked_Rational.denominator_1'(_6881338,_6881340,_6881342,_6881344):-freeze(_6881342,'blocked_blocked_Rational.denominator_1'(_6881338,_6881340,_6881342,_6881344)).
'blocked_blocked_Rational.denominator_1'('Rational.Rat'(_6879616,_6879634),_6881734,_6881740,_6881746):-!,hnf(_6879634,_6881734,_6881740,_6881746).
'blocked_blocked_Rational.denominator_1'('FAIL'(_6882234),'FAIL'(_6882234),_6882248,_6882248):-nonvar(_6882234).

'Rational.fromInt'(_6883040,_6883042,_6883044,_6883046):-freeze(_6883044,'blocked_Rational.fromInt'(_6883040,_6883042,_6883044,_6883046)).
'blocked_Rational.fromInt'(_6883126,'Rational.Rat'(_6883126,1),_6883302,_6883302).

'Rational.gcdInt'(_6884570,_6884572,_6884574,_6884576,_6884578):-freeze(_6884576,'blocked_Rational.gcdInt'(_6884570,_6884572,_6884574,_6884576,_6884578)).
'blocked_Rational.gcdInt'(_6884666,_6884684,_6887722,_6887728,_6887734):-makeShare(_6884684,_6886222),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6886222,0),_6888750,_6887728,_6888716),'blocked_Rational.gcdInt_ComplexCase'(_6888750,_6884666,_6886222,_6887722,_6888716,_6887734).

'blocked_Rational.gcdInt_ComplexCase'(_6889142,_6889144,_6889146,_6889148,_6889150,_6889152):-freeze(_6889150,freeze(_6889142,'blocked_blocked_Rational.gcdInt_ComplexCase'(_6889142,_6889144,_6889146,_6889148,_6889150,_6889152))).
'blocked_blocked_Rational.gcdInt_ComplexCase'('Prelude.True',_6884666,_6886222,_6889546,_6889552,_6889558):-hnf(_6884666,_6889546,_6889552,_6889558).
'blocked_blocked_Rational.gcdInt_ComplexCase'('Prelude.False',_6884666,_6886222,_6891328,_6891334,_6891340):-!,hnf('Prelude.otherwise',_6893504,_6891334,_6893470),'blocked_blocked_Rational.gcdInt_ComplexCase_Prelude.False_ComplexCase'(_6893504,_6884666,_6886222,_6891328,_6893470,_6891340).

'blocked_blocked_Rational.gcdInt_ComplexCase_Prelude.False_ComplexCase'(_6894106,_6894108,_6894110,_6894112,_6894114,_6894116):-freeze(_6894114,freeze(_6894106,'blocked_blocked_blocked_Rational.gcdInt_ComplexCase_Prelude.False_ComplexCase'(_6894106,_6894108,_6894110,_6894112,_6894114,_6894116))).
'blocked_blocked_blocked_Rational.gcdInt_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6884666,_6886222,_6894510,_6894516,_6894522):-makeShare(_6886222,_6894644),hnf('Rational.gcdInt'(_6894644,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23mod\'23Prelude.Integral\'23Prelude.Int\'23',_6884666),_6894644)),_6894510,_6894516,_6894522).
'blocked_blocked_blocked_Rational.gcdInt_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6884666,_6886222,_6897004,_6897010,_6897016):-!,hnf(reportFailure4PAKCS('Rational.gcdInt',['Prelude.False']),_6897004,_6897010,_6897016).
'blocked_blocked_blocked_Rational.gcdInt_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6898482),_6884666,_6886222,'FAIL'(_6898482),_6898496,_6898496).
'blocked_blocked_Rational.gcdInt_ComplexCase'('FAIL'(_6898564),_6884666,_6886222,'FAIL'(_6898564),_6898578,_6898578).

'Rational.absInt'(_6899344,_6899346,_6899348,_6899350):-freeze(_6899348,'blocked_Rational.absInt'(_6899344,_6899346,_6899348,_6899350)).
'blocked_Rational.absInt'(_6899430,_6901618,_6901624,_6901630):-makeShare(_6899430,_6900122),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_6900122),0),_6902632,_6901624,_6902604),'blocked_Rational.absInt_ComplexCase'(_6902632,_6900122,_6901618,_6902604,_6901630).

'blocked_Rational.absInt_ComplexCase'(_6903022,_6903024,_6903026,_6903028,_6903030):-freeze(_6903028,freeze(_6903022,'blocked_blocked_Rational.absInt_ComplexCase'(_6903022,_6903024,_6903026,_6903028,_6903030))).
'blocked_blocked_Rational.absInt_ComplexCase'('Prelude.True',_6900122,_6903416,_6903422,_6903428):-hnf('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_6900122),_6903416,_6903422,_6903428).
'blocked_blocked_Rational.absInt_ComplexCase'('Prelude.False',_6900122,_6904612,_6904618,_6904624):-!,hnf(_6900122,_6904612,_6904618,_6904624).
'blocked_blocked_Rational.absInt_ComplexCase'('FAIL'(_6905084),_6900122,'FAIL'(_6905084),_6905098,_6905098).

'Rational.ratAdd'(_6905856,_6905858,_6905860,_6905862,_6905864):-freeze(_6905862,'blocked_Rational.ratAdd'(_6905856,_6905858,_6905860,_6905862,_6905864)).
'blocked_Rational.ratAdd'(_6905952,_6905970,_6907228,_6907234,_6907240):-hnf(_6905952,_6908154,_6907234,_6908172),'blocked_Rational.ratAdd_1'(_6908154,_6905970,_6907228,_6908172,_6907240).

'blocked_Rational.ratAdd_1'(_6908512,_6908514,_6908516,_6908518,_6908520):-freeze(_6908518,'blocked_blocked_Rational.ratAdd_1'(_6908512,_6908514,_6908516,_6908518,_6908520)).
'blocked_blocked_Rational.ratAdd_1'('Rational.Rat'(_6906086,_6906104),_6905970,_6909162,_6909168,_6909174):-!,hnf(_6905970,_6910838,_6909168,_6910862),'blocked_blocked_Rational.ratAdd_1_Rational.Rat_3'(_6910838,_6906086,_6906104,_6909162,_6910862,_6909174).

'blocked_blocked_Rational.ratAdd_1_Rational.Rat_3'(_6911366,_6911368,_6911370,_6911372,_6911374,_6911376):-freeze(_6911374,'blocked_blocked_blocked_Rational.ratAdd_1_Rational.Rat_3'(_6911366,_6911368,_6911370,_6911372,_6911374,_6911376)).
'blocked_blocked_blocked_Rational.ratAdd_1_Rational.Rat_3'('Rational.Rat'(_6906232,_6906250),_6906086,_6906104,_6911782,_6911788,_6911794):-!,makeShare(_6906250,_6912068),makeShare(_6906104,_6912088),hnf('Rational.mkRat'('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6906086,_6912068),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6906232,_6912088)),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6912088,_6912068)),_6911782,_6911788,_6911794).
'blocked_blocked_blocked_Rational.ratAdd_1_Rational.Rat_3'('FAIL'(_6915444),_6906086,_6906104,'FAIL'(_6915444),_6915458,_6915458):-nonvar(_6915444).
'blocked_blocked_Rational.ratAdd_1'('FAIL'(_6915530),_6905970,'FAIL'(_6915530),_6915544,_6915544):-nonvar(_6915530).

'Rational.ratSub'(_6916306,_6916308,_6916310,_6916312,_6916314):-freeze(_6916312,'blocked_Rational.ratSub'(_6916306,_6916308,_6916310,_6916312,_6916314)).
'blocked_Rational.ratSub'(_6916402,_6916420,_6917678,_6917684,_6917690):-hnf(_6916402,_6918604,_6917684,_6918622),'blocked_Rational.ratSub_1'(_6918604,_6916420,_6917678,_6918622,_6917690).

'blocked_Rational.ratSub_1'(_6918962,_6918964,_6918966,_6918968,_6918970):-freeze(_6918968,'blocked_blocked_Rational.ratSub_1'(_6918962,_6918964,_6918966,_6918968,_6918970)).
'blocked_blocked_Rational.ratSub_1'('Rational.Rat'(_6916536,_6916554),_6916420,_6919612,_6919618,_6919624):-!,hnf(_6916420,_6921288,_6919618,_6921312),'blocked_blocked_Rational.ratSub_1_Rational.Rat_3'(_6921288,_6916536,_6916554,_6919612,_6921312,_6919624).

'blocked_blocked_Rational.ratSub_1_Rational.Rat_3'(_6921816,_6921818,_6921820,_6921822,_6921824,_6921826):-freeze(_6921824,'blocked_blocked_blocked_Rational.ratSub_1_Rational.Rat_3'(_6921816,_6921818,_6921820,_6921822,_6921824,_6921826)).
'blocked_blocked_blocked_Rational.ratSub_1_Rational.Rat_3'('Rational.Rat'(_6916682,_6916700),_6916536,_6916554,_6922232,_6922238,_6922244):-!,makeShare(_6916700,_6922518),makeShare(_6916554,_6922538),hnf('Rational.mkRat'('Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6916536,_6922518),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6916682,_6922538)),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6922538,_6922518)),_6922232,_6922238,_6922244).
'blocked_blocked_blocked_Rational.ratSub_1_Rational.Rat_3'('FAIL'(_6925894),_6916536,_6916554,'FAIL'(_6925894),_6925908,_6925908):-nonvar(_6925894).
'blocked_blocked_Rational.ratSub_1'('FAIL'(_6925980),_6916420,'FAIL'(_6925980),_6925994,_6925994):-nonvar(_6925980).

'Rational.ratMul'(_6926756,_6926758,_6926760,_6926762,_6926764):-freeze(_6926762,'blocked_Rational.ratMul'(_6926756,_6926758,_6926760,_6926762,_6926764)).
'blocked_Rational.ratMul'(_6926852,_6926870,_6927820,_6927826,_6927832):-hnf(_6926852,_6928746,_6927826,_6928764),'blocked_Rational.ratMul_1'(_6928746,_6926870,_6927820,_6928764,_6927832).

'blocked_Rational.ratMul_1'(_6929104,_6929106,_6929108,_6929110,_6929112):-freeze(_6929110,'blocked_blocked_Rational.ratMul_1'(_6929104,_6929106,_6929108,_6929110,_6929112)).
'blocked_blocked_Rational.ratMul_1'('Rational.Rat'(_6926986,_6927004),_6926870,_6929754,_6929760,_6929766):-!,hnf(_6926870,_6931430,_6929760,_6931454),'blocked_blocked_Rational.ratMul_1_Rational.Rat_3'(_6931430,_6926986,_6927004,_6929754,_6931454,_6929766).

'blocked_blocked_Rational.ratMul_1_Rational.Rat_3'(_6931958,_6931960,_6931962,_6931964,_6931966,_6931968):-freeze(_6931966,'blocked_blocked_blocked_Rational.ratMul_1_Rational.Rat_3'(_6931958,_6931960,_6931962,_6931964,_6931966,_6931968)).
'blocked_blocked_blocked_Rational.ratMul_1_Rational.Rat_3'('Rational.Rat'(_6927132,_6927150),_6926986,_6927004,_6932374,_6932380,_6932386):-!,hnf('Rational.mkRat'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6926986,_6927132),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6927004,_6927150)),_6932374,_6932380,_6932386).
'blocked_blocked_blocked_Rational.ratMul_1_Rational.Rat_3'('FAIL'(_6934500),_6926986,_6927004,'FAIL'(_6934500),_6934514,_6934514):-nonvar(_6934500).
'blocked_blocked_Rational.ratMul_1'('FAIL'(_6934586),_6926870,'FAIL'(_6934586),_6934600,_6934600):-nonvar(_6934586).

'Rational.ratDiv'(_6935362,_6935364,_6935366,_6935368,_6935370):-freeze(_6935368,'blocked_Rational.ratDiv'(_6935362,_6935364,_6935366,_6935368,_6935370)).
'blocked_Rational.ratDiv'(_6935458,_6935476,_6941118,_6941124,_6941130):-hnf(_6935458,_6942044,_6941124,_6942062),'blocked_Rational.ratDiv_1'(_6942044,_6935476,_6941118,_6942062,_6941130).

'blocked_Rational.ratDiv_1'(_6942402,_6942404,_6942406,_6942408,_6942410):-freeze(_6942408,'blocked_blocked_Rational.ratDiv_1'(_6942402,_6942404,_6942406,_6942408,_6942410)).
'blocked_blocked_Rational.ratDiv_1'('Rational.Rat'(_6935592,_6935610),_6935476,_6943052,_6943058,_6943064):-!,hnf(_6935476,_6944728,_6943058,_6944752),'blocked_blocked_Rational.ratDiv_1_Rational.Rat_3'(_6944728,_6935592,_6935610,_6943052,_6944752,_6943064).

'blocked_blocked_Rational.ratDiv_1_Rational.Rat_3'(_6945256,_6945258,_6945260,_6945262,_6945264,_6945266):-freeze(_6945264,'blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3'(_6945256,_6945258,_6945260,_6945262,_6945264,_6945266)).
'blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3'('Rational.Rat'(_6935738,_6935756),_6935592,_6935610,_6948386,_6948392,_6948398):-!,makeShare(_6935738,_6945966),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6945966,0),_6951012,_6948392,_6950966),'blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase'(_6951012,_6945966,_6935756,_6935592,_6935610,_6948386,_6950966,_6948398).

'blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase'(_6951702,_6951704,_6951706,_6951708,_6951710,_6951712,_6951714,_6951716):-freeze(_6951714,freeze(_6951702,'blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase'(_6951702,_6951704,_6951706,_6951708,_6951710,_6951712,_6951714,_6951716))).
'blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase'('Prelude.True',_6945966,_6935756,_6935592,_6935610,_6952126,_6952132,_6952138):-hnf('Prelude.error'(['^r','^a','^t','^D','^i','^v',^:,'^ ','^d','^i','^v','^i','^s','^i','^o','^n','^ ','^b','^y','^ ','^z','^e','^r','^o']),_6952126,_6952132,_6952138).
'blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase'('Prelude.False',_6945966,_6935756,_6935592,_6935610,_6959856,_6959862,_6959868):-!,hnf('Prelude.otherwise',_6963716,_6959862,_6963670),'blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_6963716,_6945966,_6935756,_6935592,_6935610,_6959856,_6963670,_6959868).

'blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_6964598,_6964600,_6964602,_6964604,_6964606,_6964608,_6964610,_6964612):-freeze(_6964610,freeze(_6964598,'blocked_blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_6964598,_6964600,_6964602,_6964604,_6964606,_6964608,_6964610,_6964612))).
'blocked_blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6945966,_6935756,_6935592,_6935610,_6965022,_6965028,_6965034):-hnf('Rational.mkRat'('Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6935592,_6935756),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_6935610,_6945966)),_6965022,_6965028,_6965034).
'blocked_blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6945966,_6935756,_6935592,_6935610,_6967858,_6967864,_6967870):-!,hnf(reportFailure4PAKCS('Rational.ratDiv',['Prelude.False']),_6967858,_6967864,_6967870).
'blocked_blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6969760),_6945966,_6935756,_6935592,_6935610,'FAIL'(_6969760),_6969774,_6969774).
'blocked_blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3_Rational.Rat_ComplexCase'('FAIL'(_6969858),_6945966,_6935756,_6935592,_6935610,'FAIL'(_6969858),_6969872,_6969872).
'blocked_blocked_blocked_Rational.ratDiv_1_Rational.Rat_3'('FAIL'(_6969956),_6935592,_6935610,'FAIL'(_6969956),_6969970,_6969970):-nonvar(_6969956).
'blocked_blocked_Rational.ratDiv_1'('FAIL'(_6970042),_6935476,'FAIL'(_6970042),_6970056,_6970056):-nonvar(_6970042).

'Rational.ratNeg'(_6970818,_6970820,_6970822,_6970824):-freeze(_6970822,'blocked_Rational.ratNeg'(_6970818,_6970820,_6970822,_6970824)).
'blocked_Rational.ratNeg'(_6970904,_6971478,_6971484,_6971490):-hnf(_6970904,_6972396,_6971484,_6972408),'blocked_Rational.ratNeg_1'(_6972396,_6971478,_6972408,_6971490).

'blocked_Rational.ratNeg_1'(_6972740,_6972742,_6972744,_6972746):-freeze(_6972744,'blocked_blocked_Rational.ratNeg_1'(_6972740,_6972742,_6972744,_6972746)).
'blocked_blocked_Rational.ratNeg_1'('Rational.Rat'(_6971020,_6971038),'Rational.Rat'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_6971020),_6971038),_6973142,_6973142):-!.
'blocked_blocked_Rational.ratNeg_1'('FAIL'(_6974300),'FAIL'(_6974300),_6974314,_6974314):-nonvar(_6974300).

'Rational.ratAbs'(_6975068,_6975070,_6975072,_6975074):-freeze(_6975072,'blocked_Rational.ratAbs'(_6975068,_6975070,_6975072,_6975074)).
'blocked_Rational.ratAbs'(_6975154,_6975728,_6975734,_6975740):-hnf(_6975154,_6976646,_6975734,_6976658),'blocked_Rational.ratAbs_1'(_6976646,_6975728,_6976658,_6975740).

'blocked_Rational.ratAbs_1'(_6976990,_6976992,_6976994,_6976996):-freeze(_6976994,'blocked_blocked_Rational.ratAbs_1'(_6976990,_6976992,_6976994,_6976996)).
'blocked_blocked_Rational.ratAbs_1'('Rational.Rat'(_6975270,_6975288),'Rational.Rat'('Rational.absInt'(_6975270),_6975288),_6977392,_6977392):-!.
'blocked_blocked_Rational.ratAbs_1'('FAIL'(_6978318),'FAIL'(_6978318),_6978332,_6978332):-nonvar(_6978318).

'Rational.ratSignum'(_6979200,_6979202,_6979204,_6979206):-freeze(_6979204,'blocked_Rational.ratSignum'(_6979200,_6979202,_6979204,_6979206)).
'blocked_Rational.ratSignum'(_6979286,_6981596,_6981602,_6981608):-hnf(_6979286,_6982622,_6981602,_6982634),'blocked_Rational.ratSignum_1'(_6982622,_6981596,_6982634,_6981608).

'blocked_Rational.ratSignum_1'(_6982984,_6982986,_6982988,_6982990):-freeze(_6982988,'blocked_blocked_Rational.ratSignum_1'(_6982984,_6982986,_6982988,_6982990)).
'blocked_blocked_Rational.ratSignum_1'('Rational.Rat'(_6979402,_6979420),_6985624,_6985630,_6985636):-!,makeShare(_6979402,_6983484),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_6983484),0),_6987502,_6985630,_6987468),'blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase'(_6987502,_6983484,_6979420,_6985624,_6987468,_6985636).

'blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase'(_6988068,_6988070,_6988072,_6988074,_6988076,_6988078):-freeze(_6988076,freeze(_6988068,'blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase'(_6988068,_6988070,_6988072,_6988074,_6988076,_6988078))).
'blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase'('Prelude.True',_6983484,_6979420,'Rational.Rat'(1,1),_6988478,_6988478).
'blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase'('Prelude.False',_6983484,_6979420,_6991308,_6991314,_6991320):-!,hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6983484,0),_6994420,_6991314,_6994386),'blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_6994420,_6983484,_6979420,_6991308,_6994386,_6991320).

'blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_6995178,_6995180,_6995182,_6995184,_6995186,_6995188):-freeze(_6995186,freeze(_6995178,'blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_6995178,_6995180,_6995182,_6995184,_6995186,_6995188))).
'blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6983484,_6979420,'Rational.Rat'(0,1),_6995588,_6995588).
'blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6983484,_6979420,_6998224,_6998230,_6998236):-!,hnf('Prelude.otherwise',_7002560,_6998230,_7002526),'blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7002560,_6983484,_6979420,_6998224,_7002526,_6998236).

'blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7003522,_7003524,_7003526,_7003528,_7003530,_7003532):-freeze(_7003530,freeze(_7003522,'blocked_blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7003522,_7003524,_7003526,_7003528,_7003530,_7003532))).
'blocked_blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6983484,_6979420,'Rational.Rat'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1),1),_7003932,_7003932).
'blocked_blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6983484,_6979420,_7005992,_7005998,_7006004):-!,hnf(reportFailure4PAKCS('Rational.ratSignum',['Prelude.False']),_7005992,_7005998,_7006004).
'blocked_blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_7007848),_6983484,_6979420,'FAIL'(_7007848),_7007862,_7007862).
'blocked_blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_7007930),_6983484,_6979420,'FAIL'(_7007930),_7007944,_7007944).
'blocked_blocked_blocked_Rational.ratSignum_1_Rational.Rat_ComplexCase'('FAIL'(_7008012),_6983484,_6979420,'FAIL'(_7008012),_7008026,_7008026).
'blocked_blocked_Rational.ratSignum_1'('FAIL'(_7008094),'FAIL'(_7008094),_7008108,_7008108):-nonvar(_7008094).

'Rational.ratInv'(_7008862,_7008864,_7008866,_7008868):-freeze(_7008866,'blocked_Rational.ratInv'(_7008862,_7008864,_7008866,_7008868)).
'blocked_Rational.ratInv'(_7008948,_7012282,_7012288,_7012294):-hnf(_7008948,_7013200,_7012288,_7013212),'blocked_Rational.ratInv_1'(_7013200,_7012282,_7013212,_7012294).

'blocked_Rational.ratInv_1'(_7013544,_7013546,_7013548,_7013550):-freeze(_7013548,'blocked_blocked_Rational.ratInv_1'(_7013544,_7013546,_7013548,_7013550)).
'blocked_blocked_Rational.ratInv_1'('Rational.Rat'(_7009064,_7009082),_7015908,_7015914,_7015920):-!,makeShare(_7009064,_7014094),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7014094,0),_7017678,_7015914,_7017644),'blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase'(_7017678,_7014094,_7009082,_7015908,_7017644,_7015920).

'blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase'(_7018226,_7018228,_7018230,_7018232,_7018234,_7018236):-freeze(_7018234,freeze(_7018226,'blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase'(_7018226,_7018228,_7018230,_7018232,_7018234,_7018236))).
'blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase'('Prelude.True',_7014094,_7009082,_7018630,_7018636,_7018642):-hnf('Prelude.error'(['^r','^a','^t','^I','^n','^v',^:,'^ ','^z','^e','^r','^o']),_7018630,_7018636,_7018642).
'blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase'('Prelude.False',_7014094,_7009082,_7023240,_7023246,_7023252):-!,hnf('Prelude.otherwise',_7026244,_7023246,_7026210),'blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_7026244,_7014094,_7009082,_7023240,_7026210,_7023252).

'blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_7026984,_7026986,_7026988,_7026990,_7026992,_7026994):-freeze(_7026992,freeze(_7026984,'blocked_blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'(_7026984,_7026986,_7026988,_7026990,_7026992,_7026994))).
'blocked_blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_7014094,_7009082,_7027388,_7027394,_7027400):-hnf('Rational.mkRat'(_7009082,_7014094),_7027388,_7027394,_7027400).
'blocked_blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_7014094,_7009082,_7028866,_7028872,_7028878):-!,hnf(reportFailure4PAKCS('Rational.ratInv',['Prelude.False']),_7028866,_7028872,_7028878).
'blocked_blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_7030482),_7014094,_7009082,'FAIL'(_7030482),_7030496,_7030496).
'blocked_blocked_blocked_Rational.ratInv_1_Rational.Rat_ComplexCase'('FAIL'(_7030564),_7014094,_7009082,'FAIL'(_7030564),_7030578,_7030578).
'blocked_blocked_Rational.ratInv_1'('FAIL'(_7030646),'FAIL'(_7030646),_7030660,_7030660):-nonvar(_7030646).

'Rational.ratPow'(_7031414,_7031416,_7031418,_7031420,_7031422):-freeze(_7031420,'blocked_Rational.ratPow'(_7031414,_7031416,_7031418,_7031420,_7031422)).
'blocked_Rational.ratPow'(_7031510,_7031528,_7035896,_7035902,_7035908):-makeShare(_7031528,_7034102),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7034102,0),_7036924,_7035902,_7036890),'blocked_Rational.ratPow_ComplexCase'(_7036924,_7031510,_7034102,_7035896,_7036890,_7035908).

'blocked_Rational.ratPow_ComplexCase'(_7037316,_7037318,_7037320,_7037322,_7037324,_7037326):-freeze(_7037324,freeze(_7037316,'blocked_blocked_Rational.ratPow_ComplexCase'(_7037316,_7037318,_7037320,_7037322,_7037324,_7037326))).
'blocked_blocked_Rational.ratPow_ComplexCase'('Prelude.True',_7031510,_7034102,'Rational.Rat'(1,1),_7037726,_7037726).
'blocked_blocked_Rational.ratPow_ComplexCase'('Prelude.False',_7031510,_7034102,_7041000,_7041006,_7041012):-!,makeShare(_7034102,_7038974),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23',_7038974),0),_7043176,_7041006,_7043142),'blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase'(_7043176,_7031510,_7038974,_7041000,_7043142,_7041012).

'blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase'(_7043790,_7043792,_7043794,_7043796,_7043798,_7043800):-freeze(_7043798,freeze(_7043790,'blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase'(_7043790,_7043792,_7043794,_7043796,_7043798,_7043800))).
'blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_7031510,_7038974,_7044194,_7044200,_7044206):-hnf('Rational.ratPow'('Rational.ratInv'(_7031510),'Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_7038974)),_7044194,_7044200,_7044206).
'blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_7031510,_7038974,_7047316,_7047322,_7047328):-!,hnf('Prelude.otherwise',_7050716,_7047322,_7050682),'blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7050716,_7031510,_7038974,_7047316,_7050682,_7047328).

'blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7051522,_7051524,_7051526,_7051528,_7051530,_7051532):-freeze(_7051530,freeze(_7051522,'blocked_blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_7051522,_7051524,_7051526,_7051528,_7051530,_7051532))).
'blocked_blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_7031510,_7038974,_7051926,_7051932,_7051938):-makeShare(_7031510,_7052060),hnf('Rational.ratMul'(_7052060,'Rational.ratPow'(_7052060,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_7038974,1))),_7051926,_7051932,_7051938).
'blocked_blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_7031510,_7038974,_7054500,_7054506,_7054512):-!,hnf(reportFailure4PAKCS('Rational.ratPow',['Prelude.False']),_7054500,_7054506,_7054512).
'blocked_blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_7056182),_7031510,_7038974,'FAIL'(_7056182),_7056196,_7056196).
'blocked_blocked_blocked_Rational.ratPow_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_7056264),_7031510,_7038974,'FAIL'(_7056264),_7056278,_7056278).
'blocked_blocked_Rational.ratPow_ComplexCase'('FAIL'(_7056346),_7031510,_7034102,'FAIL'(_7056346),_7056360,_7056360).

'Rational.ratEq'(_7057088,_7057090,_7057092,_7057094,_7057096):-freeze(_7057094,'blocked_Rational.ratEq'(_7057088,_7057090,_7057092,_7057094,_7057096)).
'blocked_Rational.ratEq'(_7057184,_7057202,_7058146,_7058152,_7058158):-hnf(_7057184,_7059036,_7058152,_7059054),'blocked_Rational.ratEq_1'(_7059036,_7057202,_7058146,_7059054,_7058158).

'blocked_Rational.ratEq_1'(_7059388,_7059390,_7059392,_7059394,_7059396):-freeze(_7059394,'blocked_blocked_Rational.ratEq_1'(_7059388,_7059390,_7059392,_7059394,_7059396)).
'blocked_blocked_Rational.ratEq_1'('Rational.Rat'(_7057318,_7057336),_7057202,_7060032,_7060038,_7060044):-!,hnf(_7057202,_7061672,_7060038,_7061696),'blocked_blocked_Rational.ratEq_1_Rational.Rat_3'(_7061672,_7057318,_7057336,_7060032,_7061696,_7060044).

'blocked_blocked_Rational.ratEq_1_Rational.Rat_3'(_7062194,_7062196,_7062198,_7062200,_7062202,_7062204):-freeze(_7062202,'blocked_blocked_blocked_Rational.ratEq_1_Rational.Rat_3'(_7062194,_7062196,_7062198,_7062200,_7062202,_7062204)).
'blocked_blocked_blocked_Rational.ratEq_1_Rational.Rat_3'('Rational.Rat'(_7057464,_7057482),_7057318,_7057336,_7062610,_7062616,_7062622):-!,hnf('Prelude.&&'('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7057318,_7057464),'Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7057336,_7057482)),_7062610,_7062616,_7062622).
'blocked_blocked_blocked_Rational.ratEq_1_Rational.Rat_3'('FAIL'(_7064794),_7057318,_7057336,'FAIL'(_7064794),_7064808,_7064808):-nonvar(_7064794).
'blocked_blocked_Rational.ratEq_1'('FAIL'(_7064880),_7057202,'FAIL'(_7064880),_7064894,_7064894):-nonvar(_7064880).

'Rational.ratCompare'(_7065808,_7065810,_7065812,_7065814,_7065816):-freeze(_7065814,'blocked_Rational.ratCompare'(_7065808,_7065810,_7065812,_7065814,_7065816)).
'blocked_Rational.ratCompare'(_7065904,_7065922,_7067064,_7067070,_7067076):-hnf(_7065904,_7068134,_7067070,_7068152),'blocked_Rational.ratCompare_1'(_7068134,_7065922,_7067064,_7068152,_7067076).

'blocked_Rational.ratCompare_1'(_7068516,_7068518,_7068520,_7068522,_7068524):-freeze(_7068522,'blocked_blocked_Rational.ratCompare_1'(_7068516,_7068518,_7068520,_7068522,_7068524)).
'blocked_blocked_Rational.ratCompare_1'('Rational.Rat'(_7066038,_7066056),_7065922,_7069190,_7069196,_7069202):-!,hnf(_7065922,_7071010,_7069196,_7071034),'blocked_blocked_Rational.ratCompare_1_Rational.Rat_3'(_7071010,_7066038,_7066056,_7069190,_7071034,_7069202).

'blocked_blocked_Rational.ratCompare_1_Rational.Rat_3'(_7071562,_7071564,_7071566,_7071568,_7071570,_7071572):-freeze(_7071570,'blocked_blocked_blocked_Rational.ratCompare_1_Rational.Rat_3'(_7071562,_7071564,_7071566,_7071568,_7071570,_7071572)).
'blocked_blocked_blocked_Rational.ratCompare_1_Rational.Rat_3'('Rational.Rat'(_7066184,_7066202),_7066038,_7066056,_7071978,_7071984,_7071990):-!,hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23compare\'23Prelude.Ord\'23Prelude.Int\'23','Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_7066038,_7066202)),'Prelude._impl\'23\'2A\'23Prelude.Num\'23Prelude.Int\'23'(_7066184,_7066056)),_7071978,_7071984,_7071990).
'blocked_blocked_blocked_Rational.ratCompare_1_Rational.Rat_3'('FAIL'(_7074746),_7066038,_7066056,'FAIL'(_7074746),_7074760,_7074760):-nonvar(_7074746).
'blocked_blocked_Rational.ratCompare_1'('FAIL'(_7074832),_7065922,'FAIL'(_7074832),_7074846,_7074846):-nonvar(_7074832).

'Rational.ratLe'(_7075570,_7075572,_7075574,_7075576,_7075578):-freeze(_7075576,'blocked_Rational.ratLe'(_7075570,_7075572,_7075574,_7075576,_7075578)).
'blocked_Rational.ratLe'(_7075666,_7075684,_7076190,_7076196,_7076202):-hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'2F\'3D\'23Prelude.Eq\'23Prelude.Ordering\'23','Rational.ratCompare'(_7075666,_7075684)),'Prelude.GT'),_7076190,_7076196,_7076202).

'Rational.ratLt'(_7078570,_7078572,_7078574,_7078576,_7078578):-freeze(_7078576,'blocked_Rational.ratLt'(_7078570,_7078572,_7078574,_7078576,_7078578)).
'blocked_Rational.ratLt'(_7078666,_7078684,_7079022,_7079028,_7079034):-hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Ordering\'23'('Rational.ratCompare'(_7078666,_7078684),'Prelude.LT'),_7079022,_7079028,_7079034).

'Rational.ratGe'(_7081028,_7081030,_7081032,_7081034,_7081036):-freeze(_7081034,'blocked_Rational.ratGe'(_7081028,_7081030,_7081032,_7081034,_7081036)).
'blocked_Rational.ratGe'(_7081124,_7081142,_7081648,_7081654,_7081660):-hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'2F\'3D\'23Prelude.Eq\'23Prelude.Ordering\'23','Rational.ratCompare'(_7081124,_7081142)),'Prelude.LT'),_7081648,_7081654,_7081660).

'Rational.ratGt'(_7084028,_7084030,_7084032,_7084034,_7084036):-freeze(_7084034,'blocked_Rational.ratGt'(_7084028,_7084030,_7084032,_7084034,_7084036)).
'blocked_Rational.ratGt'(_7084124,_7084142,_7084480,_7084486,_7084492):-hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Ordering\'23'('Rational.ratCompare'(_7084124,_7084142),'Prelude.GT'),_7084480,_7084486,_7084492).

'Rational.ratMin'(_7086524,_7086526,_7086528,_7086530,_7086532):-freeze(_7086530,'blocked_Rational.ratMin'(_7086524,_7086526,_7086528,_7086530,_7086532)).
'blocked_Rational.ratMin'(_7086620,_7086638,_7088298,_7088304,_7088310):-makeShare(_7086620,_7087114),makeShare(_7086638,_7087134),hnf('Rational.ratLe'(_7087114,_7087134),_7089326,_7088304,_7089292),'blocked_Rational.ratMin_ComplexCase'(_7089326,_7087114,_7087134,_7088298,_7089292,_7088310).

'blocked_Rational.ratMin_ComplexCase'(_7089730,_7089732,_7089734,_7089736,_7089738,_7089740):-freeze(_7089738,freeze(_7089730,'blocked_blocked_Rational.ratMin_ComplexCase'(_7089730,_7089732,_7089734,_7089736,_7089738,_7089740))).
'blocked_blocked_Rational.ratMin_ComplexCase'('Prelude.True',_7087114,_7087134,_7090134,_7090140,_7090146):-hnf(_7087114,_7090134,_7090140,_7090146).
'blocked_blocked_Rational.ratMin_ComplexCase'('Prelude.False',_7087114,_7087134,_7090972,_7090978,_7090984):-!,hnf(_7087134,_7090972,_7090978,_7090984).
'blocked_blocked_Rational.ratMin_ComplexCase'('FAIL'(_7091518),_7087114,_7087134,'FAIL'(_7091518),_7091532,_7091532).

'Rational.ratMax'(_7092298,_7092300,_7092302,_7092304,_7092306):-freeze(_7092304,'blocked_Rational.ratMax'(_7092298,_7092300,_7092302,_7092304,_7092306)).
'blocked_Rational.ratMax'(_7092394,_7092412,_7094072,_7094078,_7094084):-makeShare(_7092394,_7092888),makeShare(_7092412,_7092908),hnf('Rational.ratGe'(_7092888,_7092908),_7095100,_7094078,_7095066),'blocked_Rational.ratMax_ComplexCase'(_7095100,_7092888,_7092908,_7094072,_7095066,_7094084).

'blocked_Rational.ratMax_ComplexCase'(_7095504,_7095506,_7095508,_7095510,_7095512,_7095514):-freeze(_7095512,freeze(_7095504,'blocked_blocked_Rational.ratMax_ComplexCase'(_7095504,_7095506,_7095508,_7095510,_7095512,_7095514))).
'blocked_blocked_Rational.ratMax_ComplexCase'('Prelude.True',_7092888,_7092908,_7095908,_7095914,_7095920):-hnf(_7092888,_7095908,_7095914,_7095920).
'blocked_blocked_Rational.ratMax_ComplexCase'('Prelude.False',_7092888,_7092908,_7096746,_7096752,_7096758):-!,hnf(_7092908,_7096746,_7096752,_7096758).
'blocked_blocked_Rational.ratMax_ComplexCase'('FAIL'(_7097292),_7092888,_7092908,'FAIL'(_7097292),_7097306,_7097306).

'Rational.ratFloor'(_7098148,_7098150,_7098152,_7098154):-freeze(_7098152,'blocked_Rational.ratFloor'(_7098148,_7098150,_7098152,_7098154)).
'blocked_Rational.ratFloor'(_7098234,_7100044,_7100050,_7100056):-hnf(_7098234,_7101034,_7100050,_7101046),'blocked_Rational.ratFloor_1'(_7101034,_7100044,_7101046,_7100056).

'blocked_Rational.ratFloor_1'(_7101390,_7101392,_7101394,_7101396):-freeze(_7101394,'blocked_blocked_Rational.ratFloor_1'(_7101390,_7101392,_7101394,_7101396)).
'blocked_blocked_Rational.ratFloor_1'('Rational.Rat'(_7098350,_7098368),_7101786,_7101792,_7101798):-!,makeShare(_7098408,_7102024),makeShare(_7098350,_7102044),makeShare(_7098368,_7102064),hnf('Prelude.cond'(letrec4PAKCS(_7102024,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23div\'23Prelude.Integral\'23Prelude.Int\'23',_7102044),_7102064)),'Rational.ratFloor._\'23caseor0'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23','Prelude.apply'('Prelude.apply'('Prelude._impl\'23mod\'23Prelude.Integral\'23Prelude.Int\'23',_7102044),_7102064)),0),_7102024)),_7101786,_7101792,_7101798).
'blocked_blocked_Rational.ratFloor_1'('FAIL'(_7106760),'FAIL'(_7106760),_7106774,_7106774):-nonvar(_7106760).

'Rational.ratCeiling'(_7107680,_7107682,_7107684,_7107686):-freeze(_7107684,'blocked_Rational.ratCeiling'(_7107680,_7107682,_7107684,_7107686)).
'blocked_Rational.ratCeiling'(_7107766,_7109920,_7109926,_7109932):-hnf(_7107766,_7110982,_7109926,_7110994),'blocked_Rational.ratCeiling_1'(_7110982,_7109920,_7110994,_7109932).

'blocked_Rational.ratCeiling_1'(_7111350,_7111352,_7111354,_7111356):-freeze(_7111354,'blocked_blocked_Rational.ratCeiling_1'(_7111350,_7111352,_7111354,_7111356)).
'blocked_blocked_Rational.ratCeiling_1'('Rational.Rat'(_7107882,_7107900),_7111746,_7111752,_7111758):-!,makeShare(_7107940,_7112088),makeShare(_7107882,_7112108),makeShare(_7107900,_7112128),makeShare(_7107958,_7112148),hnf('Prelude.cond'(letrec4PAKCS(_7112088,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23div\'23Prelude.Integral\'23Prelude.Int\'23',_7112108),_7112128)),'Prelude.cond'(letrec4PAKCS(_7112148,'Prelude.apply'('Prelude.apply'('Prelude._impl\'23mod\'23Prelude.Integral\'23Prelude.Int\'23',_7112108),_7112128)),'Rational.ratCeiling._\'23caseor0'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_7112148),0),_7112088))),_7111746,_7111752,_7111758).
'blocked_blocked_Rational.ratCeiling_1'('FAIL'(_7117666),'FAIL'(_7117666),_7117680,_7117680):-nonvar(_7117666).

'Rational.showRat'(_7118472,_7118474,_7118476,_7118478):-freeze(_7118476,'blocked_Rational.showRat'(_7118472,_7118474,_7118476,_7118478)).
'blocked_Rational.showRat'(_7118558,_7120224,_7120230,_7120236):-hnf(_7118558,_7121178,_7120230,_7121190),'blocked_Rational.showRat_1'(_7121178,_7120224,_7121190,_7120236).

'blocked_Rational.showRat_1'(_7121528,_7121530,_7121532,_7121534):-freeze(_7121532,'blocked_blocked_Rational.showRat_1'(_7121528,_7121530,_7121532,_7121534)).
'blocked_blocked_Rational.showRat_1'('Rational.Rat'(_7118674,_7118692),_7123474,_7123480,_7123486):-!,makeShare(_7118692,_7122058),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_7122058,1),_7125280,_7123480,_7125246),'blocked_blocked_Rational.showRat_1_Rational.Rat_ComplexCase'(_7125280,_7118674,_7122058,_7123474,_7125246,_7123486).

'blocked_blocked_Rational.showRat_1_Rational.Rat_ComplexCase'(_7125834,_7125836,_7125838,_7125840,_7125842,_7125844):-freeze(_7125842,freeze(_7125834,'blocked_blocked_blocked_Rational.showRat_1_Rational.Rat_ComplexCase'(_7125834,_7125836,_7125838,_7125840,_7125842,_7125844))).
'blocked_blocked_blocked_Rational.showRat_1_Rational.Rat_ComplexCase'('Prelude.True',_7118674,_7122058,_7126238,_7126244,_7126250):-hnf('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_7118674),_7126238,_7126244,_7126250).
'blocked_blocked_blocked_Rational.showRat_1_Rational.Rat_ComplexCase'('Prelude.False',_7118674,_7122058,_7127832,_7127838,_7127844):-!,hnf('Prelude.++'('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_7118674),'Prelude.++'([^/],'Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_7122058))),_7127832,_7127838,_7127844).
'blocked_blocked_blocked_Rational.showRat_1_Rational.Rat_ComplexCase'('FAIL'(_7130624),_7118674,_7122058,'FAIL'(_7130624),_7130638,_7130638).
'blocked_blocked_Rational.showRat_1'('FAIL'(_7130706),'FAIL'(_7130706),_7130720,_7130720):-nonvar(_7130706).

'Rational.mkRat._\'23caseor0'(_7131844,_7131846,_7131848,_7131850,_7131852,_7131854):-freeze(_7131852,'blocked_Rational.mkRat._\'23caseor0'(_7131844,_7131846,_7131848,_7131850,_7131852,_7131854)).
'blocked_Rational.mkRat._\'23caseor0'(_7131950,_7131968,_7131986,_7132902,_7132908,_7132914):-hnf(_7131950,_7134232,_7132908,_7134256),'blocked_Rational.mkRat._\'23caseor0_1'(_7134232,_7131968,_7131986,_7132902,_7134256,_7132914).

'blocked_Rational.mkRat._\'23caseor0_1'(_7134676,_7134678,_7134680,_7134682,_7134684,_7134686):-freeze(_7134684,freeze(_7134676,'blocked_blocked_Rational.mkRat._\'23caseor0_1'(_7134676,_7134678,_7134680,_7134682,_7134684,_7134686))).
'blocked_blocked_Rational.mkRat._\'23caseor0_1'('Prelude.True',_7131968,_7131986,'Rational.Rat'('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_7131968),'Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(_7131986)),_7135086,_7135086).
'blocked_blocked_Rational.mkRat._\'23caseor0_1'('Prelude.False',_7131968,_7131986,'Rational.Rat'(_7131968,_7131986),_7137062,_7137062):-!.
'blocked_blocked_Rational.mkRat._\'23caseor0_1'('FAIL'(_7137870),_7131968,_7131986,'FAIL'(_7137870),_7137884,_7137884).

'Rational.ratFloor._\'23caseor0'(_7139134,_7139136,_7139138,_7139140,_7139142):-freeze(_7139140,'blocked_Rational.ratFloor._\'23caseor0'(_7139134,_7139136,_7139138,_7139140,_7139142)).
'blocked_Rational.ratFloor._\'23caseor0'(_7139230,_7139248,_7139860,_7139866,_7139872):-hnf(_7139230,_7141290,_7139866,_7141308),'blocked_Rational.ratFloor._\'23caseor0_1'(_7141290,_7139248,_7139860,_7141308,_7139872).

'blocked_Rational.ratFloor._\'23caseor0_1'(_7141738,_7141740,_7141742,_7141744,_7141746):-freeze(_7141744,freeze(_7141738,'blocked_blocked_Rational.ratFloor._\'23caseor0_1'(_7141738,_7141740,_7141742,_7141744,_7141746))).
'blocked_blocked_Rational.ratFloor._\'23caseor0_1'('Prelude.True',_7139248,_7142132,_7142138,_7142144):-hnf('Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_7139248,1),_7142132,_7142138,_7142144).
'blocked_blocked_Rational.ratFloor._\'23caseor0_1'('Prelude.False',_7139248,_7143414,_7143420,_7143426):-!,hnf(_7139248,_7143414,_7143420,_7143426).
'blocked_blocked_Rational.ratFloor._\'23caseor0_1'('FAIL'(_7143910),_7139248,'FAIL'(_7143910),_7143924,_7143924).

'Rational.ratCeiling._\'23caseor0'(_7145242,_7145244,_7145246,_7145248,_7145250):-freeze(_7145248,'blocked_Rational.ratCeiling._\'23caseor0'(_7145242,_7145244,_7145246,_7145248,_7145250)).
'blocked_Rational.ratCeiling._\'23caseor0'(_7145338,_7145356,_7145980,_7145986,_7145992):-hnf(_7145338,_7147482,_7145986,_7147500),'blocked_Rational.ratCeiling._\'23caseor0_1'(_7147482,_7145356,_7145980,_7147500,_7145992).

'blocked_Rational.ratCeiling._\'23caseor0_1'(_7147942,_7147944,_7147946,_7147948,_7147950):-freeze(_7147948,freeze(_7147942,'blocked_blocked_Rational.ratCeiling._\'23caseor0_1'(_7147942,_7147944,_7147946,_7147948,_7147950))).
'blocked_blocked_Rational.ratCeiling._\'23caseor0_1'('Prelude.True',_7145356,_7148336,_7148342,_7148348):-hnf('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(_7145356,1),_7148336,_7148342,_7148348).
'blocked_blocked_Rational.ratCeiling._\'23caseor0_1'('Prelude.False',_7145356,_7149630,_7149636,_7149642):-!,hnf(_7145356,_7149630,_7149636,_7149642).
'blocked_blocked_Rational.ratCeiling._\'23caseor0_1'('FAIL'(_7150138),_7145356,'FAIL'(_7150138),_7150152,_7150152).

:-costCenters(['']).




%%%%% Number of shared variables: 32

