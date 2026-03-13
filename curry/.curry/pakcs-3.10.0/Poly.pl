%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').
:-importModule('Rational').

:-curryModule('Poly').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23','_inst#Prelude.Eq#Poly.Poly#',1,'Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23','_impl#==#Prelude.Eq#Poly.Poly#',2,'Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._impl\'23\'2F\'3D\'23Prelude.Eq\'23Poly.Poly\'23','_impl#/=#Prelude.Eq#Poly.Poly#',0,'Poly._impl\'23\'2F\'3D\'23Prelude.Eq\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._inst\'23Prelude.Show\'23Poly.Poly\'23','_inst#Prelude.Show#Poly.Poly#',1,'Poly._inst\'23Prelude.Show\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23','_impl#show#Prelude.Show#Poly.Poly#',0,'Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._impl\'23showsPrec\'23Prelude.Show\'23Poly.Poly\'23','_impl#showsPrec#Prelude.Show#Poly.Poly#',0,'Poly._impl\'23showsPrec\'23Prelude.Show\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._impl\'23showList\'23Prelude.Show\'23Poly.Poly\'23','_impl#showList#Prelude.Show#Poly.Poly#',0,'Poly._impl\'23showList\'23Prelude.Show\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._inst\'23Prelude.Data\'23Poly.Poly\'23','_inst#Prelude.Data#Poly.Poly#',1,'Poly._inst\'23Prelude.Data\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23','_impl#===#Prelude.Data#Poly.Poly#',2,'Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly._impl\'23aValue\'23Prelude.Data\'23Poly.Poly\'23','_impl#aValue#Prelude.Data#Poly.Poly#',0,'Poly._impl\'23aValue\'23Prelude.Data\'23Poly.Poly\'23',nofix,notype).
functiontype('Poly.ratFromInt','Poly.ratFromInt',0,'Poly.ratFromInt',nofix,notype).
functiontype('Poly.lastElem','Poly.lastElem',1,'Poly.lastElem',nofix,notype).
functiontype('Poly.mkPoly',mkPoly,0,'Poly.mkPoly',nofix,notype).
functiontype('Poly.stripZeros','Poly.stripZeros',0,'Poly.stripZeros',nofix,notype).
functiontype('Poly.stripZeros._\'23lambda2','Poly.stripZeros._#lambda2',1,'Poly.stripZeros._\'23lambda2',nofix,notype).
functiontype('Poly.reverseList','Poly.reverseList',0,'Poly.reverseList',nofix,notype).
functiontype('Poly.zeroPoly',zeroPoly,0,'Poly.zeroPoly',nofix,notype).
functiontype('Poly.constPoly',constPoly,1,'Poly.constPoly',nofix,notype).
functiontype('Poly.monoX',monoX,0,'Poly.monoX',nofix,notype).
functiontype('Poly.degree',degree,1,'Poly.degree',nofix,notype).
functiontype('Poly.leadCoeff',leadCoeff,1,'Poly.leadCoeff',nofix,notype).
functiontype('Poly.evalPoly',evalPoly,2,'Poly.evalPoly',nofix,notype).
functiontype('Poly.evalPoly._\'23lambda6','Poly.evalPoly._#lambda6',3,'Poly.evalPoly._\'23lambda6',nofix,notype).
functiontype('Poly.scalePoly',scalePoly,2,'Poly.scalePoly',nofix,notype).
functiontype('Poly.addPoly',addPoly,2,'Poly.addPoly',nofix,notype).
functiontype('Poly.subPoly',subPoly,2,'Poly.subPoly',nofix,notype).
functiontype('Poly.zipWithDefault','Poly.zipWithDefault',4,'Poly.zipWithDefault',nofix,notype).
functiontype('Poly.mulPoly',mulPoly,2,'Poly.mulPoly',nofix,notype).
functiontype('Poly.mulPoly._\'23lambda10','Poly.mulPoly._#lambda10',3,'Poly.mulPoly._\'23lambda10',nofix,notype).
functiontype('Poly.mulPoly._\'23lambda10._\'23lambda14','Poly.mulPoly._#lambda10._#lambda14',4,'Poly.mulPoly._\'23lambda10._\'23lambda14',nofix,notype).
functiontype('Poly.mulPoly.addCoeffs.64','Poly.mulPoly.addCoeffs.64',2,'Poly.mulPoly.addCoeffs.64',nofix,notype).
functiontype('Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre','Poly.mulPoly.addCoeffs.64._#selFP2#pre',1,'Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre',nofix,notype).
functiontype('Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest','Poly.mulPoly.addCoeffs.64._#selFP3#rest',1,'Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest',nofix,notype).
functiontype('Poly.divModPoly',divModPoly,2,'Poly.divModPoly',nofix,notype).
functiontype('Poly.divModPoly.go.79','Poly.divModPoly.go.79',5,'Poly.divModPoly.go.79',nofix,notype).
functiontype('Poly.gcdPoly',gcdPoly,2,'Poly.gcdPoly',nofix,notype).
functiontype('Poly.monicPoly',monicPoly,1,'Poly.monicPoly',nofix,notype).
functiontype('Poly.monicPoly._\'23lambda22','Poly.monicPoly._#lambda22',2,'Poly.monicPoly._\'23lambda22',nofix,notype).
functiontype('Poly.diffPoly',diffPoly,1,'Poly.diffPoly',nofix,notype).
functiontype('Poly.diffPoly._\'23lambda24','Poly.diffPoly._#lambda24',2,'Poly.diffPoly._\'23lambda24',nofix,notype).
functiontype('Poly.composePoly',composePoly,2,'Poly.composePoly',nofix,notype).
functiontype('Poly.composePoly._\'23lambda26','Poly.composePoly._#lambda26',3,'Poly.composePoly._\'23lambda26',nofix,notype).
functiontype('Poly.squareFree',squareFree,1,'Poly.squareFree',nofix,notype).
functiontype('Poly.squareFree.go.141','Poly.squareFree.go.141',4,'Poly.squareFree.go.141',nofix,notype).
functiontype('Poly.showPoly',showPoly,1,'Poly.showPoly',nofix,notype).
functiontype('Poly.mulPoly._\'23caseor0','Poly.mulPoly._#caseor0',4,'Poly.mulPoly._\'23caseor0',nofix,notype).
functiontype('Poly.mulPoly.addCoeffs.64._\'23caseor0','Poly.mulPoly.addCoeffs.64._#caseor0',4,'Poly.mulPoly.addCoeffs.64._\'23caseor0',nofix,notype).
functiontype('Poly.divModPoly._\'23caseor0','Poly.divModPoly._#caseor0',1,'Poly.divModPoly._\'23caseor0',nofix,notype).
functiontype('Poly.divModPoly._\'23caseor0._\'23caseor0','Poly.divModPoly._#caseor0._#caseor0',6,'Poly.divModPoly._\'23caseor0._\'23caseor0',nofix,notype).
functiontype('Poly.divModPoly.go.79._\'23caseor0','Poly.divModPoly.go.79._#caseor0',1,'Poly.divModPoly.go.79._\'23caseor0',nofix,notype).
functiontype('Poly.squareFree.go.141._\'23caseor0','Poly.squareFree.go.141._#caseor0',4,'Poly.squareFree.go.141._\'23caseor0',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.
constructortype('Poly.Poly','Poly',1,'Poly',0,notype,[]).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23'(_6449114,_6449116,_6449118,_6449120):-freeze(_6449118,'blocked_Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23'(_6449114,_6449116,_6449118,_6449120)).
'blocked_Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23'(_6449200,_6449808,_6449814,_6449820):-hnf(_6449200,_6451554,_6449814,_6451566),'blocked_Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23_1'(_6451554,_6449808,_6451566,_6449820).

'blocked_Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23_1'(_6452036,_6452038,_6452040,_6452042):-freeze(_6452040,'blocked_blocked_Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23_1'(_6452036,_6452038,_6452040,_6452042)).
'blocked_blocked_Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23_1'('Prelude.()','Prelude._Dict\'23Eq'(partcall(2,'Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23',[]),'Poly._impl\'23\'2F\'3D\'23Prelude.Eq\'23Poly.Poly\'23'),_6452388,_6452388):-!.
'blocked_blocked_Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23_1'('FAIL'(_6453756),'FAIL'(_6453756),_6453770,_6453770):-nonvar(_6453756).

'Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23'(_6455484,_6455486,_6455488,_6455490,_6455492):-freeze(_6455490,'blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23'(_6455484,_6455486,_6455488,_6455490,_6455492)).
'blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23'(_6455580,_6455598,_6456468,_6456474,_6456480):-hnf(_6455580,_6458546,_6456474,_6458564),'blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1'(_6458546,_6455598,_6456468,_6458564,_6456480).

'blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1'(_6459096,_6459098,_6459100,_6459102,_6459104):-freeze(_6459102,'blocked_blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1'(_6459096,_6459098,_6459100,_6459102,_6459104)).
'blocked_blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1'('Poly.Poly'(_6455714),_6455598,_6459886,_6459892,_6459898):-!,hnf(_6455598,_6462624,_6459892,_6462642),'blocked_blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1_Poly.Poly_2'(_6462624,_6455714,_6459886,_6462642,_6459898).

'blocked_blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1_Poly.Poly_2'(_6463312,_6463314,_6463316,_6463318,_6463320):-freeze(_6463318,'blocked_blocked_blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1_Poly.Poly_2'(_6463312,_6463314,_6463316,_6463318,_6463320)).
'blocked_blocked_blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1_Poly.Poly_2'('Poly.Poly'(_6455836),_6455714,_6463674,_6463680,_6463686):-!,hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23\'5B\'5D\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Eq\'23Rational.Rational\'23',[]),_6455714,_6455836),_6463674,_6463680,_6463686).
'blocked_blocked_blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1_Poly.Poly_2'('FAIL'(_6465422),_6455714,'FAIL'(_6465422),_6465436,_6465436):-nonvar(_6465422).
'blocked_blocked_Poly._impl\'23\'3D\'3D\'23Prelude.Eq\'23Poly.Poly\'23_1'('FAIL'(_6465500),_6455598,'FAIL'(_6465500),_6465514,_6465514):-nonvar(_6465500).

'Poly._impl\'23\'2F\'3D\'23Prelude.Eq\'23Poly.Poly\'23'(_6467236,_6467238,_6467240):-freeze(_6467238,'blocked_Poly._impl\'23\'2F\'3D\'23Prelude.Eq\'23Poly.Poly\'23'(_6467236,_6467238,_6467240)).
'blocked_Poly._impl\'23\'2F\'3D\'23Prelude.Eq\'23Poly.Poly\'23'(_6467408,_6467414,_6467420):-hnf(partcall(2,'Prelude._def\'23\'2F\'3D\'23Prelude.Eq',[partcall(1,'Poly._inst\'23Prelude.Eq\'23Poly.Poly\'23',[])]),_6467408,_6467414,_6467420).

'Poly._inst\'23Prelude.Show\'23Poly.Poly\'23'(_6469964,_6469966,_6469968,_6469970):-freeze(_6469968,'blocked_Poly._inst\'23Prelude.Show\'23Poly.Poly\'23'(_6469964,_6469966,_6469968,_6469970)).
'blocked_Poly._inst\'23Prelude.Show\'23Poly.Poly\'23'(_6470050,_6470754,_6470760,_6470766):-hnf(_6470050,_6472572,_6470760,_6472584),'blocked_Poly._inst\'23Prelude.Show\'23Poly.Poly\'23_1'(_6472572,_6470754,_6472584,_6470766).

'blocked_Poly._inst\'23Prelude.Show\'23Poly.Poly\'23_1'(_6473066,_6473068,_6473070,_6473072):-freeze(_6473070,'blocked_blocked_Poly._inst\'23Prelude.Show\'23Poly.Poly\'23_1'(_6473066,_6473068,_6473070,_6473072)).
'blocked_blocked_Poly._inst\'23Prelude.Show\'23Poly.Poly\'23_1'('Prelude.()','Prelude._Dict\'23Show'('Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23','Poly._impl\'23showsPrec\'23Prelude.Show\'23Poly.Poly\'23','Poly._impl\'23showList\'23Prelude.Show\'23Poly.Poly\'23'),_6473418,_6473418):-!.
'blocked_blocked_Poly._inst\'23Prelude.Show\'23Poly.Poly\'23_1'('FAIL'(_6475254),'FAIL'(_6475254),_6475268,_6475268):-nonvar(_6475254).

'Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23'(_6477078,_6477080,_6477082):-freeze(_6477080,'blocked_Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23'(_6477078,_6477080,_6477082)).
'blocked_Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23'(_6477166,_6477172,_6477178):-hnf(partcall(1,'Poly.showPoly',[]),_6477166,_6477172,_6477178).

'Poly._impl\'23showsPrec\'23Prelude.Show\'23Poly.Poly\'23'(_6479652,_6479654,_6479656):-freeze(_6479654,'blocked_Poly._impl\'23showsPrec\'23Prelude.Show\'23Poly.Poly\'23'(_6479652,_6479654,_6479656)).
'blocked_Poly._impl\'23showsPrec\'23Prelude.Show\'23Poly.Poly\'23'(_6479824,_6479830,_6479836):-hnf(partcall(3,'Prelude._def\'23showsPrec\'23Prelude.Show',[partcall(1,'Poly._inst\'23Prelude.Show\'23Poly.Poly\'23',[])]),_6479824,_6479830,_6479836).

'Poly._impl\'23showList\'23Prelude.Show\'23Poly.Poly\'23'(_6482822,_6482824,_6482826):-freeze(_6482824,'blocked_Poly._impl\'23showList\'23Prelude.Show\'23Poly.Poly\'23'(_6482822,_6482824,_6482826)).
'blocked_Poly._impl\'23showList\'23Prelude.Show\'23Poly.Poly\'23'(_6482994,_6483000,_6483006):-hnf('Prelude._def\'23showList\'23Prelude.Show'(partcall(1,'Poly._inst\'23Prelude.Show\'23Poly.Poly\'23',[])),_6482994,_6483000,_6483006).

'Poly._inst\'23Prelude.Data\'23Poly.Poly\'23'(_6485604,_6485606,_6485608,_6485610):-freeze(_6485608,'blocked_Poly._inst\'23Prelude.Data\'23Poly.Poly\'23'(_6485604,_6485606,_6485608,_6485610)).
'blocked_Poly._inst\'23Prelude.Data\'23Poly.Poly\'23'(_6485690,_6486310,_6486316,_6486322):-hnf(_6485690,_6488128,_6486316,_6488140),'blocked_Poly._inst\'23Prelude.Data\'23Poly.Poly\'23_1'(_6488128,_6486310,_6488140,_6486322).

'blocked_Poly._inst\'23Prelude.Data\'23Poly.Poly\'23_1'(_6488622,_6488624,_6488626,_6488628):-freeze(_6488626,'blocked_blocked_Poly._inst\'23Prelude.Data\'23Poly.Poly\'23_1'(_6488622,_6488624,_6488626,_6488628)).
'blocked_blocked_Poly._inst\'23Prelude.Data\'23Poly.Poly\'23_1'('Prelude.()','Prelude._Dict\'23Data'(partcall(2,'Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23',[]),'Poly._impl\'23aValue\'23Prelude.Data\'23Poly.Poly\'23'),_6488974,_6488974):-!.
'blocked_blocked_Poly._inst\'23Prelude.Data\'23Poly.Poly\'23_1'('FAIL'(_6490408),'FAIL'(_6490408),_6490422,_6490422):-nonvar(_6490408).

'Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23'(_6492278,_6492280,_6492282,_6492284,_6492286):-freeze(_6492284,'blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23'(_6492278,_6492280,_6492282,_6492284,_6492286)).
'blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23'(_6492374,_6492392,_6493292,_6493298,_6493304):-hnf(_6492374,_6495550,_6493298,_6495568),'blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1'(_6495550,_6492392,_6493292,_6495568,_6493304).

'blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1'(_6496130,_6496132,_6496134,_6496136,_6496138):-freeze(_6496136,'blocked_blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1'(_6496130,_6496132,_6496134,_6496136,_6496138)).
'blocked_blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1'('Poly.Poly'(_6492508),_6492392,_6496950,_6496956,_6496962):-!,hnf(_6492392,_6499868,_6496956,_6499886),'blocked_blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1_Poly.Poly_2'(_6499868,_6492508,_6496950,_6499886,_6496962).

'blocked_blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1_Poly.Poly_2'(_6500586,_6500588,_6500590,_6500592,_6500594):-freeze(_6500592,'blocked_blocked_blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1_Poly.Poly_2'(_6500586,_6500588,_6500590,_6500592,_6500594)).
'blocked_blocked_blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1_Poly.Poly_2'('Poly.Poly'(_6492630),_6492508,_6500948,_6500954,_6500960):-!,hnf('Prelude._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23\'5B\'5D\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Data\'23Rational.Rational\'23',[]),_6492508,_6492630),_6500948,_6500954,_6500960).
'blocked_blocked_blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1_Poly.Poly_2'('FAIL'(_6502768),_6492508,'FAIL'(_6502768),_6502782,_6502782):-nonvar(_6502768).
'blocked_blocked_Poly._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Poly.Poly\'23_1'('FAIL'(_6502846),_6492392,'FAIL'(_6502846),_6502860,_6502860):-nonvar(_6502846).

'Poly._impl\'23aValue\'23Prelude.Data\'23Poly.Poly\'23'(_6504754,_6504756,_6504758):-freeze(_6504756,'blocked_Poly._impl\'23aValue\'23Prelude.Data\'23Poly.Poly\'23'(_6504754,_6504756,_6504758)).
'blocked_Poly._impl\'23aValue\'23Prelude.Data\'23Poly.Poly\'23'('Poly.Poly'('Prelude._impl\'23aValue\'23Prelude.Data\'23\'5B\'5D\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Data\'23Rational.Rational\'23',[]))),_6505016,_6505016).

'Poly.ratFromInt'(_6507096,_6507098,_6507100):-freeze(_6507098,'blocked_Poly.ratFromInt'(_6507096,_6507098,_6507100)).
'blocked_Poly.ratFromInt'(_6507184,_6507190,_6507196):-hnf(partcall(1,'Rational.fromInt',[]),_6507184,_6507190,_6507196).

'Poly.lastElem'(_6508174,_6508176,_6508178,_6508180):-freeze(_6508178,'blocked_Poly.lastElem'(_6508174,_6508176,_6508178,_6508180)).
'blocked_Poly.lastElem'(_6508260,_6512170,_6512176,_6512182):-hnf(_6508260,_6513016,_6512176,_6513028),'blocked_Poly.lastElem_1'(_6513016,_6512170,_6513028,_6512182).

'blocked_Poly.lastElem_1'(_6513354,_6513356,_6513358,_6513360):-freeze(_6513358,freeze(_6513354,'blocked_blocked_Poly.lastElem_1'(_6513354,_6513356,_6513358,_6513360))).
'blocked_blocked_Poly.lastElem_1'([_6508376|_6508394],_6514396,_6514402,_6514408):-makeShare(_6508394,_6513640),hnf(_6513640,_6515722,_6514402,_6515746),'blocked_blocked_Poly.lastElem_1_[|]_2'(_6515722,_6508376,_6515722,_6514396,_6515746,_6514408).

'blocked_blocked_Poly.lastElem_1_[|]_2'(_6516184,_6516186,_6516188,_6516190,_6516192,_6516194):-freeze(_6516192,freeze(_6516184,'blocked_blocked_blocked_Poly.lastElem_1_[|]_2'(_6516184,_6516186,_6516188,_6516190,_6516192,_6516194))).
'blocked_blocked_blocked_Poly.lastElem_1_[|]_2'([],_6508376,_6513640,_6516452,_6516458,_6516464):-hnf(_6508376,_6516452,_6516458,_6516464).
'blocked_blocked_blocked_Poly.lastElem_1_[|]_2'([_6508608|_6508626],_6508376,_6513640,_6517182,_6517188,_6517194):-!,hnf('Poly.lastElem'(_6513640),_6517182,_6517188,_6517194).
'blocked_blocked_blocked_Poly.lastElem_1_[|]_2'('FAIL'(_6518060),_6508376,_6513640,'FAIL'(_6518060),_6518074,_6518074).
'blocked_blocked_Poly.lastElem_1'([],_6518304,_6518310,_6518316):-!,hnf('Prelude.error'(['^l','^a','^s','^t','^E','^l','^e','^m',^:,'^ ','^e','^m','^p','^t','^y','^ ','^l','^i','^s','^t']),_6518304,_6518310,_6518316).
'blocked_blocked_Poly.lastElem_1'('FAIL'(_6522870),'FAIL'(_6522870),_6522884,_6522884).

'Poly.mkPoly'(_6523482,_6523484,_6523486):-freeze(_6523484,'blocked_Poly.mkPoly'(_6523482,_6523484,_6523486)).
'blocked_Poly.mkPoly'(_6523738,_6523744,_6523750):-hnf('Prelude..'('Poly.Poly','Poly.stripZeros'),_6523738,_6523744,_6523750).

'Poly.stripZeros'(_6525136,_6525138,_6525140):-freeze(_6525138,'blocked_Poly.stripZeros'(_6525136,_6525138,_6525140)).
'blocked_Poly.stripZeros'(_6525644,_6525650,_6525656):-hnf('Prelude..'('Poly.reverseList','Prelude..'(partcall(1,'Prelude.dropWhile',[partcall(1,'Poly.stripZeros._\'23lambda2',[])]),'Poly.reverseList')),_6525644,_6525650,_6525656).

'Poly.stripZeros._\'23lambda2'(_6528218,_6528220,_6528222,_6528224):-freeze(_6528222,'blocked_Poly.stripZeros._\'23lambda2'(_6528218,_6528220,_6528222,_6528224)).
'blocked_Poly.stripZeros._\'23lambda2'(_6528304,_6528810,_6528816,_6528822):-hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_6528304),'Prelude.apply'('Poly.ratFromInt',0)),_6528810,_6528816,_6528822).

'Poly.reverseList'(_6531252,_6531254,_6531256):-freeze(_6531254,'blocked_Poly.reverseList'(_6531252,_6531254,_6531256)).
'blocked_Poly.reverseList'(_6531592,_6531598,_6531604):-hnf(partcall(1,'Prelude.foldl',[[],partcall(2,'Prelude.flip',['[|]'])]),_6531592,_6531598,_6531604).

'Poly.zeroPoly'(_6532978,_6532980,_6532982):-freeze(_6532980,'blocked_Poly.zeroPoly'(_6532978,_6532980,_6532982)).
'blocked_Poly.zeroPoly'('Poly.Poly'([]),_6533156,_6533156).

'Poly.constPoly'(_6534208,_6534210,_6534212,_6534214):-freeze(_6534212,'blocked_Poly.constPoly'(_6534208,_6534210,_6534212,_6534214)).
'blocked_Poly.constPoly'(_6534294,_6538092,_6538098,_6538104):-makeShare(_6534294,_6535900),hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_6535900),'Prelude.apply'('Poly.ratFromInt',0)),_6539070,_6538098,_6539042),'blocked_Poly.constPoly_ComplexCase'(_6539070,_6535900,_6538092,_6539042,_6538104).

'blocked_Poly.constPoly_ComplexCase'(_6539454,_6539456,_6539458,_6539460,_6539462):-freeze(_6539460,freeze(_6539454,'blocked_blocked_Poly.constPoly_ComplexCase'(_6539454,_6539456,_6539458,_6539460,_6539462))).
'blocked_blocked_Poly.constPoly_ComplexCase'('Prelude.True',_6535900,_6539848,_6539854,_6539860):-hnf('Poly.zeroPoly',_6539848,_6539854,_6539860).
'blocked_blocked_Poly.constPoly_ComplexCase'('Prelude.False',_6535900,_6541496,_6541502,_6541508):-!,hnf('Prelude.otherwise',_6543622,_6541502,_6543594),'blocked_blocked_Poly.constPoly_ComplexCase_Prelude.False_ComplexCase'(_6543622,_6535900,_6541496,_6543594,_6541508).

'blocked_blocked_Poly.constPoly_ComplexCase_Prelude.False_ComplexCase'(_6544216,_6544218,_6544220,_6544222,_6544224):-freeze(_6544222,freeze(_6544216,'blocked_blocked_blocked_Poly.constPoly_ComplexCase_Prelude.False_ComplexCase'(_6544216,_6544218,_6544220,_6544222,_6544224))).
'blocked_blocked_blocked_Poly.constPoly_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6535900,'Poly.Poly'([_6535900]),_6544616,_6544616).
'blocked_blocked_blocked_Poly.constPoly_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6535900,_6545932,_6545938,_6545944):-!,hnf(reportFailure4PAKCS('Poly.constPoly',['Prelude.False']),_6545932,_6545938,_6545944).
'blocked_blocked_blocked_Poly.constPoly_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6547324),_6535900,'FAIL'(_6547324),_6547338,_6547338).
'blocked_blocked_Poly.constPoly_ComplexCase'('FAIL'(_6547398),_6535900,'FAIL'(_6547398),_6547412,_6547412).

'Poly.monoX'(_6547980,_6547982,_6547984):-freeze(_6547982,'blocked_Poly.monoX'(_6547980,_6547982,_6547984)).
'blocked_Poly.monoX'('Poly.Poly'(['Prelude.apply'('Poly.ratFromInt',0),'Prelude.apply'('Poly.ratFromInt',1)]),_6548802,_6548802).

'Poly.degree'(_6550874,_6550876,_6550878,_6550880):-freeze(_6550878,'blocked_Poly.degree'(_6550874,_6550876,_6550878,_6550880)).
'blocked_Poly.degree'(_6550960,_6551808,_6551814,_6551820):-hnf(_6550960,_6552582,_6551814,_6552594),'blocked_Poly.degree_1'(_6552582,_6551808,_6552594,_6551820).

'blocked_Poly.degree_1'(_6552902,_6552904,_6552906,_6552908):-freeze(_6552906,'blocked_blocked_Poly.degree_1'(_6552902,_6552904,_6552906,_6552908)).
'blocked_blocked_Poly.degree_1'('Poly.Poly'(_6551076),_6553972,_6553978,_6553984):-!,makeShare(_6551076,_6553256),hnf(_6553256,_6555406,_6553978,_6555424),'blocked_blocked_Poly.degree_1_Poly.Poly_1'(_6555406,_6555406,_6553972,_6555424,_6553984).

'blocked_blocked_Poly.degree_1_Poly.Poly_1'(_6555896,_6555898,_6555900,_6555902,_6555904):-freeze(_6555902,freeze(_6555896,'blocked_blocked_blocked_Poly.degree_1_Poly.Poly_1'(_6555896,_6555898,_6555900,_6555902,_6555904))).
'blocked_blocked_blocked_Poly.degree_1_Poly.Poly_1'([],_6553256,_6556154,_6556160,_6556166):-hnf('Prelude._impl\'23negate\'23Prelude.Num\'23Prelude.Int\'23'(1),_6556154,_6556160,_6556166).
'blocked_blocked_blocked_Poly.degree_1_Poly.Poly_1'([_6551368|_6551386],_6553256,_6557254,_6557260,_6557266):-!,hnf('Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'('Prelude.length'(_6553256),1),_6557254,_6557260,_6557266).
'blocked_blocked_blocked_Poly.degree_1_Poly.Poly_1'('FAIL'(_6558582),_6553256,'FAIL'(_6558582),_6558596,_6558596).
'blocked_blocked_Poly.degree_1'('FAIL'(_6558656),'FAIL'(_6558656),_6558670,_6558670):-nonvar(_6558656).

'Poly.leadCoeff'(_6559386,_6559388,_6559390,_6559392):-freeze(_6559390,'blocked_Poly.leadCoeff'(_6559386,_6559388,_6559390,_6559392)).
'blocked_Poly.leadCoeff'(_6559472,_6560198,_6560204,_6560210):-hnf(_6559472,_6561080,_6560204,_6561092),'blocked_Poly.leadCoeff_1'(_6561080,_6560198,_6561092,_6560210).

'blocked_Poly.leadCoeff_1'(_6561418,_6561420,_6561422,_6561424):-freeze(_6561422,'blocked_blocked_Poly.leadCoeff_1'(_6561418,_6561420,_6561422,_6561424)).
'blocked_blocked_Poly.leadCoeff_1'('Poly.Poly'(_6559588),_6562486,_6562492,_6562498):-!,makeShare(_6559588,_6561772),hnf(_6561772,_6564028,_6562492,_6564046),'blocked_blocked_Poly.leadCoeff_1_Poly.Poly_1'(_6564028,_6564028,_6562486,_6564046,_6562498).

'blocked_blocked_Poly.leadCoeff_1_Poly.Poly_1'(_6564536,_6564538,_6564540,_6564542,_6564544):-freeze(_6564542,freeze(_6564536,'blocked_blocked_blocked_Poly.leadCoeff_1_Poly.Poly_1'(_6564536,_6564538,_6564540,_6564542,_6564544))).
'blocked_blocked_blocked_Poly.leadCoeff_1_Poly.Poly_1'([],_6561772,'Prelude.Nothing',_6564800,_6564800).
'blocked_blocked_blocked_Poly.leadCoeff_1_Poly.Poly_1'([_6559810|_6559828],_6561772,'Prelude.Just'('Poly.lastElem'(_6561772)),_6565586,_6565586):-!.
'blocked_blocked_blocked_Poly.leadCoeff_1_Poly.Poly_1'('FAIL'(_6566584),_6561772,'FAIL'(_6566584),_6566598,_6566598).
'blocked_blocked_Poly.leadCoeff_1'('FAIL'(_6566658),'FAIL'(_6566658),_6566672,_6566672):-nonvar(_6566658).

'Poly.evalPoly'(_6567350,_6567352,_6567354,_6567356,_6567358):-freeze(_6567356,'blocked_Poly.evalPoly'(_6567350,_6567352,_6567354,_6567356,_6567358)).
'blocked_Poly.evalPoly'(_6567446,_6567464,_6568646,_6568652,_6568658):-hnf(_6567446,_6569500,_6568652,_6569518),'blocked_Poly.evalPoly_1'(_6569500,_6567464,_6568646,_6569518,_6568658).

'blocked_Poly.evalPoly_1'(_6569846,_6569848,_6569850,_6569852,_6569854):-freeze(_6569852,'blocked_blocked_Poly.evalPoly_1'(_6569846,_6569848,_6569850,_6569852,_6569854)).
'blocked_blocked_Poly.evalPoly_1'('Poly.Poly'(_6567580),_6567464,_6571056,_6571062,_6571068):-!,makeShare(_6567580,_6570210),hnf(_6570210,_6572570,_6571062,_6572594),'blocked_blocked_Poly.evalPoly_1_Poly.Poly_1'(_6572570,_6572570,_6567464,_6571056,_6572594,_6571068).

'blocked_blocked_Poly.evalPoly_1_Poly.Poly_1'(_6573086,_6573088,_6573090,_6573092,_6573094,_6573096):-freeze(_6573094,freeze(_6573086,'blocked_blocked_blocked_Poly.evalPoly_1_Poly.Poly_1'(_6573086,_6573088,_6573090,_6573092,_6573094,_6573096))).
'blocked_blocked_blocked_Poly.evalPoly_1_Poly.Poly_1'([],_6570210,_6567464,_6573354,_6573360,_6573366):-hnf('Prelude.apply'('Poly.ratFromInt',0),_6573354,_6573360,_6573366).
'blocked_blocked_blocked_Poly.evalPoly_1_Poly.Poly_1'([_6567956|_6567974],_6570210,_6567464,_6574494,_6574500,_6574506):-!,hnf('Prelude.foldr'(partcall(2,'Poly.evalPoly._\'23lambda6',[_6567464]),'Prelude.apply'('Poly.ratFromInt',0),_6570210),_6574494,_6574500,_6574506).
'blocked_blocked_blocked_Poly.evalPoly_1_Poly.Poly_1'('FAIL'(_6576244),_6570210,_6567464,'FAIL'(_6576244),_6576258,_6576258).
'blocked_blocked_Poly.evalPoly_1'('FAIL'(_6576326),_6567464,'FAIL'(_6576326),_6576340,_6576340):-nonvar(_6576326).

'Poly.evalPoly._\'23lambda6'(_6577434,_6577436,_6577438,_6577440,_6577442,_6577444):-freeze(_6577442,'blocked_Poly.evalPoly._\'23lambda6'(_6577434,_6577436,_6577438,_6577440,_6577442,_6577444)).
'blocked_Poly.evalPoly._\'23lambda6'(_6577540,_6577558,_6577576,_6577900,_6577906,_6577912):-hnf('Rational.ratAdd'(_6577558,'Rational.ratMul'(_6577540,_6577576)),_6577900,_6577906,_6577912).

'Poly.scalePoly'(_6579714,_6579716,_6579718,_6579720,_6579722):-freeze(_6579720,'blocked_Poly.scalePoly'(_6579714,_6579716,_6579718,_6579720,_6579722)).
'blocked_Poly.scalePoly'(_6579810,_6579828,_6581796,_6581802,_6581808):-hnf(_6579828,_6582686,_6581802,_6582704),'blocked_Poly.scalePoly_2'(_6582686,_6579810,_6581796,_6582704,_6581808).

'blocked_Poly.scalePoly_2'(_6583038,_6583040,_6583042,_6583044,_6583046):-freeze(_6583044,'blocked_blocked_Poly.scalePoly_2'(_6583038,_6583040,_6583042,_6583044,_6583046)).
'blocked_blocked_Poly.scalePoly_2'('Poly.Poly'(_6579944),_6579810,_6585860,_6585866,_6585872):-!,makeShare(_6579810,_6583518),hnf('Prelude.apply'('Prelude.apply'('Rational._impl\'23\'3D\'3D\'23Prelude.Eq\'23Rational.Rational\'23',_6583518),'Prelude.apply'('Poly.ratFromInt',0)),_6587512,_6585866,_6587478),'blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase'(_6587512,_6579944,_6583518,_6585860,_6587478,_6585872).

'blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase'(_6588036,_6588038,_6588040,_6588042,_6588044,_6588046):-freeze(_6588044,freeze(_6588036,'blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase'(_6588036,_6588038,_6588040,_6588042,_6588044,_6588046))).
'blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase'('Prelude.True',_6579944,_6583518,_6588440,_6588446,_6588452):-hnf('Poly.zeroPoly',_6588440,_6588446,_6588452).
'blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase'('Prelude.False',_6579944,_6583518,_6590520,_6590526,_6590532):-!,hnf('Prelude.otherwise',_6593380,_6590526,_6593346),'blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase_Prelude.False_ComplexCase'(_6593380,_6579944,_6583518,_6590520,_6593346,_6590532).

'blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase_Prelude.False_ComplexCase'(_6594096,_6594098,_6594100,_6594102,_6594104,_6594106):-freeze(_6594104,freeze(_6594096,'blocked_blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase_Prelude.False_ComplexCase'(_6594096,_6594098,_6594100,_6594102,_6594104,_6594106))).
'blocked_blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6579944,_6583518,_6594500,_6594506,_6594512):-hnf('Prelude.apply'('Poly.mkPoly','Prelude.map'(partcall(1,'Rational.ratMul',[_6583518]),_6579944)),_6594500,_6594506,_6594512).
'blocked_blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6579944,_6583518,_6596504,_6596510,_6596516):-!,hnf(reportFailure4PAKCS('Poly.scalePoly',['Prelude.False']),_6596504,_6596510,_6596516).
'blocked_blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6598090),_6579944,_6583518,'FAIL'(_6598090),_6598104,_6598104).
'blocked_blocked_blocked_Poly.scalePoly_2_Poly.Poly_ComplexCase'('FAIL'(_6598172),_6579944,_6583518,'FAIL'(_6598172),_6598186,_6598186).
'blocked_blocked_Poly.scalePoly_2'('FAIL'(_6598254),_6579810,'FAIL'(_6598254),_6598268,_6598268):-nonvar(_6598254).

'Poly.addPoly'(_6598916,_6598918,_6598920,_6598922,_6598924):-freeze(_6598922,'blocked_Poly.addPoly'(_6598916,_6598918,_6598920,_6598922,_6598924)).
'blocked_Poly.addPoly'(_6599012,_6599030,_6600096,_6600102,_6600108):-hnf(_6599012,_6600914,_6600102,_6600932),'blocked_Poly.addPoly_1'(_6600914,_6599030,_6600096,_6600932,_6600108).

'blocked_Poly.addPoly_1'(_6601254,_6601256,_6601258,_6601260,_6601262):-freeze(_6601260,'blocked_blocked_Poly.addPoly_1'(_6601254,_6601256,_6601258,_6601260,_6601262)).
'blocked_blocked_Poly.addPoly_1'('Poly.Poly'(_6599146),_6599030,_6601834,_6601840,_6601846):-!,hnf(_6599030,_6603312,_6601840,_6603330),'blocked_blocked_Poly.addPoly_1_Poly.Poly_2'(_6603312,_6599146,_6601834,_6603330,_6601846).

'blocked_blocked_Poly.addPoly_1_Poly.Poly_2'(_6603790,_6603792,_6603794,_6603796,_6603798):-freeze(_6603796,'blocked_blocked_blocked_Poly.addPoly_1_Poly.Poly_2'(_6603790,_6603792,_6603794,_6603796,_6603798)).
'blocked_blocked_blocked_Poly.addPoly_1_Poly.Poly_2'('Poly.Poly'(_6599268),_6599146,_6604152,_6604158,_6604164):-!,hnf('Prelude.apply'('Poly.mkPoly','Poly.zipWithDefault'('Prelude.apply'('Poly.ratFromInt',0),partcall(2,'Rational.ratAdd',[]),_6599146,_6599268)),_6604152,_6604158,_6604164).
'blocked_blocked_blocked_Poly.addPoly_1_Poly.Poly_2'('FAIL'(_6606090),_6599146,'FAIL'(_6606090),_6606104,_6606104):-nonvar(_6606090).
'blocked_blocked_Poly.addPoly_1'('FAIL'(_6606168),_6599030,'FAIL'(_6606168),_6606182,_6606182):-nonvar(_6606168).

'Poly.subPoly'(_6606830,_6606832,_6606834,_6606836,_6606838):-freeze(_6606836,'blocked_Poly.subPoly'(_6606830,_6606832,_6606834,_6606836,_6606838)).
'blocked_Poly.subPoly'(_6606926,_6606944,_6608010,_6608016,_6608022):-hnf(_6606926,_6608828,_6608016,_6608846),'blocked_Poly.subPoly_1'(_6608828,_6606944,_6608010,_6608846,_6608022).

'blocked_Poly.subPoly_1'(_6609168,_6609170,_6609172,_6609174,_6609176):-freeze(_6609174,'blocked_blocked_Poly.subPoly_1'(_6609168,_6609170,_6609172,_6609174,_6609176)).
'blocked_blocked_Poly.subPoly_1'('Poly.Poly'(_6607060),_6606944,_6609748,_6609754,_6609760):-!,hnf(_6606944,_6611226,_6609754,_6611244),'blocked_blocked_Poly.subPoly_1_Poly.Poly_2'(_6611226,_6607060,_6609748,_6611244,_6609760).

'blocked_blocked_Poly.subPoly_1_Poly.Poly_2'(_6611704,_6611706,_6611708,_6611710,_6611712):-freeze(_6611710,'blocked_blocked_blocked_Poly.subPoly_1_Poly.Poly_2'(_6611704,_6611706,_6611708,_6611710,_6611712)).
'blocked_blocked_blocked_Poly.subPoly_1_Poly.Poly_2'('Poly.Poly'(_6607182),_6607060,_6612066,_6612072,_6612078):-!,hnf('Prelude.apply'('Poly.mkPoly','Poly.zipWithDefault'('Prelude.apply'('Poly.ratFromInt',0),partcall(2,'Rational.ratSub',[]),_6607060,_6607182)),_6612066,_6612072,_6612078).
'blocked_blocked_blocked_Poly.subPoly_1_Poly.Poly_2'('FAIL'(_6614004),_6607060,'FAIL'(_6614004),_6614018,_6614018):-nonvar(_6614004).
'blocked_blocked_Poly.subPoly_1'('FAIL'(_6614082),_6606944,'FAIL'(_6614082),_6614096,_6614096):-nonvar(_6614082).

'Poly.zipWithDefault'(_6615010,_6615012,_6615014,_6615016,_6615018,_6615020,_6615022):-freeze(_6615020,'blocked_Poly.zipWithDefault'(_6615010,_6615012,_6615014,_6615016,_6615018,_6615020,_6615022)).
'blocked_Poly.zipWithDefault'(_6615126,_6615144,_6615162,_6615180,_6622096,_6622102,_6622108):-hnf('Prelude.(,)'(_6615162,_6615180),_6623296,_6622102,_6623250),'blocked_Poly.zipWithDefault_ComplexCase'(_6623296,_6615126,_6615144,_6615162,_6615180,_6622096,_6623250,_6622108).

'blocked_Poly.zipWithDefault_ComplexCase'(_6623704,_6623706,_6623708,_6623710,_6623712,_6623714,_6623716,_6623718):-freeze(_6623716,freeze(_6623704,'blocked_blocked_Poly.zipWithDefault_ComplexCase'(_6623704,_6623706,_6623708,_6623710,_6623712,_6623714,_6623716,_6623718))).
'blocked_blocked_Poly.zipWithDefault_ComplexCase'('Prelude.(,)'(_6615450,_6615468),_6615126,_6615144,_6615162,_6615180,_6624432,_6624438,_6624444):-!,hnf(_6615450,_6626606,_6624438,_6626648),'blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1'(_6626606,_6615468,_6615126,_6615144,_6615162,_6615180,_6624432,_6626648,_6624444).

'blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1'(_6627260,_6627262,_6627264,_6627266,_6627268,_6627270,_6627272,_6627274,_6627276):-freeze(_6627274,freeze(_6627260,'blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1'(_6627260,_6627262,_6627264,_6627266,_6627268,_6627270,_6627272,_6627274,_6627276))).
'blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1'([],_6615468,_6615126,_6615144,_6615162,_6615180,_6628002,_6628008,_6628014):-hnf(_6615468,_6630706,_6628008,_6630742),'blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[]_1'(_6630706,_6615126,_6615144,_6615162,_6615180,_6628002,_6630742,_6628014).

'blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[]_1'(_6631406,_6631408,_6631410,_6631412,_6631414,_6631416,_6631418,_6631420):-freeze(_6631418,freeze(_6631406,'blocked_blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[]_1'(_6631406,_6631408,_6631410,_6631412,_6631414,_6631416,_6631418,_6631420))).
'blocked_blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[]_1'([],_6615126,_6615144,_6615162,_6615180,[],_6631700,_6631700).
'blocked_blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[]_1'([_6615794|_6615812],_6615126,_6615144,_6615162,_6615180,['Prelude.apply'('Prelude.apply'(_6633054,_6633074),_6615794)|'Poly.zipWithDefault'(_6633074,_6633054,[],_6615812)],_6632794,_6632800):-!,makeShare(_6615144,_6633054),makeShare(_6615126,_6633074),_6632794=_6632800.
'blocked_blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[]_1'('FAIL'(_6635678),_6615126,_6615144,_6615162,_6615180,'FAIL'(_6635678),_6635692,_6635692).
'blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1'([_6616704|_6616722],_6615468,_6615126,_6615144,_6615162,_6615180,_6636426,_6636432,_6636438):-!,hnf(_6615468,_6639160,_6636432,_6639208),'blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[|]_3'(_6639160,_6616704,_6616722,_6615126,_6615144,_6615162,_6615180,_6636426,_6639208,_6636438).

'blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[|]_3'(_6639912,_6639914,_6639916,_6639918,_6639920,_6639922,_6639924,_6639926,_6639928,_6639930):-freeze(_6639928,freeze(_6639912,'blocked_blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[|]_3'(_6639912,_6639914,_6639916,_6639918,_6639920,_6639922,_6639924,_6639926,_6639928,_6639930))).
'blocked_blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[|]_3'([],_6616704,_6616722,_6615126,_6615144,_6615162,_6615180,['Prelude.apply'('Prelude.apply'(_6640514,_6616704),_6640534)|'Poly.zipWithDefault'(_6640534,_6640514,_6616722,[])],_6640226,_6640232):-makeShare(_6615144,_6640514),makeShare(_6615126,_6640534),_6640226=_6640232.
'blocked_blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[|]_3'([_6617706|_6617724],_6616704,_6616722,_6615126,_6615144,_6615162,_6615180,['Prelude.apply'('Prelude.apply'(_6643706,_6616704),_6617706)|'Poly.zipWithDefault'(_6615126,_6643706,_6616722,_6617724)],_6643322,_6643328):-!,makeShare(_6615144,_6643706),_6643322=_6643328.
'blocked_blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1_[|]_3'('FAIL'(_6646206),_6616704,_6616722,_6615126,_6615144,_6615162,_6615180,'FAIL'(_6646206),_6646220,_6646220).
'blocked_blocked_blocked_Poly.zipWithDefault_ComplexCase_Prelude.(,)_1'('FAIL'(_6646320),_6615468,_6615126,_6615144,_6615162,_6615180,'FAIL'(_6646320),_6646334,_6646334).
'blocked_blocked_Poly.zipWithDefault_ComplexCase'('FAIL'(_6646426),_6615126,_6615144,_6615162,_6615180,'FAIL'(_6646426),_6646440,_6646440).

'Poly.mulPoly'(_6647108,_6647110,_6647112,_6647114,_6647116):-freeze(_6647114,'blocked_Poly.mulPoly'(_6647108,_6647110,_6647112,_6647114,_6647116)).
'blocked_Poly.mulPoly'(_6647204,_6647222,_6649732,_6649738,_6649744):-hnf(_6647204,_6650550,_6649738,_6650568),'blocked_Poly.mulPoly_1'(_6650550,_6647222,_6649732,_6650568,_6649744).

'blocked_Poly.mulPoly_1'(_6650890,_6650892,_6650894,_6650896,_6650898):-freeze(_6650896,'blocked_blocked_Poly.mulPoly_1'(_6650890,_6650892,_6650894,_6650896,_6650898)).
'blocked_blocked_Poly.mulPoly_1'('Poly.Poly'(_6647338),_6647222,_6651470,_6651476,_6651482):-!,hnf(_6647222,_6652948,_6651476,_6652966),'blocked_blocked_Poly.mulPoly_1_Poly.Poly_2'(_6652948,_6647338,_6651470,_6652966,_6651482).

'blocked_blocked_Poly.mulPoly_1_Poly.Poly_2'(_6653426,_6653428,_6653430,_6653432,_6653434):-freeze(_6653432,'blocked_blocked_blocked_Poly.mulPoly_1_Poly.Poly_2'(_6653426,_6653428,_6653430,_6653432,_6653434)).
'blocked_blocked_blocked_Poly.mulPoly_1_Poly.Poly_2'('Poly.Poly'(_6647460),_6647338,_6653788,_6653794,_6653800):-!,makeShare(_6647494,_6654206),makeShare(_6647338,_6654226),makeShare(_6647460,_6654246),makeShare(_6647512,_6654266),hnf('Prelude.cond'(letrec4PAKCS(_6654206,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'('Prelude.length'(_6654226),'Prelude.length'(_6654246)),1)),'Prelude.cond'(letrec4PAKCS(_6654266,'Prelude.foldr'(partcall(2,'Poly.mulPoly._\'23lambda10',[_6654246]),[],'Prelude.zip'('Prelude._impl\'23enumFrom\'23Prelude.Enum\'23Prelude.Int\'23'(0),_6654226))),'Poly.mulPoly._\'23caseor0'('Prelude.null'(_6654226),_6654246,_6654206,_6654266))),_6653788,_6653794,_6653800).
'blocked_blocked_blocked_Poly.mulPoly_1_Poly.Poly_2'('FAIL'(_6660210),_6647338,'FAIL'(_6660210),_6660224,_6660224):-nonvar(_6660210).
'blocked_blocked_Poly.mulPoly_1'('FAIL'(_6660288),_6647222,'FAIL'(_6660288),_6660302,_6660302):-nonvar(_6660288).

'Poly.mulPoly._\'23lambda10'(_6661396,_6661398,_6661400,_6661402,_6661404,_6661406):-freeze(_6661404,'blocked_Poly.mulPoly._\'23lambda10'(_6661396,_6661398,_6661400,_6661402,_6661404,_6661406)).
'blocked_Poly.mulPoly._\'23lambda10'(_6661502,_6661520,_6661538,_6662726,_6662732,_6662738):-hnf(_6661520,_6664020,_6662732,_6664044),'blocked_Poly.mulPoly._\'23lambda10_2'(_6664020,_6661502,_6661538,_6662726,_6664044,_6662738).

'blocked_Poly.mulPoly._\'23lambda10_2'(_6664458,_6664460,_6664462,_6664464,_6664466,_6664468):-freeze(_6664466,freeze(_6664458,'blocked_blocked_Poly.mulPoly._\'23lambda10_2'(_6664458,_6664460,_6664462,_6664464,_6664466,_6664468))).
'blocked_blocked_Poly.mulPoly._\'23lambda10_2'('Prelude.(,)'(_6661654,_6661672),_6661502,_6661538,_6664854,_6664860,_6664866):-!,hnf('Prelude.++'('Prelude.foldr'(partcall(2,'Poly.mulPoly._\'23lambda10._\'23lambda14',[_6661672,_6661654]),[],'Prelude.zip'('Prelude._impl\'23enumFrom\'23Prelude.Enum\'23Prelude.Int\'23'(0),_6661502)),_6661538),_6664854,_6664860,_6664866).
'blocked_blocked_Poly.mulPoly._\'23lambda10_2'('FAIL'(_6667460),_6661502,_6661538,'FAIL'(_6667460),_6667474,_6667474).

'Poly.mulPoly._\'23lambda10._\'23lambda14'(_6669018,_6669020,_6669022,_6669024,_6669026,_6669028,_6669030):-freeze(_6669028,'blocked_Poly.mulPoly._\'23lambda10._\'23lambda14'(_6669018,_6669020,_6669022,_6669024,_6669026,_6669028,_6669030)).
'blocked_Poly.mulPoly._\'23lambda10._\'23lambda14'(_6669134,_6669152,_6669170,_6669188,_6670294,_6670300,_6670306):-hnf(_6669170,_6672064,_6670300,_6672094),'blocked_Poly.mulPoly._\'23lambda10._\'23lambda14_3'(_6672064,_6669134,_6669152,_6669188,_6670294,_6672094,_6670306).

'blocked_Poly.mulPoly._\'23lambda10._\'23lambda14_3'(_6672594,_6672596,_6672598,_6672600,_6672602,_6672604,_6672606):-freeze(_6672604,freeze(_6672594,'blocked_blocked_Poly.mulPoly._\'23lambda10._\'23lambda14_3'(_6672594,_6672596,_6672598,_6672600,_6672602,_6672604,_6672606))).
'blocked_blocked_Poly.mulPoly._\'23lambda10._\'23lambda14_3'('Prelude.(,)'(_6669304,_6669322),_6669134,_6669152,_6669188,['Prelude.(,)'('Prelude._impl\'23\'2B\'23Prelude.Num\'23Prelude.Int\'23'(_6669134,_6669304),'Rational.ratMul'(_6669152,_6669322))|_6669188],_6673006,_6673006):-!.
'blocked_blocked_Poly.mulPoly._\'23lambda10._\'23lambda14_3'('FAIL'(_6675180),_6669134,_6669152,_6669188,'FAIL'(_6675180),_6675194,_6675194).

'Poly.mulPoly.addCoeffs.64'(_6676348,_6676350,_6676352,_6676354,_6676356):-freeze(_6676354,'blocked_Poly.mulPoly.addCoeffs.64'(_6676348,_6676350,_6676352,_6676354,_6676356)).
'blocked_Poly.mulPoly.addCoeffs.64'(_6676444,_6676462,_6678488,_6678494,_6678500):-hnf(_6676462,_6679774,_6678494,_6679792),'blocked_Poly.mulPoly.addCoeffs.64_2'(_6679774,_6676444,_6678488,_6679792,_6678500).

'blocked_Poly.mulPoly.addCoeffs.64_2'(_6680192,_6680194,_6680196,_6680198,_6680200):-freeze(_6680198,'blocked_blocked_Poly.mulPoly.addCoeffs.64_2'(_6680192,_6680194,_6680196,_6680198,_6680200)).
'blocked_blocked_Poly.mulPoly.addCoeffs.64_2'('Prelude.(,)'(_6676578,_6676596),_6676444,_6680572,_6680578,_6680584):-!,makeShare(_6676636,_6681098),makeShare(_6676444,_6681118),makeShare(_6676654,_6681138),makeShare(_6676672,_6681158),hnf('Prelude.cond'(letrec4PAKCS(_6681098,'Prelude.splitAt'(_6676578,_6681118)),'Prelude.cond'(letrec4PAKCS(_6681138,'Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre'(_6681098)),'Prelude.cond'(letrec4PAKCS(_6681158,'Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest'(_6681098)),'Poly.mulPoly.addCoeffs.64._\'23caseor0'(_6681158,_6681138,_6676596,_6681118)))),_6680572,_6680578,_6680584).
'blocked_blocked_Poly.mulPoly.addCoeffs.64_2'('FAIL'(_6685918),_6676444,'FAIL'(_6685918),_6685932,_6685932):-nonvar(_6685918).

'Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre'(_6687624,_6687626,_6687628,_6687630):-freeze(_6687628,'blocked_Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre'(_6687624,_6687626,_6687628,_6687630)).
'blocked_Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre'(_6687710,_6688208,_6688214,_6688220):-hnf(_6687710,_6690098,_6688214,_6690110),'blocked_Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre_1'(_6690098,_6688208,_6690110,_6688220).

'blocked_Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre_1'(_6690604,_6690606,_6690608,_6690610):-freeze(_6690608,'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre_1'(_6690604,_6690606,_6690608,_6690610)).
'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre_1'('Prelude.(,)'(_6687826,_6687844),_6690974,_6690980,_6690986):-!,hnf(_6687826,_6690974,_6690980,_6690986).
'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23selFP2\'23pre_1'('FAIL'(_6691606),'FAIL'(_6691606),_6691620,_6691620):-nonvar(_6691606).

'Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest'(_6693342,_6693344,_6693346,_6693348):-freeze(_6693346,'blocked_Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest'(_6693342,_6693344,_6693346,_6693348)).
'blocked_Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest'(_6693428,_6693932,_6693938,_6693944):-hnf(_6693428,_6695858,_6693938,_6695870),'blocked_Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest_1'(_6695858,_6693932,_6695870,_6693944).

'blocked_Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest_1'(_6696370,_6696372,_6696374,_6696376):-freeze(_6696374,'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest_1'(_6696370,_6696372,_6696374,_6696376)).
'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest_1'('Prelude.(,)'(_6693544,_6693562),_6696740,_6696746,_6696752):-!,hnf(_6693562,_6696740,_6696746,_6696752).
'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23selFP3\'23rest_1'('FAIL'(_6697378),'FAIL'(_6697378),_6697392,_6697392):-nonvar(_6697378).

'Poly.divModPoly'(_6698146,_6698148,_6698150,_6698152,_6698154):-freeze(_6698152,'blocked_Poly.divModPoly'(_6698146,_6698148,_6698150,_6698152,_6698154)).
'blocked_Poly.divModPoly'(_6698242,_6698260,_6700440,_6700446,_6700452):-hnf(_6698260,_6701366,_6700446,_6701384),'blocked_Poly.divModPoly_2'(_6701366,_6698242,_6700440,_6701384,_6700452).

'blocked_Poly.divModPoly_2'(_6701724,_6701726,_6701728,_6701730,_6701732):-freeze(_6701730,'blocked_blocked_Poly.divModPoly_2'(_6701724,_6701726,_6701728,_6701730,_6701732)).
'blocked_blocked_Poly.divModPoly_2'('Poly.Poly'(_6698376),_6698242,_6702086,_6702092,_6702098):-!,makeShare(_6698410,_6702546),makeShare(_6698376,_6702566),makeShare(_6698428,_6702586),makeShare(_6698446,_6702606),hnf('Prelude.cond'(letrec4PAKCS(_6702546,'Poly.Poly'(_6702566)),'Prelude.cond'(letrec4PAKCS(_6702586,'Poly.degree'(_6702546)),'Prelude.cond'(letrec4PAKCS(_6702606,'Poly.divModPoly._\'23caseor0'('Poly.leadCoeff'(_6702546))),'Poly.divModPoly._\'23caseor0._\'23caseor0'('Prelude.null'(_6702566),_6702566,_6702546,_6702586,_6702606,_6698242)))),_6702086,_6702092,_6702098).
'blocked_blocked_Poly.divModPoly_2'('FAIL'(_6707378),_6698242,'FAIL'(_6707378),_6707392,_6707392):-nonvar(_6707378).

'Poly.divModPoly.go.79'(_6708382,_6708384,_6708386,_6708388,_6708390,_6708392,_6708394,_6708396):-freeze(_6708394,'blocked_Poly.divModPoly.go.79'(_6708382,_6708384,_6708386,_6708388,_6708390,_6708392,_6708394,_6708396)).
'blocked_Poly.divModPoly.go.79'(_6708508,_6708526,_6708544,_6708562,_6708580,_6719412,_6719418,_6719424):-makeShare(_6708580,_6715938),makeShare(_6708526,_6715958),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23','Poly.degree'(_6715938)),_6715958),_6720698,_6719418,_6720646),'blocked_Poly.divModPoly.go.79_ComplexCase'(_6720698,_6708508,_6715958,_6708544,_6708562,_6715938,_6719412,_6720646,_6719424).

'blocked_Poly.divModPoly.go.79_ComplexCase'(_6721144,_6721146,_6721148,_6721150,_6721152,_6721154,_6721156,_6721158,_6721160):-freeze(_6721158,freeze(_6721144,'blocked_blocked_Poly.divModPoly.go.79_ComplexCase'(_6721144,_6721146,_6721148,_6721150,_6721152,_6721154,_6721156,_6721158,_6721160))).
'blocked_blocked_Poly.divModPoly.go.79_ComplexCase'('Prelude.True',_6708508,_6715958,_6708544,_6708562,_6715938,'Prelude.(,)'(_6708562,_6715938),_6721584,_6721584).
'blocked_blocked_Poly.divModPoly.go.79_ComplexCase'('Prelude.False',_6708508,_6715958,_6708544,_6708562,_6715938,_6726672,_6726678,_6726684):-!,hnf('Prelude.otherwise',_6729106,_6726678,_6729054),'blocked_blocked_Poly.divModPoly.go.79_ComplexCase_Prelude.False_ComplexCase'(_6729106,_6708508,_6715958,_6708544,_6708562,_6715938,_6726672,_6729054,_6726684).

'blocked_blocked_Poly.divModPoly.go.79_ComplexCase_Prelude.False_ComplexCase'(_6729750,_6729752,_6729754,_6729756,_6729758,_6729760,_6729762,_6729764,_6729766):-freeze(_6729764,freeze(_6729750,'blocked_blocked_blocked_Poly.divModPoly.go.79_ComplexCase_Prelude.False_ComplexCase'(_6729750,_6729752,_6729754,_6729756,_6729758,_6729760,_6729762,_6729764,_6729766))).
'blocked_blocked_blocked_Poly.divModPoly.go.79_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6708508,_6715958,_6708544,_6708562,_6715938,_6730184,_6730190,_6730196):-makeShare(_6709464,_6731956),makeShare(_6715938,_6731976),makeShare(_6709482,_6731996),makeShare(_6709500,_6732016),makeShare(_6708544,_6732036),makeShare(_6709518,_6732056),makeShare(_6715958,_6732076),makeShare(_6709536,_6732096),makeShare(_6709554,_6732116),makeShare(_6708508,_6732136),hnf('Prelude.cond'(letrec4PAKCS(_6731956,'Poly.degree'(_6731976)),'Prelude.cond'(letrec4PAKCS(_6731996,'Poly.divModPoly.go.79._\'23caseor0'('Poly.leadCoeff'(_6731976))),'Prelude.cond'(letrec4PAKCS(_6732016,'Rational.ratDiv'(_6731996,_6732036)),'Prelude.cond'(letrec4PAKCS(_6732056,'Prelude._impl\'23\'2D\'23Prelude.Num\'23Prelude.Int\'23'(_6731956,_6732076)),'Prelude.cond'(letrec4PAKCS(_6732096,'Poly.Poly'('Prelude.++'('Prelude.replicate'(_6732056,'Prelude.apply'('Poly.ratFromInt',0)),[_6732016]))),'Prelude.cond'(letrec4PAKCS(_6732116,'Poly.subPoly'(_6731976,'Poly.mulPoly'(_6732096,_6732136))),'Poly.divModPoly.go.79'(_6732136,_6732076,_6732036,'Poly.addPoly'(_6708562,_6732096),_6732116))))))),_6730184,_6730190,_6730196).
'blocked_blocked_blocked_Poly.divModPoly.go.79_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6708508,_6715958,_6708544,_6708562,_6715938,_6743164,_6743170,_6743176):-!,hnf(reportFailure4PAKCS('Poly.divModPoly.go.79',['Prelude.False']),_6743164,_6743170,_6743176).
'blocked_blocked_blocked_Poly.divModPoly.go.79_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6744936),_6708508,_6715958,_6708544,_6708562,_6715938,'FAIL'(_6744936),_6744950,_6744950).
'blocked_blocked_Poly.divModPoly.go.79_ComplexCase'('FAIL'(_6745042),_6708508,_6715958,_6708544,_6708562,_6715938,'FAIL'(_6745042),_6745056,_6745056).

'Poly.gcdPoly'(_6745732,_6745734,_6745736,_6745738,_6745740):-freeze(_6745738,'blocked_Poly.gcdPoly'(_6745732,_6745734,_6745736,_6745738,_6745740)).
'blocked_Poly.gcdPoly'(_6745828,_6745846,_6747030,_6747036,_6747042):-hnf(_6745846,_6747848,_6747036,_6747866),'blocked_Poly.gcdPoly_2'(_6747848,_6745828,_6747030,_6747866,_6747042).

'blocked_Poly.gcdPoly_2'(_6748188,_6748190,_6748192,_6748194,_6748196):-freeze(_6748194,'blocked_blocked_Poly.gcdPoly_2'(_6748188,_6748190,_6748192,_6748194,_6748196)).
'blocked_blocked_Poly.gcdPoly_2'('Poly.Poly'(_6745962),_6745828,_6749392,_6749398,_6749404):-!,makeShare(_6745962,_6748552),hnf(_6748552,_6750870,_6749398,_6750894),'blocked_blocked_Poly.gcdPoly_2_Poly.Poly_1'(_6750870,_6750870,_6745828,_6749392,_6750894,_6749404).

'blocked_blocked_Poly.gcdPoly_2_Poly.Poly_1'(_6751380,_6751382,_6751384,_6751386,_6751388,_6751390):-freeze(_6751388,freeze(_6751380,'blocked_blocked_blocked_Poly.gcdPoly_2_Poly.Poly_1'(_6751380,_6751382,_6751384,_6751386,_6751388,_6751390))).
'blocked_blocked_blocked_Poly.gcdPoly_2_Poly.Poly_1'([],_6748552,_6745828,_6751648,_6751654,_6751660):-hnf('Poly.monicPoly'(_6745828),_6751648,_6751654,_6751660).
'blocked_blocked_blocked_Poly.gcdPoly_2_Poly.Poly_1'([_6746254|_6746272],_6748552,_6745828,_6752602,_6752608,_6752614):-!,makeShare(_6748552,_6752738),hnf('Poly.gcdPoly'('Poly.Poly'(_6752738),'Prelude.snd'('Poly.divModPoly'(_6745828,'Poly.Poly'(_6752738)))),_6752602,_6752608,_6752614).
'blocked_blocked_blocked_Poly.gcdPoly_2_Poly.Poly_1'('FAIL'(_6754642),_6748552,_6745828,'FAIL'(_6754642),_6754656,_6754656).
'blocked_blocked_Poly.gcdPoly_2'('FAIL'(_6754724),_6745828,'FAIL'(_6754724),_6754738,_6754738):-nonvar(_6754724).

'Poly.monicPoly'(_6755462,_6755464,_6755466,_6755468):-freeze(_6755466,'blocked_Poly.monicPoly'(_6755462,_6755464,_6755466,_6755468)).
'blocked_Poly.monicPoly'(_6755548,_6757540,_6757546,_6757552):-makeShare(_6755548,_6756580),hnf(_6756580,_6758422,_6757546,_6758440),'blocked_Poly.monicPoly_1'(_6758422,_6758422,_6757540,_6758440,_6757552).

'blocked_Poly.monicPoly_1'(_6758786,_6758788,_6758790,_6758792,_6758794):-freeze(_6758792,'blocked_blocked_Poly.monicPoly_1'(_6758786,_6758788,_6758790,_6758792,_6758794)).
'blocked_blocked_Poly.monicPoly_1'('Poly.Poly'(_6755664),_6756580,_6760142,_6760148,_6760154):-!,makeShare(_6755664,_6759150),hnf(_6759150,_6761692,_6760148,_6761716),'blocked_blocked_Poly.monicPoly_1_Poly.Poly_1'(_6761692,_6761692,_6756580,_6760142,_6761716,_6760154).

'blocked_blocked_Poly.monicPoly_1_Poly.Poly_1'(_6762214,_6762216,_6762218,_6762220,_6762222,_6762224):-freeze(_6762222,freeze(_6762214,'blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1'(_6762214,_6762216,_6762218,_6762220,_6762222,_6762224))).
'blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1'([],_6759150,_6756580,_6762482,_6762488,_6762494):-hnf('Poly.zeroPoly',_6762482,_6762488,_6762494).
'blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1'([_6755886|_6755904],_6759150,_6756580,_6764480,_6764486,_6764492):-!,hnf('Poly.leadCoeff'(_6756580),_6766692,_6764486,_6766646),'blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1_[|]_ComplexCase'(_6766692,_6755886,_6755904,_6759150,_6756580,_6764480,_6766646,_6764492).

'blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1_[|]_ComplexCase'(_6767292,_6767294,_6767296,_6767298,_6767300,_6767302,_6767304,_6767306):-freeze(_6767304,freeze(_6767292,'blocked_blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1_[|]_ComplexCase'(_6767292,_6767294,_6767296,_6767298,_6767300,_6767302,_6767304,_6767306))).
'blocked_blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1_[|]_ComplexCase'('Prelude.Nothing',_6755886,_6755904,_6759150,_6756580,_6767752,_6767758,_6767764):-hnf('Poly.zeroPoly',_6767752,_6767758,_6767764).
'blocked_blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1_[|]_ComplexCase'('Prelude.Just'(_6756216),_6755886,_6755904,_6759150,_6756580,'Poly.Poly'('Prelude.map'(partcall(1,'Poly.monicPoly._\'23lambda22',[_6756216]),_6759150)),_6769034,_6769034):-!.
'blocked_blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1_[|]_ComplexCase'('FAIL'(_6770684),_6755886,_6755904,_6759150,_6756580,'FAIL'(_6770684),_6770698,_6770698).
'blocked_blocked_blocked_Poly.monicPoly_1_Poly.Poly_1'('FAIL'(_6770782),_6759150,_6756580,'FAIL'(_6770782),_6770796,_6770796).
'blocked_blocked_Poly.monicPoly_1'('FAIL'(_6770864),_6756580,'FAIL'(_6770864),_6770878,_6770878):-nonvar(_6770864).

'Poly.monicPoly._\'23lambda22'(_6772048,_6772050,_6772052,_6772054,_6772056):-freeze(_6772054,'blocked_Poly.monicPoly._\'23lambda22'(_6772048,_6772050,_6772052,_6772054,_6772056)).
'blocked_Poly.monicPoly._\'23lambda22'(_6772144,_6772162,_6772332,_6772338,_6772344):-hnf('Rational.ratDiv'(_6772162,_6772144),_6772332,_6772338,_6772344).

'Poly.diffPoly'(_6773730,_6773732,_6773734,_6773736):-freeze(_6773734,'blocked_Poly.diffPoly'(_6773730,_6773732,_6773734,_6773736)).
'blocked_Poly.diffPoly'(_6773816,_6774858,_6774864,_6774870):-hnf(_6773816,_6775704,_6774864,_6775716),'blocked_Poly.diffPoly_1'(_6775704,_6774858,_6775716,_6774870).

'blocked_Poly.diffPoly_1'(_6776036,_6776038,_6776040,_6776042):-freeze(_6776040,'blocked_blocked_Poly.diffPoly_1'(_6776036,_6776038,_6776040,_6776042)).
'blocked_blocked_Poly.diffPoly_1'('Poly.Poly'(_6773932),_6776604,_6776610,_6776616):-!,hnf(_6773932,_6778110,_6776610,_6778122),'blocked_blocked_Poly.diffPoly_1_Poly.Poly_1'(_6778110,_6776604,_6778122,_6776616).

'blocked_blocked_Poly.diffPoly_1_Poly.Poly_1'(_6778586,_6778588,_6778590,_6778592):-freeze(_6778590,freeze(_6778586,'blocked_blocked_blocked_Poly.diffPoly_1_Poly.Poly_1'(_6778586,_6778588,_6778590,_6778592))).
'blocked_blocked_blocked_Poly.diffPoly_1_Poly.Poly_1'([],_6778834,_6778840,_6778846):-hnf('Poly.zeroPoly',_6778834,_6778840,_6778846).
'blocked_blocked_blocked_Poly.diffPoly_1_Poly.Poly_1'([_6774154|_6774172],_6779546,_6779552,_6779558):-!,hnf('Prelude.apply'('Poly.mkPoly','Prelude.zipWith'(partcall(2,'Poly.diffPoly._\'23lambda24',[]),'Prelude._impl\'23enumFrom\'23Prelude.Enum\'23Prelude.Int\'23'(1),_6774172)),_6779546,_6779552,_6779558).
'blocked_blocked_blocked_Poly.diffPoly_1_Poly.Poly_1'('FAIL'(_6781484),'FAIL'(_6781484),_6781498,_6781498).
'blocked_blocked_Poly.diffPoly_1'('FAIL'(_6781550),'FAIL'(_6781550),_6781564,_6781564):-nonvar(_6781550).

'Poly.diffPoly._\'23lambda24'(_6782688,_6782690,_6782692,_6782694,_6782696):-freeze(_6782694,'blocked_Poly.diffPoly._\'23lambda24'(_6782688,_6782690,_6782692,_6782694,_6782696)).
'blocked_Poly.diffPoly._\'23lambda24'(_6782784,_6782802,_6783140,_6783146,_6783152):-hnf('Rational.ratMul'('Prelude.apply'('Poly.ratFromInt',_6782784),_6782802),_6783140,_6783146,_6783152).

'Poly.composePoly'(_6785032,_6785034,_6785036,_6785038,_6785040):-freeze(_6785038,'blocked_Poly.composePoly'(_6785032,_6785034,_6785036,_6785038,_6785040)).
'blocked_Poly.composePoly'(_6785128,_6785146,_6786038,_6786044,_6786050):-hnf(_6785128,_6787000,_6786044,_6787018),'blocked_Poly.composePoly_1'(_6787000,_6785146,_6786038,_6787018,_6786050).

'blocked_Poly.composePoly_1'(_6787364,_6787366,_6787368,_6787370,_6787372):-freeze(_6787370,'blocked_blocked_Poly.composePoly_1'(_6787364,_6787366,_6787368,_6787370,_6787372)).
'blocked_blocked_Poly.composePoly_1'('Poly.Poly'(_6785262),_6785146,_6788544,_6788550,_6788556):-!,makeShare(_6785262,_6787728),hnf(_6787728,_6790166,_6788550,_6790190),'blocked_blocked_Poly.composePoly_1_Poly.Poly_1'(_6790166,_6790166,_6785146,_6788544,_6790190,_6788556).

'blocked_blocked_Poly.composePoly_1_Poly.Poly_1'(_6790700,_6790702,_6790704,_6790706,_6790708,_6790710):-freeze(_6790708,freeze(_6790700,'blocked_blocked_blocked_Poly.composePoly_1_Poly.Poly_1'(_6790700,_6790702,_6790704,_6790706,_6790708,_6790710))).
'blocked_blocked_blocked_Poly.composePoly_1_Poly.Poly_1'([],_6787728,_6785146,_6790968,_6790974,_6790980):-hnf('Poly.zeroPoly',_6790968,_6790974,_6790980).
'blocked_blocked_blocked_Poly.composePoly_1_Poly.Poly_1'([_6785484|_6785502],_6787728,_6785146,_6791846,_6791852,_6791858):-!,hnf('Prelude.foldr'(partcall(2,'Poly.composePoly._\'23lambda26',[_6785146]),'Poly.zeroPoly',_6787728),_6791846,_6791852,_6791858).
'blocked_blocked_blocked_Poly.composePoly_1_Poly.Poly_1'('FAIL'(_6793358),_6787728,_6785146,'FAIL'(_6793358),_6793372,_6793372).
'blocked_blocked_Poly.composePoly_1'('FAIL'(_6793440),_6785146,'FAIL'(_6793440),_6793454,_6793454):-nonvar(_6793440).

'Poly.composePoly._\'23lambda26'(_6794700,_6794702,_6794704,_6794706,_6794708,_6794710):-freeze(_6794708,'blocked_Poly.composePoly._\'23lambda26'(_6794700,_6794702,_6794704,_6794706,_6794708,_6794710)).
'blocked_Poly.composePoly._\'23lambda26'(_6794806,_6794824,_6794842,_6795250,_6795256,_6795262):-hnf('Poly.addPoly'('Poly.constPoly'(_6794824),'Poly.mulPoly'(_6794806,_6794842)),_6795250,_6795256,_6795262).

'Poly.squareFree'(_6797284,_6797286,_6797288,_6797290):-freeze(_6797288,'blocked_Poly.squareFree'(_6797284,_6797286,_6797288,_6797290)).
'blocked_Poly.squareFree'(_6797370,_6800140,_6800146,_6800152):-hnf(_6797370,_6801058,_6800146,_6801070),'blocked_Poly.squareFree_1'(_6801058,_6800140,_6801070,_6800152).

'blocked_Poly.squareFree_1'(_6801402,_6801404,_6801406,_6801408):-freeze(_6801406,'blocked_blocked_Poly.squareFree_1'(_6801402,_6801404,_6801406,_6801408)).
'blocked_blocked_Poly.squareFree_1'('Poly.Poly'(_6797486),_6803058,_6803064,_6803070):-!,makeShare(_6797486,_6801756),hnf(_6801756,_6804636,_6803064,_6804654),'blocked_blocked_Poly.squareFree_1_Poly.Poly_1'(_6804636,_6804636,_6803058,_6804654,_6803070).

'blocked_blocked_Poly.squareFree_1_Poly.Poly_1'(_6805150,_6805152,_6805154,_6805156,_6805158):-freeze(_6805156,freeze(_6805150,'blocked_blocked_blocked_Poly.squareFree_1_Poly.Poly_1'(_6805150,_6805152,_6805154,_6805156,_6805158))).
'blocked_blocked_blocked_Poly.squareFree_1_Poly.Poly_1'([],_6801756,[],_6805414,_6805414).
'blocked_blocked_blocked_Poly.squareFree_1_Poly.Poly_1'([_6797708|_6797726],_6801756,_6806106,_6806112,_6806118):-!,makeShare(_6797766,_6806634),makeShare(_6797784,_6806654),makeShare(_6797802,_6806674),makeShare(_6797820,_6806694),hnf('Prelude.cond'(letrec4PAKCS(_6806634,'Poly.Poly'(_6801756)),'Prelude.cond'(letrec4PAKCS(_6806654,'Poly.diffPoly'(_6806634)),'Prelude.cond'(letrec4PAKCS(_6806674,'Poly.gcdPoly'(_6806634,_6806654)),'Prelude.cond'(letrec4PAKCS(_6806694,'Prelude.fst'('Poly.divModPoly'(_6806634,_6806674))),'Poly.squareFree.go.141'(partcall(1,'Prelude._inst\'23Prelude.Num\'23Prelude.Int\'23',[]),_6806694,_6806674,1))))),_6806106,_6806112,_6806118).
'blocked_blocked_blocked_Poly.squareFree_1_Poly.Poly_1'('FAIL'(_6812358),_6801756,'FAIL'(_6812358),_6812372,_6812372).
'blocked_blocked_Poly.squareFree_1'('FAIL'(_6812432),'FAIL'(_6812432),_6812446,_6812446):-nonvar(_6812432).

'Poly.squareFree.go.141'(_6813466,_6813468,_6813470,_6813472,_6813474,_6813476,_6813478):-freeze(_6813476,'blocked_Poly.squareFree.go.141'(_6813466,_6813468,_6813470,_6813472,_6813474,_6813476,_6813478)).
'blocked_Poly.squareFree.go.141'(_6813582,_6813600,_6813618,_6813636,_6823428,_6823434,_6823440):-makeShare(_6813600,_6820538),hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'('Poly.degree'(_6820538),0),_6824736,_6823434,_6824690),'blocked_Poly.squareFree.go.141_ComplexCase'(_6824736,_6813582,_6820538,_6813618,_6813636,_6823428,_6824690,_6823440).

'blocked_Poly.squareFree.go.141_ComplexCase'(_6825174,_6825176,_6825178,_6825180,_6825182,_6825184,_6825186,_6825188):-freeze(_6825186,freeze(_6825174,'blocked_blocked_Poly.squareFree.go.141_ComplexCase'(_6825174,_6825176,_6825178,_6825180,_6825182,_6825184,_6825186,_6825188))).
'blocked_blocked_Poly.squareFree.go.141_ComplexCase'('Prelude.True',_6813582,_6820538,_6813618,_6813636,_6827770,_6827776,_6827782):-makeShare(_6813618,_6825698),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23','Poly.degree'(_6825698)),0),_6830196,_6827776,_6830150),'blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.True_ComplexCase'(_6830196,_6813582,_6820538,_6825698,_6813636,_6827770,_6830150,_6827782).

'blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.True_ComplexCase'(_6830832,_6830834,_6830836,_6830838,_6830840,_6830842,_6830844,_6830846):-freeze(_6830844,freeze(_6830832,'blocked_blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.True_ComplexCase'(_6830832,_6830834,_6830836,_6830838,_6830840,_6830842,_6830844,_6830846))).
'blocked_blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.True_ComplexCase'('Prelude.True',_6813582,_6820538,_6825698,_6813636,['Prelude.(,)'(_6825698,_6813636)],_6831262,_6831262).
'blocked_blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.True_ComplexCase'('Prelude.False',_6813582,_6820538,_6825698,_6813636,[],_6832950,_6832950):-!.
'blocked_blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.True_ComplexCase'('FAIL'(_6833866),_6813582,_6820538,_6825698,_6813636,'FAIL'(_6833866),_6833880,_6833880).
'blocked_blocked_Poly.squareFree.go.141_ComplexCase'('Prelude.False',_6813582,_6820538,_6813618,_6813636,_6837192,_6837198,_6837204):-!,hnf('Prelude.otherwise',_6839648,_6837198,_6839602),'blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.False_ComplexCase'(_6839648,_6813582,_6820538,_6813618,_6813636,_6837192,_6839602,_6837204).

'blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.False_ComplexCase'(_6840296,_6840298,_6840300,_6840302,_6840304,_6840306,_6840308,_6840310):-freeze(_6840308,freeze(_6840296,'blocked_blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.False_ComplexCase'(_6840296,_6840298,_6840300,_6840302,_6840304,_6840306,_6840308,_6840310))).
'blocked_blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6813582,_6820538,_6813618,_6813636,_6840720,_6840726,_6840732):-makeShare(_6815130,_6841920),makeShare(_6820538,_6841940),makeShare(_6813618,_6841960),makeShare(_6815148,_6841980),makeShare(_6815166,_6842000),makeShare(_6815184,_6842020),makeShare(_6813582,_6842040),makeShare(_6813636,_6842060),hnf('Prelude.cond'(letrec4PAKCS(_6841920,'Poly.gcdPoly'(_6841940,_6841960)),'Prelude.cond'(letrec4PAKCS(_6841980,'Prelude.fst'('Poly.divModPoly'(_6841940,_6841920))),'Prelude.cond'(letrec4PAKCS(_6842000,'Prelude.fst'('Poly.divModPoly'(_6841960,_6841920))),'Prelude.cond'(letrec4PAKCS(_6842020,'Poly.squareFree.go.141'(_6842040,_6841920,_6842000,'Prelude.apply'('Prelude.apply'('Prelude.+'(_6842040),_6842060),'Prelude.apply'('Prelude.fromInt'(_6842040),1)))),'Poly.squareFree.go.141._\'23caseor0'('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23','Poly.degree'(_6841980)),0),_6841980,_6842060,_6842020))))),_6840720,_6840726,_6840732).
'blocked_blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6813582,_6820538,_6813618,_6813636,_6851720,_6851726,_6851732):-!,hnf(reportFailure4PAKCS('Poly.squareFree.go.141',['Prelude.False']),_6851720,_6851726,_6851732).
'blocked_blocked_blocked_Poly.squareFree.go.141_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6853430),_6813582,_6820538,_6813618,_6813636,'FAIL'(_6853430),_6853444,_6853444).
'blocked_blocked_Poly.squareFree.go.141_ComplexCase'('FAIL'(_6853528),_6813582,_6820538,_6813618,_6813636,'FAIL'(_6853528),_6853542,_6853542).

'Poly.showPoly'(_6854248,_6854250,_6854252,_6854254):-freeze(_6854252,'blocked_Poly.showPoly'(_6854248,_6854250,_6854252,_6854254)).
'blocked_Poly.showPoly'(_6854334,_6856230,_6856236,_6856242):-hnf(_6854334,_6857076,_6856236,_6857088),'blocked_Poly.showPoly_1'(_6857076,_6856230,_6857088,_6856242).

'blocked_Poly.showPoly_1'(_6857408,_6857410,_6857412,_6857414):-freeze(_6857412,'blocked_blocked_Poly.showPoly_1'(_6857408,_6857410,_6857412,_6857414)).
'blocked_blocked_Poly.showPoly_1'('Poly.Poly'(_6854450),_6858656,_6858662,_6858668):-!,makeShare(_6854450,_6857762),hnf(_6857762,_6860162,_6858662,_6860180),'blocked_blocked_Poly.showPoly_1_Poly.Poly_1'(_6860162,_6860162,_6858656,_6860180,_6858668).

'blocked_blocked_Poly.showPoly_1_Poly.Poly_1'(_6860664,_6860666,_6860668,_6860670,_6860672):-freeze(_6860670,freeze(_6860664,'blocked_blocked_blocked_Poly.showPoly_1_Poly.Poly_1'(_6860664,_6860666,_6860668,_6860670,_6860672))).
'blocked_blocked_blocked_Poly.showPoly_1_Poly.Poly_1'([],_6857762,['^0'],_6860928,_6860928).
'blocked_blocked_blocked_Poly.showPoly_1_Poly.Poly_1'([_6854826|_6854844],_6857762,_6861810,_6861816,_6861822):-!,hnf('Prelude.++'(['^P','^o','^l','^y','^ '],'Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23\'5B\'5D\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Show\'23Rational.Rational\'23',[])),_6857762)),_6861810,_6861816,_6861822).
'blocked_blocked_blocked_Poly.showPoly_1_Poly.Poly_1'('FAIL'(_6864864),_6857762,'FAIL'(_6864864),_6864878,_6864878).
'blocked_blocked_Poly.showPoly_1'('FAIL'(_6864938),'FAIL'(_6864938),_6864952,_6864952):-nonvar(_6864938).

'Poly.mulPoly._\'23caseor0'(_6866000,_6866002,_6866004,_6866006,_6866008,_6866010,_6866012):-freeze(_6866010,'blocked_Poly.mulPoly._\'23caseor0'(_6866000,_6866002,_6866004,_6866006,_6866008,_6866010,_6866012)).
'blocked_Poly.mulPoly._\'23caseor0'(_6866116,_6866134,_6866152,_6866170,_6868182,_6868188,_6868194):-hnf(_6866116,_6869448,_6868188,_6869478),'blocked_Poly.mulPoly._\'23caseor0_1'(_6869448,_6866134,_6866152,_6866170,_6868182,_6869478,_6868194).

'blocked_Poly.mulPoly._\'23caseor0_1'(_6869894,_6869896,_6869898,_6869900,_6869902,_6869904,_6869906):-freeze(_6869904,freeze(_6869894,'blocked_blocked_Poly.mulPoly._\'23caseor0_1'(_6869894,_6869896,_6869898,_6869900,_6869902,_6869904,_6869906))).
'blocked_blocked_Poly.mulPoly._\'23caseor0_1'('Prelude.True',_6866134,_6866152,_6866170,_6870308,_6870314,_6870320):-hnf('Poly.zeroPoly',_6870308,_6870314,_6870320).
'blocked_blocked_Poly.mulPoly._\'23caseor0_1'('Prelude.False',_6866134,_6866152,_6866170,_6872626,_6872632,_6872638):-!,hnf('Prelude.null'(_6866134),_6874780,_6872632,_6874740),'blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase'(_6874780,_6866134,_6866152,_6866170,_6872626,_6874740,_6872638).

'blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase'(_6875378,_6875380,_6875382,_6875384,_6875386,_6875388,_6875390):-freeze(_6875388,freeze(_6875378,'blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase'(_6875378,_6875380,_6875382,_6875384,_6875386,_6875388,_6875390))).
'blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_6866134,_6866152,_6866170,_6875792,_6875798,_6875804):-hnf('Poly.zeroPoly',_6875792,_6875798,_6875804).
'blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_6866134,_6866152,_6866170,_6878240,_6878246,_6878252):-!,hnf('Prelude.otherwise',_6881618,_6878246,_6881578),'blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6881618,_6866134,_6866152,_6866170,_6878240,_6881578,_6878252).

'blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6882420,_6882422,_6882424,_6882426,_6882428,_6882430,_6882432):-freeze(_6882430,freeze(_6882420,'blocked_blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6882420,_6882422,_6882424,_6882426,_6882428,_6882430,_6882432))).
'blocked_blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6866134,_6866152,_6866170,_6882834,_6882840,_6882846):-hnf('Prelude.apply'('Poly.mkPoly','Prelude.foldl'(partcall(2,'Poly.mulPoly.addCoeffs.64',[]),'Prelude.replicate'(_6866152,'Prelude.apply'('Poly.ratFromInt',0)),_6866170)),_6882834,_6882840,_6882846).
'blocked_blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6866134,_6866152,_6866170,_6885742,_6885748,_6885754):-!,hnf(reportFailure4PAKCS('Poly.mulPoly._\'23caseor0',['Prelude.False']),_6885742,_6885748,_6885754).
'blocked_blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6887546),_6866134,_6866152,_6866170,'FAIL'(_6887546),_6887560,_6887560).
'blocked_blocked_blocked_Poly.mulPoly._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_6887636),_6866134,_6866152,_6866170,'FAIL'(_6887636),_6887650,_6887650).
'blocked_blocked_Poly.mulPoly._\'23caseor0_1'('FAIL'(_6887726),_6866134,_6866152,_6866170,'FAIL'(_6887726),_6887740,_6887740).

'Poly.mulPoly.addCoeffs.64._\'23caseor0'(_6889302,_6889304,_6889306,_6889308,_6889310,_6889312,_6889314):-freeze(_6889312,'blocked_Poly.mulPoly.addCoeffs.64._\'23caseor0'(_6889302,_6889304,_6889306,_6889308,_6889310,_6889312,_6889314)).
'blocked_Poly.mulPoly.addCoeffs.64._\'23caseor0'(_6889418,_6889436,_6889454,_6889472,_6890488,_6890494,_6890500):-hnf(_6889418,_6892222,_6890494,_6892252),'blocked_Poly.mulPoly.addCoeffs.64._\'23caseor0_1'(_6892222,_6889436,_6889454,_6889472,_6890488,_6892252,_6890500).

'blocked_Poly.mulPoly.addCoeffs.64._\'23caseor0_1'(_6892746,_6892748,_6892750,_6892752,_6892754,_6892756,_6892758):-freeze(_6892756,freeze(_6892746,'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23caseor0_1'(_6892746,_6892748,_6892750,_6892752,_6892754,_6892756,_6892758))).
'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23caseor0_1'([_6889588|_6889606],_6889436,_6889454,_6889472,_6893052,_6893058,_6893064):-hnf('Prelude.++'(_6889436,['Rational.ratAdd'(_6889588,_6889454)|_6889606]),_6893052,_6893058,_6893064).
'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23caseor0_1'([],_6889436,_6889454,_6889472,_6894912,_6894918,_6894924):-!,hnf(_6889472,_6894912,_6894918,_6894924).
'blocked_blocked_Poly.mulPoly.addCoeffs.64._\'23caseor0_1'('FAIL'(_6895604),_6889436,_6889454,_6889472,'FAIL'(_6895604),_6895618,_6895618).

'Poly.divModPoly._\'23caseor0'(_6896800,_6896802,_6896804,_6896806):-freeze(_6896804,'blocked_Poly.divModPoly._\'23caseor0'(_6896800,_6896802,_6896804,_6896806)).
'blocked_Poly.divModPoly._\'23caseor0'(_6896886,_6898994,_6899000,_6899006):-hnf(_6896886,_6900344,_6899000,_6900356),'blocked_Poly.divModPoly._\'23caseor0_1'(_6900344,_6898994,_6900356,_6899006).

'blocked_Poly.divModPoly._\'23caseor0_1'(_6900766,_6900768,_6900770,_6900772):-freeze(_6900770,freeze(_6900766,'blocked_blocked_Poly.divModPoly._\'23caseor0_1'(_6900766,_6900768,_6900770,_6900772))).
'blocked_blocked_Poly.divModPoly._\'23caseor0_1'('Prelude.Just'(_6897002),_6901160,_6901166,_6901172):-hnf(_6897002,_6901160,_6901166,_6901172).
'blocked_blocked_Poly.divModPoly._\'23caseor0_1'('Prelude.Nothing',_6901952,_6901958,_6901964):-!,hnf('Prelude.error'(['^i','^m','^p','^o','^s','^s','^i','^b','^l','^e']),_6901952,_6901958,_6901964).
'blocked_blocked_Poly.divModPoly._\'23caseor0_1'('FAIL'(_6904582),'FAIL'(_6904582),_6904596,_6904596).

'Poly.divModPoly._\'23caseor0._\'23caseor0'(_6906162,_6906164,_6906166,_6906168,_6906170,_6906172,_6906174,_6906176,_6906178):-freeze(_6906176,'blocked_Poly.divModPoly._\'23caseor0._\'23caseor0'(_6906162,_6906164,_6906166,_6906168,_6906170,_6906172,_6906174,_6906176,_6906178)).
'blocked_Poly.divModPoly._\'23caseor0._\'23caseor0'(_6906298,_6906316,_6906334,_6906352,_6906370,_6906388,_6914874,_6914880,_6914886):-hnf(_6906298,_6916696,_6914880,_6916738),'blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1'(_6916696,_6906316,_6906334,_6906352,_6906370,_6906388,_6914874,_6916738,_6914886).

'blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1'(_6917260,_6917262,_6917264,_6917266,_6917268,_6917270,_6917272,_6917274,_6917276):-freeze(_6917274,freeze(_6917260,'blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1'(_6917260,_6917262,_6917264,_6917266,_6917268,_6917270,_6917272,_6917274,_6917276))).
'blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1'('Prelude.True',_6906316,_6906334,_6906352,_6906370,_6906388,_6917694,_6917700,_6917706):-hnf('Prelude.error'(['^d','^i','^v','^M','^o','^d','^P','^o','^l','^y',^:,'^ ','^d','^i','^v','^i','^s','^i','^o','^n','^ ','^b','^y','^ ','^z','^e','^r','^o','^ ','^p','^o','^l','^y','^n','^o','^m','^i','^a','^l']),_6917694,_6917700,_6917706).
'blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1'('Prelude.False',_6906316,_6906334,_6906352,_6906370,_6906388,_6929982,_6929988,_6929994):-!,makeShare(_6906388,_6927202),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3C\'23Prelude.Ord\'23Prelude.Int\'23','Poly.degree'(_6927202)),'Poly.degree'('Poly.Poly'(_6906316))),_6932704,_6929988,_6932652),'blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_6932704,_6906316,_6906334,_6906352,_6906370,_6927202,_6929982,_6932652,_6929994).

'blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_6933408,_6933410,_6933412,_6933414,_6933416,_6933418,_6933420,_6933422,_6933424):-freeze(_6933422,freeze(_6933408,'blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'(_6933408,_6933410,_6933412,_6933414,_6933416,_6933418,_6933420,_6933422,_6933424))).
'blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.True',_6906316,_6906334,_6906352,_6906370,_6927202,'Prelude.(,)'('Poly.zeroPoly',_6927202),_6933848,_6933848).
'blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('Prelude.False',_6906316,_6906334,_6906352,_6906370,_6927202,_6937002,_6937008,_6937014):-!,hnf('Prelude.otherwise',_6940948,_6937008,_6940896),'blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6940948,_6906316,_6906334,_6906352,_6906370,_6927202,_6937002,_6940896,_6937014).

'blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6941844,_6941846,_6941848,_6941850,_6941852,_6941854,_6941856,_6941858,_6941860):-freeze(_6941858,freeze(_6941844,'blocked_blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'(_6941844,_6941846,_6941848,_6941850,_6941852,_6941854,_6941856,_6941858,_6941860))).
'blocked_blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6906316,_6906334,_6906352,_6906370,_6927202,_6942278,_6942284,_6942290):-hnf('Poly.divModPoly.go.79'(_6906334,_6906352,_6906370,'Poly.zeroPoly',_6927202),_6942278,_6942284,_6942290).
'blocked_blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6906316,_6906334,_6906352,_6906370,_6927202,_6944600,_6944606,_6944612):-!,hnf(reportFailure4PAKCS('Poly.divModPoly._\'23caseor0._\'23caseor0',['Prelude.False']),_6944600,_6944606,_6944612).
'blocked_blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6946732),_6906316,_6906334,_6906352,_6906370,_6927202,'FAIL'(_6946732),_6946746,_6946746).
'blocked_blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1_Prelude.False_ComplexCase'('FAIL'(_6946838),_6906316,_6906334,_6906352,_6906370,_6927202,'FAIL'(_6946838),_6946852,_6946852).
'blocked_blocked_Poly.divModPoly._\'23caseor0._\'23caseor0_1'('FAIL'(_6946944),_6906316,_6906334,_6906352,_6906370,_6906388,'FAIL'(_6946944),_6946958,_6946958).

'Poly.divModPoly.go.79._\'23caseor0'(_6948384,_6948386,_6948388,_6948390):-freeze(_6948388,'blocked_Poly.divModPoly.go.79._\'23caseor0'(_6948384,_6948386,_6948388,_6948390)).
'blocked_Poly.divModPoly.go.79._\'23caseor0'(_6948470,_6950614,_6950620,_6950626):-hnf(_6948470,_6952180,_6950620,_6952192),'blocked_Poly.divModPoly.go.79._\'23caseor0_1'(_6952180,_6950614,_6952192,_6950626).

'blocked_Poly.divModPoly.go.79._\'23caseor0_1'(_6952638,_6952640,_6952642,_6952644):-freeze(_6952642,freeze(_6952638,'blocked_blocked_Poly.divModPoly.go.79._\'23caseor0_1'(_6952638,_6952640,_6952642,_6952644))).
'blocked_blocked_Poly.divModPoly.go.79._\'23caseor0_1'('Prelude.Just'(_6948586),_6953032,_6953038,_6953044):-hnf(_6948586,_6953032,_6953038,_6953044).
'blocked_blocked_Poly.divModPoly.go.79._\'23caseor0_1'('Prelude.Nothing',_6953860,_6953866,_6953872):-!,hnf('Prelude.error'(['^i','^m','^p','^o','^s','^s','^i','^b','^l','^e']),_6953860,_6953866,_6953872).
'blocked_blocked_Poly.divModPoly.go.79._\'23caseor0_1'('FAIL'(_6956526),'FAIL'(_6956526),_6956540,_6956540).

'Poly.squareFree.go.141._\'23caseor0'(_6957964,_6957966,_6957968,_6957970,_6957972,_6957974,_6957976):-freeze(_6957974,'blocked_Poly.squareFree.go.141._\'23caseor0'(_6957964,_6957966,_6957968,_6957970,_6957972,_6957974,_6957976)).
'blocked_Poly.squareFree.go.141._\'23caseor0'(_6958080,_6958098,_6958116,_6958134,_6958930,_6958936,_6958942):-hnf(_6958080,_6960556,_6958936,_6960586),'blocked_Poly.squareFree.go.141._\'23caseor0_1'(_6960556,_6958098,_6958116,_6958134,_6958930,_6960586,_6958942).

'blocked_Poly.squareFree.go.141._\'23caseor0_1'(_6961062,_6961064,_6961066,_6961068,_6961070,_6961072,_6961074):-freeze(_6961072,freeze(_6961062,'blocked_blocked_Poly.squareFree.go.141._\'23caseor0_1'(_6961062,_6961064,_6961066,_6961068,_6961070,_6961072,_6961074))).
'blocked_blocked_Poly.squareFree.go.141._\'23caseor0_1'('Prelude.True',_6958098,_6958116,_6958134,['Prelude.(,)'(_6958098,_6958116)|_6958134],_6961482,_6961482).
'blocked_blocked_Poly.squareFree.go.141._\'23caseor0_1'('Prelude.False',_6958098,_6958116,_6958134,_6962916,_6962922,_6962928):-!,hnf(_6958134,_6962916,_6962922,_6962928).
'blocked_blocked_Poly.squareFree.go.141._\'23caseor0_1'('FAIL'(_6963590),_6958098,_6958116,_6958134,'FAIL'(_6963590),_6963604,_6963604).

:-costCenters(['']).




%%%%% Number of shared variables: 47

