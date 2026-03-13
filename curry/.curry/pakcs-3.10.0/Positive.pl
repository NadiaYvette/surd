%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').

:-curryModule('Positive').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23','_inst#Prelude.Eq#Positive.Positive#',1,'Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23','_impl#==#Prelude.Eq#Positive.Positive#',2,'Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23\'2F\'3D\'23Prelude.Eq\'23Positive.Positive\'23','_impl#/=#Prelude.Eq#Positive.Positive#',0,'Positive._impl\'23\'2F\'3D\'23Prelude.Eq\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23','_inst#Prelude.Ord#Positive.Positive#',1,'Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23','_impl#compare#Prelude.Ord#Positive.Positive#',2,'Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23\'3C\'23Prelude.Ord\'23Positive.Positive\'23','_impl#<#Prelude.Ord#Positive.Positive#',0,'Positive._impl\'23\'3C\'23Prelude.Ord\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23\'3E\'23Prelude.Ord\'23Positive.Positive\'23','_impl#>#Prelude.Ord#Positive.Positive#',0,'Positive._impl\'23\'3E\'23Prelude.Ord\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23','_impl#<=#Prelude.Ord#Positive.Positive#',2,'Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23\'3E\'3D\'23Prelude.Ord\'23Positive.Positive\'23','_impl#>=#Prelude.Ord#Positive.Positive#',0,'Positive._impl\'23\'3E\'3D\'23Prelude.Ord\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23min\'23Prelude.Ord\'23Positive.Positive\'23','_impl#min#Prelude.Ord#Positive.Positive#',0,'Positive._impl\'23min\'23Prelude.Ord\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23max\'23Prelude.Ord\'23Positive.Positive\'23','_impl#max#Prelude.Ord#Positive.Positive#',0,'Positive._impl\'23max\'23Prelude.Ord\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._inst\'23Prelude.Show\'23Positive.Positive\'23','_inst#Prelude.Show#Positive.Positive#',1,'Positive._inst\'23Prelude.Show\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23show\'23Prelude.Show\'23Positive.Positive\'23','_impl#show#Prelude.Show#Positive.Positive#',0,'Positive._impl\'23show\'23Prelude.Show\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23showsPrec\'23Prelude.Show\'23Positive.Positive\'23','_impl#showsPrec#Prelude.Show#Positive.Positive#',0,'Positive._impl\'23showsPrec\'23Prelude.Show\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23showList\'23Prelude.Show\'23Positive.Positive\'23','_impl#showList#Prelude.Show#Positive.Positive#',0,'Positive._impl\'23showList\'23Prelude.Show\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._inst\'23Prelude.Data\'23Positive.Positive\'23','_inst#Prelude.Data#Positive.Positive#',1,'Positive._inst\'23Prelude.Data\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23','_impl#===#Prelude.Data#Positive.Positive#',2,'Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive._impl\'23aValue\'23Prelude.Data\'23Positive.Positive\'23','_impl#aValue#Prelude.Data#Positive.Positive#',0,'Positive._impl\'23aValue\'23Prelude.Data\'23Positive.Positive\'23',nofix,notype).
functiontype('Positive.unPositive',unPositive,1,'Positive.unPositive',nofix,notype).
functiontype('Positive.positive',positive,1,'Positive.positive',nofix,notype).
functiontype('Positive.unsafePositive',unsafePositive,1,'Positive.unsafePositive',nofix,notype).
functiontype('Positive.showPositive',showPositive,1,'Positive.showPositive',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.
constructortype('Positive.Pos','Positive.Pos',1,'Pos',0,notype,[]).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23'(_6161642,_6161644,_6161646,_6161648):-freeze(_6161646,'blocked_Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23'(_6161642,_6161644,_6161646,_6161648)).
'blocked_Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23'(_6161728,_6162408,_6162414,_6162420):-hnf(_6161728,_6164586,_6162414,_6164598),'blocked_Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23_1'(_6164586,_6162408,_6164598,_6162420).

'blocked_Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23_1'(_6165140,_6165142,_6165144,_6165146):-freeze(_6165144,'blocked_blocked_Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23_1'(_6165140,_6165142,_6165144,_6165146)).
'blocked_blocked_Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23_1'('Prelude.()','Prelude._Dict\'23Eq'(partcall(2,'Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23',[]),'Positive._impl\'23\'2F\'3D\'23Prelude.Eq\'23Positive.Positive\'23'),_6165492,_6165492):-!.
'blocked_blocked_Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23_1'('FAIL'(_6167076),'FAIL'(_6167076),_6167090,_6167090):-nonvar(_6167076).

'Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23'(_6169260,_6169262,_6169264,_6169266,_6169268):-freeze(_6169266,'blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23'(_6169260,_6169262,_6169264,_6169266,_6169268)).
'blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23'(_6169356,_6169374,_6170232,_6170238,_6170244):-hnf(_6169356,_6172742,_6170238,_6172760),'blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1'(_6172742,_6169374,_6170232,_6172760,_6170244).

'blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1'(_6173364,_6173366,_6173368,_6173370,_6173372):-freeze(_6173370,'blocked_blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1'(_6173364,_6173366,_6173368,_6173370,_6173372)).
'blocked_blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1'('Positive.Pos'(_6169490),_6169374,_6174262,_6174268,_6174274):-!,hnf(_6169374,_6177522,_6174268,_6177540),'blocked_blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1_Positive.Pos_2'(_6177522,_6169490,_6174262,_6177540,_6174274).

'blocked_blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1_Positive.Pos_2'(_6178300,_6178302,_6178304,_6178306,_6178308):-freeze(_6178306,'blocked_blocked_blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1_Positive.Pos_2'(_6178300,_6178302,_6178304,_6178306,_6178308)).
'blocked_blocked_blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1_Positive.Pos_2'('Positive.Pos'(_6169612),_6169490,_6178698,_6178704,_6178710):-!,hnf('Prelude._impl\'23\'3D\'3D\'23Prelude.Eq\'23Prelude.Int\'23'(_6169490,_6169612),_6178698,_6178704,_6178710).
'blocked_blocked_blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1_Positive.Pos_2'('FAIL'(_6180108),_6169490,'FAIL'(_6180108),_6180122,_6180122):-nonvar(_6180108).
'blocked_blocked_Positive._impl\'23\'3D\'3D\'23Prelude.Eq\'23Positive.Positive\'23_1'('FAIL'(_6180186),_6169374,'FAIL'(_6180186),_6180200,_6180200):-nonvar(_6180186).

'Positive._impl\'23\'2F\'3D\'23Prelude.Eq\'23Positive.Positive\'23'(_6182378,_6182380,_6182382):-freeze(_6182380,'blocked_Positive._impl\'23\'2F\'3D\'23Prelude.Eq\'23Positive.Positive\'23'(_6182378,_6182380,_6182382)).
'blocked_Positive._impl\'23\'2F\'3D\'23Prelude.Eq\'23Positive.Positive\'23'(_6182550,_6182556,_6182562):-hnf(partcall(2,'Prelude._def\'23\'2F\'3D\'23Prelude.Eq',[partcall(1,'Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23',[])]),_6182550,_6182556,_6182562).

'Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23'(_6185668,_6185670,_6185672,_6185674):-freeze(_6185672,'blocked_Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23'(_6185668,_6185670,_6185672,_6185674)).
'blocked_Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23'(_6185754,_6186944,_6186950,_6186956):-hnf(_6185754,_6189158,_6186950,_6189170),'blocked_Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6189158,_6186944,_6189170,_6186956).

'blocked_Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6189718,_6189720,_6189722,_6189724):-freeze(_6189722,'blocked_blocked_Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6189718,_6189720,_6189722,_6189724)).
'blocked_blocked_Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23_1'('Prelude.()','Prelude._Dict\'23Ord'(partcall(1,'Positive._inst\'23Prelude.Eq\'23Positive.Positive\'23',[]),partcall(2,'Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23',[]),'Positive._impl\'23\'3C\'23Prelude.Ord\'23Positive.Positive\'23','Positive._impl\'23\'3E\'23Prelude.Ord\'23Positive.Positive\'23',partcall(2,'Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23',[]),'Positive._impl\'23\'3E\'3D\'23Prelude.Ord\'23Positive.Positive\'23','Positive._impl\'23min\'23Prelude.Ord\'23Positive.Positive\'23','Positive._impl\'23max\'23Prelude.Ord\'23Positive.Positive\'23'),_6190070,_6190070):-!.
'blocked_blocked_Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23_1'('FAIL'(_6194392),'FAIL'(_6194392),_6194406,_6194406):-nonvar(_6194392).

'Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23'(_6196748,_6196750,_6196752,_6196754,_6196756):-freeze(_6196754,'blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23'(_6196748,_6196750,_6196752,_6196754,_6196756)).
'blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23'(_6196844,_6196862,_6197900,_6197906,_6197912):-hnf(_6196844,_6200482,_6197906,_6200500),'blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6200482,_6196862,_6197900,_6200500,_6197912).

'blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6201116,_6201118,_6201120,_6201122,_6201124):-freeze(_6201122,'blocked_blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6201116,_6201118,_6201120,_6201122,_6201124)).
'blocked_blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1'('Positive.Pos'(_6196978),_6196862,_6202026,_6202032,_6202038):-!,hnf(_6196862,_6205358,_6202032,_6205376),'blocked_blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'(_6205358,_6196978,_6202026,_6205376,_6202038).

'blocked_blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'(_6206148,_6206150,_6206152,_6206154,_6206156):-freeze(_6206154,'blocked_blocked_blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'(_6206148,_6206150,_6206152,_6206154,_6206156)).
'blocked_blocked_blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'('Positive.Pos'(_6197100),_6196978,_6206546,_6206552,_6206558):-!,hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23compare\'23Prelude.Ord\'23Prelude.Int\'23',_6196978),_6197100),_6206546,_6206552,_6206558).
'blocked_blocked_blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'('FAIL'(_6208354),_6196978,'FAIL'(_6208354),_6208368,_6208368):-nonvar(_6208354).
'blocked_blocked_Positive._impl\'23compare\'23Prelude.Ord\'23Positive.Positive\'23_1'('FAIL'(_6208432),_6196862,'FAIL'(_6208432),_6208446,_6208446):-nonvar(_6208432).

'Positive._impl\'23\'3C\'23Prelude.Ord\'23Positive.Positive\'23'(_6210596,_6210598,_6210600):-freeze(_6210598,'blocked_Positive._impl\'23\'3C\'23Prelude.Ord\'23Positive.Positive\'23'(_6210596,_6210598,_6210600)).
'blocked_Positive._impl\'23\'3C\'23Prelude.Ord\'23Positive.Positive\'23'(_6210768,_6210774,_6210780):-hnf(partcall(2,'Prelude._def\'23\'3C\'23Prelude.Ord',[partcall(1,'Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23',[])]),_6210768,_6210774,_6210780).

'Positive._impl\'23\'3E\'23Prelude.Ord\'23Positive.Positive\'23'(_6214000,_6214002,_6214004):-freeze(_6214002,'blocked_Positive._impl\'23\'3E\'23Prelude.Ord\'23Positive.Positive\'23'(_6214000,_6214002,_6214004)).
'blocked_Positive._impl\'23\'3E\'23Prelude.Ord\'23Positive.Positive\'23'(_6214172,_6214178,_6214184):-hnf(partcall(2,'Prelude._def\'23\'3E\'23Prelude.Ord',[partcall(1,'Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23',[])]),_6214172,_6214178,_6214184).

'Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23'(_6217470,_6217472,_6217474,_6217476,_6217478):-freeze(_6217476,'blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23'(_6217470,_6217472,_6217474,_6217476,_6217478)).
'blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23'(_6217566,_6217584,_6218448,_6218454,_6218460):-hnf(_6217566,_6220994,_6218454,_6221012),'blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6220994,_6217584,_6218448,_6221012,_6218460).

'blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6221622,_6221624,_6221626,_6221628,_6221630):-freeze(_6221628,'blocked_blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1'(_6221622,_6221624,_6221626,_6221628,_6221630)).
'blocked_blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1'('Positive.Pos'(_6217700),_6217584,_6222526,_6222532,_6222538):-!,hnf(_6217584,_6225822,_6222532,_6225840),'blocked_blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'(_6225822,_6217700,_6222526,_6225840,_6222538).

'blocked_blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'(_6226606,_6226608,_6226610,_6226612,_6226614):-freeze(_6226612,'blocked_blocked_blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'(_6226606,_6226608,_6226610,_6226612,_6226614)).
'blocked_blocked_blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'('Positive.Pos'(_6217822),_6217700,_6227004,_6227010,_6227016):-!,hnf('Prelude._impl\'23\'3C\'3D\'23Prelude.Ord\'23Prelude.Int\'23'(_6217700,_6217822),_6227004,_6227010,_6227016).
'blocked_blocked_blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1_Positive.Pos_2'('FAIL'(_6228426),_6217700,'FAIL'(_6228426),_6228440,_6228440):-nonvar(_6228426).
'blocked_blocked_Positive._impl\'23\'3C\'3D\'23Prelude.Ord\'23Positive.Positive\'23_1'('FAIL'(_6228504),_6217584,'FAIL'(_6228504),_6228518,_6228518):-nonvar(_6228504).

'Positive._impl\'23\'3E\'3D\'23Prelude.Ord\'23Positive.Positive\'23'(_6230734,_6230736,_6230738):-freeze(_6230736,'blocked_Positive._impl\'23\'3E\'3D\'23Prelude.Ord\'23Positive.Positive\'23'(_6230734,_6230736,_6230738)).
'blocked_Positive._impl\'23\'3E\'3D\'23Prelude.Ord\'23Positive.Positive\'23'(_6230906,_6230912,_6230918):-hnf(partcall(2,'Prelude._def\'23\'3E\'3D\'23Prelude.Ord',[partcall(1,'Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23',[])]),_6230906,_6230912,_6230918).

'Positive._impl\'23min\'23Prelude.Ord\'23Positive.Positive\'23'(_6234222,_6234224,_6234226):-freeze(_6234224,'blocked_Positive._impl\'23min\'23Prelude.Ord\'23Positive.Positive\'23'(_6234222,_6234224,_6234226)).
'blocked_Positive._impl\'23min\'23Prelude.Ord\'23Positive.Positive\'23'(_6234394,_6234400,_6234406):-hnf(partcall(2,'Prelude._def\'23min\'23Prelude.Ord',[partcall(1,'Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23',[])]),_6234394,_6234400,_6234406).

'Positive._impl\'23max\'23Prelude.Ord\'23Positive.Positive\'23'(_6237674,_6237676,_6237678):-freeze(_6237676,'blocked_Positive._impl\'23max\'23Prelude.Ord\'23Positive.Positive\'23'(_6237674,_6237676,_6237678)).
'blocked_Positive._impl\'23max\'23Prelude.Ord\'23Positive.Positive\'23'(_6237846,_6237852,_6237858):-hnf(partcall(2,'Prelude._def\'23max\'23Prelude.Ord',[partcall(1,'Positive._inst\'23Prelude.Ord\'23Positive.Positive\'23',[])]),_6237846,_6237852,_6237858).

'Positive._inst\'23Prelude.Show\'23Positive.Positive\'23'(_6240984,_6240986,_6240988,_6240990):-freeze(_6240988,'blocked_Positive._inst\'23Prelude.Show\'23Positive.Positive\'23'(_6240984,_6240986,_6240988,_6240990)).
'blocked_Positive._inst\'23Prelude.Show\'23Positive.Positive\'23'(_6241070,_6241846,_6241852,_6241858):-hnf(_6241070,_6244096,_6241852,_6244108),'blocked_Positive._inst\'23Prelude.Show\'23Positive.Positive\'23_1'(_6244096,_6241846,_6244108,_6241858).

'blocked_Positive._inst\'23Prelude.Show\'23Positive.Positive\'23_1'(_6244662,_6244664,_6244666,_6244668):-freeze(_6244666,'blocked_blocked_Positive._inst\'23Prelude.Show\'23Positive.Positive\'23_1'(_6244662,_6244664,_6244666,_6244668)).
'blocked_blocked_Positive._inst\'23Prelude.Show\'23Positive.Positive\'23_1'('Prelude.()','Prelude._Dict\'23Show'('Positive._impl\'23show\'23Prelude.Show\'23Positive.Positive\'23','Positive._impl\'23showsPrec\'23Prelude.Show\'23Positive.Positive\'23','Positive._impl\'23showList\'23Prelude.Show\'23Positive.Positive\'23'),_6245014,_6245014):-!.
'blocked_blocked_Positive._inst\'23Prelude.Show\'23Positive.Positive\'23_1'('FAIL'(_6247138),'FAIL'(_6247138),_6247152,_6247152):-nonvar(_6247138).

'Positive._impl\'23show\'23Prelude.Show\'23Positive.Positive\'23'(_6249418,_6249420,_6249422):-freeze(_6249420,'blocked_Positive._impl\'23show\'23Prelude.Show\'23Positive.Positive\'23'(_6249418,_6249420,_6249422)).
'blocked_Positive._impl\'23show\'23Prelude.Show\'23Positive.Positive\'23'(_6249506,_6249512,_6249518):-hnf(partcall(1,'Positive.showPositive',[]),_6249506,_6249512,_6249518).

'Positive._impl\'23showsPrec\'23Prelude.Show\'23Positive.Positive\'23'(_6252568,_6252570,_6252572):-freeze(_6252570,'blocked_Positive._impl\'23showsPrec\'23Prelude.Show\'23Positive.Positive\'23'(_6252568,_6252570,_6252572)).
'blocked_Positive._impl\'23showsPrec\'23Prelude.Show\'23Positive.Positive\'23'(_6252740,_6252746,_6252752):-hnf(partcall(3,'Prelude._def\'23showsPrec\'23Prelude.Show',[partcall(1,'Positive._inst\'23Prelude.Show\'23Positive.Positive\'23',[])]),_6252740,_6252746,_6252752).

'Positive._impl\'23showList\'23Prelude.Show\'23Positive.Positive\'23'(_6256338,_6256340,_6256342):-freeze(_6256340,'blocked_Positive._impl\'23showList\'23Prelude.Show\'23Positive.Positive\'23'(_6256338,_6256340,_6256342)).
'blocked_Positive._impl\'23showList\'23Prelude.Show\'23Positive.Positive\'23'(_6256510,_6256516,_6256522):-hnf('Prelude._def\'23showList\'23Prelude.Show'(partcall(1,'Positive._inst\'23Prelude.Show\'23Positive.Positive\'23',[])),_6256510,_6256516,_6256522).

'Positive._inst\'23Prelude.Data\'23Positive.Positive\'23'(_6259720,_6259722,_6259724,_6259726):-freeze(_6259724,'blocked_Positive._inst\'23Prelude.Data\'23Positive.Positive\'23'(_6259720,_6259722,_6259724,_6259726)).
'blocked_Positive._inst\'23Prelude.Data\'23Positive.Positive\'23'(_6259806,_6260498,_6260504,_6260510):-hnf(_6259806,_6262748,_6260504,_6262760),'blocked_Positive._inst\'23Prelude.Data\'23Positive.Positive\'23_1'(_6262748,_6260498,_6262760,_6260510).

'blocked_Positive._inst\'23Prelude.Data\'23Positive.Positive\'23_1'(_6263314,_6263316,_6263318,_6263320):-freeze(_6263318,'blocked_blocked_Positive._inst\'23Prelude.Data\'23Positive.Positive\'23_1'(_6263314,_6263316,_6263318,_6263320)).
'blocked_blocked_Positive._inst\'23Prelude.Data\'23Positive.Positive\'23_1'('Prelude.()','Prelude._Dict\'23Data'(partcall(2,'Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23',[]),'Positive._impl\'23aValue\'23Prelude.Data\'23Positive.Positive\'23'),_6263666,_6263666):-!.
'blocked_blocked_Positive._inst\'23Prelude.Data\'23Positive.Positive\'23_1'('FAIL'(_6265316),'FAIL'(_6265316),_6265330,_6265330):-nonvar(_6265316).

'Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23'(_6267642,_6267644,_6267646,_6267648,_6267650):-freeze(_6267648,'blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23'(_6267642,_6267644,_6267646,_6267648,_6267650)).
'blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23'(_6267738,_6267756,_6268812,_6268818,_6268824):-hnf(_6267738,_6271502,_6268818,_6271520),'blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1'(_6271502,_6267756,_6268812,_6271520,_6268824).

'blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1'(_6272154,_6272156,_6272158,_6272160,_6272162):-freeze(_6272160,'blocked_blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1'(_6272154,_6272156,_6272158,_6272160,_6272162)).
'blocked_blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1'('Positive.Pos'(_6267872),_6267756,_6273082,_6273088,_6273094):-!,hnf(_6267756,_6276522,_6273088,_6276540),'blocked_blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1_Positive.Pos_2'(_6276522,_6267872,_6273082,_6276540,_6273094).

'blocked_blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1_Positive.Pos_2'(_6277330,_6277332,_6277334,_6277336,_6277338):-freeze(_6277336,'blocked_blocked_blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1_Positive.Pos_2'(_6277330,_6277332,_6277334,_6277336,_6277338)).
'blocked_blocked_blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1_Positive.Pos_2'('Positive.Pos'(_6267994),_6267872,_6277728,_6277734,_6277740):-!,hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Prelude.Int\'23',_6267872),_6267994),_6277728,_6277734,_6277740).
'blocked_blocked_blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1_Positive.Pos_2'('FAIL'(_6279572),_6267872,'FAIL'(_6279572),_6279586,_6279586):-nonvar(_6279572).
'blocked_blocked_Positive._impl\'23\'3D\'3D\'3D\'23Prelude.Data\'23Positive.Positive\'23_1'('FAIL'(_6279650),_6267756,'FAIL'(_6279650),_6279664,_6279664):-nonvar(_6279650).

'Positive._impl\'23aValue\'23Prelude.Data\'23Positive.Positive\'23'(_6282014,_6282016,_6282018):-freeze(_6282016,'blocked_Positive._impl\'23aValue\'23Prelude.Data\'23Positive.Positive\'23'(_6282014,_6282016,_6282018)).
'blocked_Positive._impl\'23aValue\'23Prelude.Data\'23Positive.Positive\'23'('Positive.Pos'('Prelude._impl\'23aValue\'23Prelude.Data\'23Prelude.Int\'23'),_6282192,_6282192).

'Positive.unPositive'(_6284072,_6284074,_6284076,_6284078):-freeze(_6284076,'blocked_Positive.unPositive'(_6284072,_6284074,_6284076,_6284078)).
'blocked_Positive.unPositive'(_6284158,_6284494,_6284500,_6284506):-hnf(_6284158,_6285556,_6284500,_6285568),'blocked_Positive.unPositive_1'(_6285556,_6284494,_6285568,_6284506).

'blocked_Positive.unPositive_1'(_6285924,_6285926,_6285928,_6285930):-freeze(_6285928,'blocked_blocked_Positive.unPositive_1'(_6285924,_6285926,_6285928,_6285930)).
'blocked_blocked_Positive.unPositive_1'('Positive.Pos'(_6284274),_6286312,_6286318,_6286324):-!,hnf(_6284274,_6286312,_6286318,_6286324).
'blocked_blocked_Positive.unPositive_1'('FAIL'(_6286740),'FAIL'(_6286740),_6286754,_6286754):-nonvar(_6286740).

'Positive.positive'(_6287584,_6287586,_6287588,_6287590):-freeze(_6287588,'blocked_Positive.positive'(_6287584,_6287586,_6287588,_6287590)).
'blocked_Positive.positive'(_6287670,_6290778,_6290784,_6290790):-makeShare(_6287670,_6289018),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_6289018),0),_6291864,_6290784,_6291836),'blocked_Positive.positive_ComplexCase'(_6291864,_6289018,_6290778,_6291836,_6290790).

'blocked_Positive.positive_ComplexCase'(_6292266,_6292268,_6292270,_6292272,_6292274):-freeze(_6292272,freeze(_6292266,'blocked_blocked_Positive.positive_ComplexCase'(_6292266,_6292268,_6292270,_6292272,_6292274))).
'blocked_blocked_Positive.positive_ComplexCase'('Prelude.True',_6289018,'Prelude.Just'('Positive.Pos'(_6289018)),_6292666,_6292666).
'blocked_blocked_Positive.positive_ComplexCase'('Prelude.False',_6289018,_6294540,_6294546,_6294552):-!,hnf('Prelude.otherwise',_6296774,_6294546,_6296746),'blocked_blocked_Positive.positive_ComplexCase_Prelude.False_ComplexCase'(_6296774,_6289018,_6294540,_6296746,_6294552).

'blocked_blocked_Positive.positive_ComplexCase_Prelude.False_ComplexCase'(_6297386,_6297388,_6297390,_6297392,_6297394):-freeze(_6297392,freeze(_6297386,'blocked_blocked_blocked_Positive.positive_ComplexCase_Prelude.False_ComplexCase'(_6297386,_6297388,_6297390,_6297392,_6297394))).
'blocked_blocked_blocked_Positive.positive_ComplexCase_Prelude.False_ComplexCase'('Prelude.True',_6289018,'Prelude.Nothing',_6297786,_6297786).
'blocked_blocked_blocked_Positive.positive_ComplexCase_Prelude.False_ComplexCase'('Prelude.False',_6289018,_6298848,_6298854,_6298860):-!,hnf(reportFailure4PAKCS('Positive.positive',['Prelude.False']),_6298848,_6298854,_6298860).
'blocked_blocked_blocked_Positive.positive_ComplexCase_Prelude.False_ComplexCase'('FAIL'(_6300276),_6289018,'FAIL'(_6300276),_6300290,_6300290).
'blocked_blocked_Positive.positive_ComplexCase'('FAIL'(_6300350),_6289018,'FAIL'(_6300350),_6300364,_6300364).

'Positive.unsafePositive'(_6301426,_6301428,_6301430,_6301432):-freeze(_6301430,'blocked_Positive.unsafePositive'(_6301426,_6301428,_6301430,_6301432)).
'blocked_Positive.unsafePositive'(_6301512,_6310042,_6310048,_6310054):-makeShare(_6301512,_6307664),hnf('Prelude.apply'('Prelude.apply'('Prelude._impl\'23\'3E\'23Prelude.Ord\'23Prelude.Int\'23',_6307664),0),_6311344,_6310048,_6311316),'blocked_Positive.unsafePositive_ComplexCase'(_6311344,_6307664,_6310042,_6311316,_6310054).

'blocked_Positive.unsafePositive_ComplexCase'(_6311782,_6311784,_6311786,_6311788,_6311790):-freeze(_6311788,freeze(_6311782,'blocked_blocked_Positive.unsafePositive_ComplexCase'(_6311782,_6311784,_6311786,_6311788,_6311790))).
'blocked_blocked_Positive.unsafePositive_ComplexCase'('Prelude.True',_6307664,'Positive.Pos'(_6307664),_6312182,_6312182).
'blocked_blocked_Positive.unsafePositive_ComplexCase'('Prelude.False',_6307664,_6313152,_6313158,_6313164):-!,hnf('Prelude.error'(['^u','^n','^s','^a','^f','^e','^P','^o','^s','^i','^t','^i','^v','^e',^:,'^ ','^n','^o','^n',^-,'^p','^o','^s','^i','^t','^i','^v','^e','^ ','^v','^a','^l','^u','^e']),_6313152,_6313158,_6313164).
'blocked_blocked_Positive.unsafePositive_ComplexCase'('FAIL'(_6320740),_6307664,'FAIL'(_6320740),_6320754,_6320754).

'Positive.showPositive'(_6321740,_6321742,_6321744,_6321746):-freeze(_6321744,'blocked_Positive.showPositive'(_6321740,_6321742,_6321744,_6321746)).
'blocked_Positive.showPositive'(_6321826,_6322342,_6322348,_6322354):-hnf(_6321826,_6323476,_6322348,_6323488),'blocked_Positive.showPositive_1'(_6323476,_6322342,_6323488,_6322354).

'blocked_Positive.showPositive_1'(_6323856,_6323858,_6323860,_6323862):-freeze(_6323860,'blocked_blocked_Positive.showPositive_1'(_6323856,_6323858,_6323860,_6323862)).
'blocked_blocked_Positive.showPositive_1'('Positive.Pos'(_6321942),_6324244,_6324250,_6324256):-!,hnf('Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23Prelude.Int\'23',_6321942),_6324244,_6324250,_6324256).
'blocked_blocked_Positive.showPositive_1'('FAIL'(_6325296),'FAIL'(_6325296),_6325310,_6325310):-nonvar(_6325296).

:-costCenters(['']).




%%%%% Number of shared variables: 2

