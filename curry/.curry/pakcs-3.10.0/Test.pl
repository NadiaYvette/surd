%PAKCS3.10 swi9 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Interval').
:-importModule('Poly').
:-importModule('Positive').
:-importModule('Prelude').
:-importModule('PrimeFactors').
:-importModule('RadExpr').
:-importModule('Rational').

:-curryModule('Test').


%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('Test.testRat',testRat,0,'Test.testRat',nofix,notype).
functiontype('Test.testPrime',testPrime,0,'Test.testPrime',nofix,notype).
functiontype('Test.testPoly',testPoly,0,'Test.testPoly',nofix,notype).
functiontype('Test.testRadExpr',testRadExpr,0,'Test.testRadExpr',nofix,notype).
functiontype('Test.testInterval',testInterval,0,'Test.testInterval',nofix,notype).
functiontype('Test.main',main,0,'Test.main',nofix,notype).
functiontype('Test.unlines',unlines,1,'Test.unlines',nofix,notype).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/7.
:-dynamic constructortype/7.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
'Test.testRat'(_8185384,_8185386,_8185388):-freeze(_8185386,'blocked_Test.testRat'(_8185384,_8185386,_8185388)).
'blocked_Test.testRat'(_8191524,_8191530,_8191536):-makeShare(_8185470,_8192158),makeShare(_8185488,_8192178),makeShare(_8185506,_8192198),makeShare(_8185524,_8192218),makeShare(_8185542,_8192238),hnf('Prelude.cond'(letrec4PAKCS(_8192158,'Rational.mkRat'(3,4)),'Prelude.cond'(letrec4PAKCS(_8192178,'Rational.mkRat'(1,2)),'Prelude.cond'(letrec4PAKCS(_8192198,'Rational.ratAdd'(_8192158,_8192178)),'Prelude.cond'(letrec4PAKCS(_8192218,'Rational.ratMul'(_8192158,_8192178)),'Prelude.cond'(letrec4PAKCS(_8192238,'Rational.ratDiv'(_8192158,_8192178)),'Prelude.++'(['^s','^u','^m',^=],'Prelude.++'('Prelude.apply'('Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23',_8192198),'Prelude.++'(['^ ','^p','^r','^o','^d',^=],'Prelude.++'('Prelude.apply'('Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23',_8192218),'Prelude.++'(['^ ','^d','^i','^v',^=],'Prelude.apply'('Rational._impl\'23show\'23Prelude.Show\'23Rational.Rational\'23',_8192238))))))))))),_8191524,_8191530,_8191536).

'Test.testPrime'(_8205008,_8205010,_8205012):-freeze(_8205010,'blocked_Test.testPrime'(_8205008,_8205010,_8205012)).
'blocked_Test.testPrime'(_8208490,_8208496,_8208502):-makeShare(_8205094,_8208650),hnf('Prelude.cond'(letrec4PAKCS(_8208650,'PrimeFactors.factorise'('Positive.unsafePositive'(360))),'Prelude.++'(['^f','^a','^c','^t','^o','^r','^i','^s','^e','^(','^3','^6','^0','^)',^=],'Prelude.apply'('Prelude._impl\'23show\'23Prelude.Show\'23\'5B\'5D\'230\'23\'23'(partcall(1,'Prelude._inst\'23Prelude.Show\'23\'28\'2C\'29\'230\'23\'231\'23\'23',[partcall(1,'Prelude._inst\'23Prelude.Show\'23Prelude.Int\'23',[]),partcall(1,'Prelude._inst\'23Prelude.Show\'23Prelude.Int\'23',[])])),_8208650))),_8208490,_8208496,_8208502).

'Test.testPoly'(_8215936,_8215938,_8215940):-freeze(_8215938,'blocked_Test.testPoly'(_8215936,_8215938,_8215940)).
'blocked_Test.testPoly'(_8221580,_8221586,_8221592):-makeShare(_8216022,_8221948),makeShare(_8216040,_8221968),makeShare(_8216058,_8221988),hnf('Prelude.cond'(letrec4PAKCS(_8221948,'Prelude.apply'('Poly.mkPoly',['Rational.fromInt'(1),'Rational.fromInt'(0),'Rational.fromInt'(1)])),'Prelude.cond'(letrec4PAKCS(_8221968,'Prelude.apply'('Poly.mkPoly',['Rational.fromInt'(1),'Rational.fromInt'(1)])),'Prelude.cond'(letrec4PAKCS(_8221988,'Poly.mulPoly'(_8221948,_8221968)),'Prelude.++'(['^p',^=],'Prelude.++'('Prelude.apply'('Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23',_8221948),'Prelude.++'(['^ ','^q',^=],'Prelude.++'('Prelude.apply'('Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23',_8221968),'Prelude.++'(['^ ','^p',^*,'^q',^=],'Prelude.apply'('Poly._impl\'23show\'23Prelude.Show\'23Poly.Poly\'23',_8221988))))))))),_8221580,_8221586,_8221592).

'Test.testRadExpr'(_8233642,_8233644,_8233646):-freeze(_8233644,'blocked_Test.testRadExpr'(_8233642,_8233644,_8233646)).
'blocked_Test.testRadExpr'(_8235986,_8235992,_8235998):-makeShare(_8233728,_8236164),makeShare(_8233746,_8236184),hnf('Prelude.cond'(letrec4PAKCS(_8236164,'Prelude.apply'('RadExpr.sqrtE','RadExpr.intE'(2))),'Prelude.cond'(letrec4PAKCS(_8236184,'RadExpr.Add'('RadExpr.intE'(1),_8236164)),'Prelude.++'(['^e','^x','^p','^r',^=],'RadExpr._impl\'23show\'23Prelude.Show\'23RadExpr.RadExpr\'230\'23\'23'(partcall(1,'Rational._inst\'23Prelude.Show\'23Rational.Rational\'23',[]),_8236184)))),_8235986,_8235992,_8235998).

'Test.testInterval'(_8242010,_8242012,_8242014):-freeze(_8242012,'blocked_Test.testInterval'(_8242010,_8242012,_8242014)).
'blocked_Test.testInterval'(_8245428,_8245434,_8245440):-makeShare(_8242096,_8245706),makeShare(_8242114,_8245726),makeShare(_8242132,_8245746),hnf('Prelude.cond'(letrec4PAKCS(_8245706,'Interval.IV'('Rational.mkRat'(1,2),'Rational.mkRat'(3,4))),'Prelude.cond'(letrec4PAKCS(_8245726,'Interval.IV'('Rational.mkRat'(1,3),'Rational.mkRat'(2,3))),'Prelude.cond'(letrec4PAKCS(_8245746,'Interval.iadd'(_8245706,_8245726)),'Prelude.++'(['^i','^1',^+,'^i','^2',^=],'Prelude.apply'('Interval._impl\'23show\'23Prelude.Show\'23Interval.Interval\'23',_8245746))))),_8245428,_8245434,_8245440).

'Test.main'(_8252870,_8252872,_8252874):-freeze(_8252872,'blocked_Test.main'(_8252870,_8252872,_8252874)).
'blocked_Test.main'(_8253882,_8253888,_8253894):-hnf('Test.unlines'(['Test.testRat','Test.testPrime','Test.testPoly','Test.testRadExpr','Test.testInterval']),_8253882,_8253888,_8253894).

'Test.unlines'(_8256398,_8256400,_8256402,_8256404):-freeze(_8256402,'blocked_Test.unlines'(_8256398,_8256400,_8256402,_8256404)).
'blocked_Test.unlines'(_8256484,_8257462,_8257468,_8257474):-hnf(_8256484,_8258272,_8257468,_8258284),'blocked_Test.unlines_1'(_8258272,_8257462,_8258284,_8257474).

'blocked_Test.unlines_1'(_8258598,_8258600,_8258602,_8258604):-freeze(_8258602,'blocked_blocked_Test.unlines_1'(_8258598,_8258600,_8258602,_8258604)).
'blocked_blocked_Test.unlines_1'([],[],_8258846,_8258846).
'blocked_blocked_Test.unlines_1'([_8256700|_8256718],_8259326,_8259332,_8259338):-!,hnf('Prelude.++'(_8256700,'Prelude.++'(['^010'],'Test.unlines'(_8256718))),_8259326,_8259332,_8259338).
'blocked_blocked_Test.unlines_1'('FAIL'(_8260850),'FAIL'(_8260850),_8260864,_8260864):-nonvar(_8260850).

:-costCenters(['']).




%%%%% Number of shared variables: 14

