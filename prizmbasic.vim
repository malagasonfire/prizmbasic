" Vim syntax file
" Language:	PrizmBasic
" Maintainer:	Malagas 
" Last Change:  2019 Dec 28 by Malagas

" First version based on Micro$soft QBASIC circa 1989, as documented in
" 'Learn BASIC Now' by Halvorson&Rygmyr. Microsoft Press 1989.
" This syntax file not a complete implementation yet.  Send suggestions to the
" maintainer.
" Implementation for Casio CG50 Prizm calc 2019 based on basic syntax

" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" A bunch of useful BASIC keywords
" .. changed for Casio Prizm calculators
syn keyword basicStatement	And
syn keyword basicStatement	Break
syn keyword basicStatement	ClrGraph ClrList ClrMat ClrText ClrVct
syn keyword basicStatement	Cls ColorAuto ColorLighter ColorLinkOnlyY
syn keyword basicStatement	CellSum( CellProd( CellMin( CellMax( CellMean( CellMedian( CellIf(
syn keyword basicStatement	ColorLinkX&Y ColorLinkOnlyX 
syn keyword basicStatement	ColorLinkOn ColorLinkOff
syn keyword basicStatement	ColorNormal ColorClr ColorLinkX&Freq 
syn keyword basicStatement	DisplayData Disps Display% D@_Var
syn keyword basicStatement	Do LpWhile
syn keyword basicStatement	DrawGraph DrawDyna DrawStat DrawWeb
syn keyword basicStatement	DrawFTG-Con DrawFTG-Plt DrawR-Con DrawR-Plt
syn keyword basicStatement	DrawRSigma-Con DrawRSigma-Plt
syn keyword basicStatement	D@_SelOn D@_SelOff D@_Var D@_Start D@_End D@_pitch 
syn keyword basicStatement	Eng EngOn EngOff 
syn keyword basicStatement	ERROR
syn keyword basicStatement	Factor Fill Fix
syn keyword basicStatement	File File1 File2 File3 File4 File5 File6
syn keyword basicStatement	f1 f2 f3 f4 f5 f6 fn
syn keyword basicStatement	For To Step Next
syn keyword basicStatement	Getkey Goto
syn keyword basicStatement	GraphY Graphr GraphXt GraphYt GraphX
syn keyword basicStatement	If Then Else IfEnd
syn keyword basicStatement	Int Integral Dsz Isz
syn keyword basicStatement	LabelOn LocusOn LabelOff LocusOff
syn keyword basicStatement	Lbl
syn keyword basicStatement	LCM( GCD( List->Mat( SortA( SortD(
syn keyword basicStatement	Line Linear LinRegTTest
syn keyword basicStatement	List List1 List2 List3 List4 List5 List6 
syn keyword basicStatement	ln Locate Lower Log log
syn keyword basicStatement	Max( Sum Prod Cuml Percent Dlist 
syn keyword basicStatement	Min( Mean( Median( Seq(
syn keyword basicStatement	MOD( MOD_Exp(
syn keyword basicStatement	MSa MSe SSa SSe
syn keyword basicStatement	MSb MSab SSab SSb
syn keyword basicStatement	Neg Norm N-Dist NormalG Normal None
syn keyword basicStatement	NormPD( NormCD( InvNormCD(
syn keyword basicStatement	Or Not Xor
syn keyword basicStatement	ERROR BLANK OpenComport38k CloseComport38k
syn keyword basicStatement	Graph@_Y= Graph@_Integral Graph@_Y> Graph@_Y<
syn keyword basicStatement	Graph@_Y>= Graph@_Y<= Graph@_r= Graph(X,Y)=( Para,
syn keyword basicStatement	Pie Bar DotG
syn keyword basicStatement	Plot PlotPhase Line Pol( Rec(
syn keyword basicStatement	Ply@_Coef Sim@_Result Sim@_Coef Ply@_Result
syn keyword basicStatement	ProbP( ProbQ( ProbR( Probt( 
syn keyword basicStatement	Prog Prod Percent Cuml Imaginary
syn keyword basicStatement	P/Year C/Year Power
syn keyword basicStatement	Ran# RanInt#( RanList#( RanBin#( RanNorm#( RanSamp#( nCr nPr
syn keyword basicStatement	Rec( Pol( >DMS dms deg rad gra &o @D8
syn keyword basicStatement	Real ReP Arg Conjg Imaginary ImP >a+bi >re^Theta a+bi re^Theta 
syn keyword basicStatement	Ref Det Identity Augment( List->Mat( Mat->List( *Row *Row+ Row+
syn keyword basicStatement	Rref CrossP( DotP( Angle( UnitV( Norm( Trn Dim Mat Swap
syn keyword basicStatement	Regression_a Regression_b Regression_r
syn keyword basicStatement	Regression_c Regression_d Regression_e
syn keyword basicStatement	RightXmin RightXmax RightXscl RightYmin RightYmax RightYscl
syn keyword basicStatement	RightTThetamin RightTThetamax RightTThetaptch
syn keyword basicStatement	Rnd Abs Frac Intg RndFix( GCD( LCM( MOD( MOD_Exp( 
syn keyword basicStatement	R@_SelOn NormalG BrokenThickG ThickG  
syn keyword basicStatement	R@_SelOff DotG ThinG 
syn keyword basicStatement	Subscriptn anType an+1Type an+2Type
syn keyword basicStatement	an an+1 an+2 bn bn+1 bn+2 cn cn+1 cn+2 
syn keyword basicStatement	Sigmaan Sigmaan+1 Sigmaan+2
syn keyword basicStatement	Sigmabn Sigmabn+1 Sigmabn+2
syn keyword basicStatement	Sigmacn Sigmacn+1 Sigmacn+2
syn keyword basicStatement	R@_Result anStart bnStart CnStart R@_Start
syn keyword basicStatement	R@_End a0 a1 a2 b0 b1 b2 c0 c1 c2
syn keyword basicStatement	Sel@_a0 Sel@_a1 SetG-Color
syn keyword basicStatement	S-Gph1 S-Gph2 S-Gph3 DrawOn DrawOff
syn keyword basicStatement	Scatter xyLine Broken Quart Log Hist MedBox Exp(ae^bx) Exp(a^bx)
syn keyword basicStatement	SinReg PowerReg LogisticReg LogReg QuadReg CubicReg QuartReg Med-MedLine LinearReg(a+bx) NPPlot
syn keyword basicStatement	Square Cross Dot DrawDistNorm DrawDistT DrawDistChi DrawDistF 1-Variable 2-Variable LinearReg(ax+b)
syn keyword basicStatement	StdDev( OneSampleTTest TwoSampleTTest LinRegTTest ChiGOFTest ChiTest TwoSampleFTest OneWayANOVA TwoWayANOVA
syn keyword basicStatement	StdDev_sigma( Variance_sigma^2( Variance( OneSampleZTest TwoSampleZTest OnePropZTest TwoPropZTest
syn keyword basicStatement	Y=Type r=Type ParamType X=Type X>Type X<Type Y>Type Y<Type Y>=Type Y<=Type X<=Type
syn keyword basicStatement	StoGMEM G@_SelOn G@_SelOff
syn keyword basicStatement	RclGMEM GraphY Graphr GraphXt GraphYt GraphX
syn keyword basicStatement	T@_SelOn T@_SelOff F@_Result F@_Start F@_End F@_pitch
syn keyword basicStatement	VarList1 VarList1 VarList2 VarList3 VarList4 VarList5 VarList6
syn keyword basicStatement	[week] [day] [h] [min] [s] [ms] [micros] [ns]


syn keyword basicFunction 	CellSum CellProd CellMin CellMax CellMean CellMedian CellIf
syn keyword basicFunction 	LCM GCD List->Mat SortA SortD
syn keyword basicFunction 	Max Min Mean Median Seq
syn keyword basicFunction 	MOD MOD_Exp
syn keyword basicFunction 	NormPD NormCD InvNormCD
syn keyword basicFunction 	Graph(X,Y)=
syn keyword basicFunction 	Pol Rec
syn keyword basicFunction 	ProbP ProbQ ProbR Probt 
" .. Last Edit
syn keyword basicFunction 	Ran# RanInt#( RanList#( RanBin#( RanNorm#( RanSamp#( nCr nPr
syn keyword basicFunction 	Rec( Pol( >DMS dms deg rad gra &o @D8
syn keyword basicFunction 	Real ReP Arg Conjg Imaginary ImP >a+bi >re^Theta a+bi re^Theta 
syn keyword basicFunction 	Ref Det Identity Augment( List->Mat( Mat->List( *Row *Row+ Row+
syn keyword basicFunction 	Rref CrossP( DotP( Angle( UnitV( Norm( Trn Dim Mat Swap
syn keyword basicFunction 	Regression_a Regression_b Regression_r
syn keyword basicFunction 	Regression_c Regression_d Regression_e
syn keyword basicFunction 	RightXmin RightXmax RightXscl RightYmin RightYmax RightYscl
syn keyword basicFunction 	RightTThetamin RightTThetamax RightTThetaptch
syn keyword basicFunction 	Rnd Abs Frac Intg RndFix( GCD( LCM( MOD( MOD_Exp( 
syn keyword basicFunction 	R@_SelOn NormalG BrokenThickG ThickG  
syn keyword basicFunction 	R@_SelOff DotG ThinG 
syn keyword basicFunction 	Subscriptn anType an+1Type an+2Type
syn keyword basicFunction 	an an+1 an+2 bn bn+1 bn+2 cn cn+1 cn+2 
syn keyword basicFunction 	Sigmaan Sigmaan+1 Sigmaan+2
syn keyword basicFunction 	Sigmabn Sigmabn+1 Sigmabn+2
syn keyword basicFunction 	Sigmacn Sigmacn+1 Sigmacn+2
syn keyword basicFunction 	R@_Result anStart bnStart CnStart R@_Start
syn keyword basicFunction 	R@_End a0 a1 a2 b0 b1 b2 c0 c1 c2
syn keyword basicFunction 	Sel@_a0 Sel@_a1 SetG-Color
syn keyword basicFunction 	S-Gph1 S-Gph2 S-Gph3 DrawOn DrawOff
syn keyword basicFunction 	Scatter xyLine Broken Quart Log Hist MedBox Exp(ae^bx) Exp(a^bx)
syn keyword basicFunction 	SinReg PowerReg LogisticReg LogReg QuadReg CubicReg QuartReg Med-MedLine LinearReg(a+bx) NPPlot
syn keyword basicFunction 	Square Cross Dot DrawDistNorm DrawDistT DrawDistChi DrawDistF 1-Variable 2-Variable LinearReg(ax+b)
syn keyword basicFunction 	StdDev( OneSampleTTest TwoSampleTTest LinRegTTest ChiGOFTest ChiTest TwoSampleFTest OneWayANOVA TwoWayANOVA
syn keyword basicFunction 	StdDev_sigma( Variance_sigma^2( Variance( OneSampleZTest TwoSampleZTest OnePropZTest TwoPropZTest
syn keyword basicFunction 	Y=Type r=Type ParamType X=Type X>Type X<Type Y>Type Y<Type Y>=Type Y<=Type X<=Type
syn keyword basicFunction 	StoGMEM G@_SelOn G@_SelOff
syn keyword basicFunction 	RclGMEM GraphY Graphr GraphXt GraphYt GraphX
syn keyword basicFunction 	T@_SelOn T@_SelOff F@_Result F@_Start F@_End F@_pitch
syn keyword basicFunction 	VarList1 VarList1 VarList2 VarList3 VarList4 VarList5 VarList6
syn keyword basicFunction 	[week] [day] [h] [min] [s] [ms] [micros] [ns]

" .. Last Edit


syn keyword basicFunction	ABS abs Abs ASC asc Asc
syn keyword basicFunction	ATN atn Atn CDBL cdbl Cdbl
syn keyword basicFunction	CINT cint Cint CLNG clng Clng
syn keyword basicFunction	COS cos Cos CSNG csng Csng
syn keyword basicFunction	CSRLIN csrlin Csrlin CVD cvd Cvd
syn keyword basicFunction	CVDMBF cvdmbf Cvdmbf CVI cvi Cvi
syn keyword basicFunction	CVL cvl Cvl CVS cvs Cvs
syn keyword basicFunction	CVSMBF cvsmbf Cvsmbf EOF eof Eof
syn keyword basicFunction	ERDEV erdev Erdev ERL erl Erl
syn keyword basicFunction	ERR err Err EXP exp Exp
syn keyword basicFunction	FILEATTR fileattr Fileattr FIX fix Fix
syn keyword basicFunction	FRE fre Fre FREEFILE freefile Freefile
syn keyword basicFunction	INP inp Inp INSTR instr Instr
syn keyword basicFunction	INT int Int LBOUND lbound Lbound
syn keyword basicFunction	LEN len Len LOC loc Loc
syn keyword basicFunction	LOF lof Lof LOG log Log
syn keyword basicFunction	LPOS lpos Lpos PEEK peek Peek
syn keyword basicFunction	PEN pen Pen POINT point Point
syn keyword basicFunction	POS pos Pos RND rnd Rnd
syn keyword basicFunction	SADD sadd Sadd SCREEN screen Screen
syn keyword basicFunction	SEEK seek Seek SETMEM setmem Setmem
syn keyword basicFunction	SGN sgn Sgn SIN sin Sin
syn keyword basicFunction	SPC spc Spc SQR sqr Sqr
syn keyword basicFunction	STICK stick Stick STRIG strig Strig
syn keyword basicFunction	TAB tab Tab TAN tan Tan
syn keyword basicFunction	UBOUND ubound Ubound VAL val Val
syn keyword basicFunction	VALPTR valptr Valptr VALSEG valseg Valseg
syn keyword basicFunction	VARPTR varptr Varptr VARSEG varseg Varseg
syn keyword basicFunction	CHR$ Chr$ chr$ COMMAND$ command$ Command$
syn keyword basicFunction	DATE$ date$ Date$ ENVIRON$ environ$ Environ$
syn keyword basicFunction	ERDEV$ erdev$ Erdev$ HEX$ hex$ Hex$
syn keyword basicFunction	INKEY$ inkey$ Inkey$ INPUT$ input$ Input$
syn keyword basicFunction	IOCTL$ ioctl$ Ioctl$ LCASES$ lcases$ Lcases$
syn keyword basicFunction	LAFT$ laft$ Laft$ LTRIM$ ltrim$ Ltrim$
syn keyword basicFunction	MID$ mid$ Mid$ MKDMBF$ mkdmbf$ Mkdmbf$
syn keyword basicFunction	MKD$ mkd$ Mkd$ MKI$ mki$ Mki$
syn keyword basicFunction	MKL$ mkl$ Mkl$ MKSMBF$ mksmbf$ Mksmbf$
syn keyword basicFunction	MKS$ mks$ Mks$ OCT$ oct$ Oct$
syn keyword basicFunction	RIGHT$ right$ Right$ RTRIM$ rtrim$ Rtrim$
syn keyword basicFunction	SPACE$ space$ Space$ STR$ str$ Str$
syn keyword basicFunction	STRING$ string$ String$ TIME$ time$ Time$
syn keyword basicFunction	UCASE$ ucase$ Ucase$ VARPTR$ varptr$ Varptr$
syn keyword basicTodo contained	TODO

"integer number, or floating point number without a dot.
syn match  basicNumber		"\<\d\+\>"
"floating point number, with dot
syn match  basicNumber		"\<\d\+\.\d*\>"
"floating point number, starting with a dot
syn match  basicNumber		"\.\d\+\>"

" String and Character contstants
syn match   basicSpecial contained "\\\d\d\d\|\\."
syn region  basicString		  start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=basicSpecial
syn region  basicString		  start=+'+  skip=+\\\\\|\\'+  end=+'+  contains=basicSpecial

syn region  basicComment	start="REM" end="$" contains=basicTodo
syn region  basicComment	start="^[ \t]*'" end="$" contains=basicTodo
syn region  basicLineNumber	start="^\d" end="\s"
syn match   basicTypeSpecifier  "[a-zA-Z0-9][\$%&!#]"ms=s+1
" Used with OPEN statement
syn match   basicFilenumber  "#\d\+"
"syn sync ccomment basicComment
" syn match   basicMathsOperator "[<>+\*^/\\=-]"
syn match   basicMathsOperator   "-\|=\|[:<>+\*^/\\]\|AND\|OR"

" Define the default highlighting.
" Only when an item doesn't have highlighting yet

hi def link basicLabel		Label
hi def link basicConditional	Conditional
hi def link basicRepeat		Repeat
hi def link basicLineNumber	Comment
hi def link basicNumber		Number
hi def link basicError		Error
hi def link basicStatement	Statement
hi def link basicString		String
hi def link basicComment		Comment
hi def link basicSpecial		Special
hi def link basicTodo		Todo
hi def link basicFunction		Identifier
hi def link basicTypeSpecifier Type
hi def link basicFilenumber basicTypeSpecifier
"hi basicMathsOperator term=bold cterm=bold gui=bold


let b:current_syntax = "prizmbasic"

let &cpo = s:cpo_save
unlet s:cpo_save
" vim: ts=8
