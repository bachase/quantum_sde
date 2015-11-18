(* Mathematica Test File *)

TestExecute[
	Needs["ItoCalculus`"];
]
Test[
	ItoDifferential[x,y] + ItoDifferential[x,z]
	,
	ItoDifferential[2*x,y+z]
	,
	TestID->"Check that ito terms add"
]

Test[
	ItoDifferential[x,y] - ItoDifferential[x,z]
	,
	ItoDifferential[0,y-z]
	,
	TestID->"Check that ito terms subtract"
]

Test[
	ItoDifferential[x,y] * ItoDifferential[a,b]
	,
	ItoDifferential[y*b,0]
	,
	TestID->"Check ito product rule"
]

Test[
	2 * ItoDifferential[x,y]
	,
	ItoDifferential[2*x,2*y]
	,
	TestID->"Constants fold in"
]

Test[
	Power[ItoDifferential[x,y],2]
	,
	ItoDifferential[y*y,0]
	,
	TestID->"Test simplify squares"
]