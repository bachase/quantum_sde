(* Mathematica Test File *)
TestExecute[
	Needs["ItoCalculus`"];
]

Test[
	QuantumItoProductRule[{QuantumOperator[a],QuantumOperator[b]},{QuantumOperator[da],QuantumOperator[db]}]
	,
	QuantumOperator[a]**QuantumOperator[db] + QuantumOperator[da]**QuantumOperator[b] + QuantumOperator[da]**QuantumOperator[db]
	,
	TestID->"Check quantum ito product rule Form"
]

Test[
	QuantumItoProductRule[{QuantumOperator[a],QuantumOperator[b],QuantumOperator[c]},{QuantumOperator[da],QuantumOperator[db],QuantumOperator[dc]}]
	,
	QuantumOperator[a]**QuantumOperator[b]**QuantumOperator[dc] + QuantumOperator[a]**QuantumOperator[db]**QuantumOperator[c] + QuantumOperator[a]**QuantumOperator[db]**QuantumOperator[dc] + QuantumOperator[da]**QuantumOperator[b]**QuantumOperator[c] + QuantumOperator[da]**QuantumOperator[b]**QuantumOperator[dc] + QuantumOperator[da]**QuantumOperator[db]**QuantumOperator[c] + QuantumOperator[da]**QuantumOperator[db]**QuantumOperator[dc]
	,
	TestID->"Check quantum ito product rule Form with 3"
]

Test[
	ItoProductRule[{a,b},{da,db}]
	,
	a*db + da*b + da*db
	,
	TestID->"Check classical ito product rule Form"
]

