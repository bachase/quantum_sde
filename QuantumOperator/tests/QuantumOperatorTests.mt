(* Test the quantum operator file *)
TestExecute[
	Needs["QuantumOperator`"];
]

Test[
	QuantumOperator[a]**0
	,
	0
	,
	TestID->"Test Op ** 0"
]

Test[
	0**QuantumOperator[a]
	,
	0
	,
	TestID->"Test 0 ** Op"
]

Test[
	QuantumOperator[a]**QuantumOperator[0]
	,
	0
	,
	TestID->"Op ** 0 Op"
]

Test[
	QuantumOperator[0]**QuantumOperator[a]
	,
	0
	,
	TestID->"Op 0 ** Op"
]

Test[
	QuantumOperator[a] + QuantumOperator[0]
	,
	QuantumOperator[a]
	,
	TestID->"Op + 0 op"
]

Test[
	QuantumOperator[0] + QuantumOperator[a]
	,
	QuantumOperator[a]
	,
	TestID->"0 op + Op"
]

Test[
	QuantumOperator[1] ** QuantumOperator[a]
	,
	QuantumOperator[a]
	,
	TestID->"Id op ** Op"
]

Test[
	QuantumOperator[a] ** QuantumOperator[b]
	,
	QuantumOperator[a] ** QuantumOperator[b]
	,
	TestID->"Op ** Op doesn't simplify"
]

Test[
	QuantumOperator[a] **  ( 2 * QuantumOperator[b] )
	,
	2 * QuantumOperator[a] ** QuantumOperator[b]
	,
	TestID->"Opa ** (2 * Opb) == 2 * (Opa ** Opb)"
]

Test[
	QuantumOperator[a] ** 5
	,
	5 * QuantumOperator[a]
	,
	TestID->"Noncommutative times a constant restuls in normal times"
]

Test[
	QuantumOperator[a] ** ( 2 * QuantumOperator[b] + QuantumOperator[c] * 5 )
	,
	2 * QuantumOperator[a] ** QuantumOperator[b] + 5 * QuantumOperator[a] ** QuantumOperator[c]
	,
	TestID->"Distribute and constant multiply"
]

Test[
	2 QuantumOperator[a] ** QuantumOperator[b] + 3 QuantumOperator[a] ** QuantumOperator[b]
	,
	5 QuantumOperator[a] ** QuantumOperator[b]
	,
	TestID->"Add identical ops"
]

Test[
	Operator[a] ** Operator[b] - Operator[a] ** Operator[b]
	,
	0
	,
	TestID->"Subtracts to zeros"
]