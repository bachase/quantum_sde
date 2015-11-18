(* Test conditional expectation semantics *)
TestExecute[
	Needs["QuantumOperator`"];
]

Test[
	QuantumConditionalExpectation[4*Sin[x] ]
	,
	4*Sin[x]
	,
	TestID->"Non quantum operators are themselves"
]

Test[
	{QuantumConditionalExpectation[4* QuantumOperator[Jx] ],
	QuantumConditionalExpectation[QuantumOperator[Jx] *4 ]}
	,
	{4 * QuantumConditionalExpectation[QuantumOperator[Jx]],
			4 * QuantumConditionalExpectation[QuantumOperator[Jx]] }
	,
	TestID->"Constants factor out"
]

Test[
	QuantumConditionalExpectationDistribute[QuantumConditionalExpectation[ 4 * QuantumOperator[Jx] + QuantumOperator[Jz]**QuantumOperator[Jy] ]]
	,
	4 * QuantumConditionalExpectation[QuantumOperator[Jx] ] + 
		QuantumConditionalExpectation[ QuantumOperator[Jz]**QuantumOperator[Jy] ]
	,
	TestID->"Condition expectation distributes over addition"
]

Test[
	QuantumConditionalExpectationDistribute[QuantumConditionalExpectation[ 4 * QuantumOperator[Jx] + QuantumOperator[Jz]**QuantumOperator[Jy] ]
	  - QuantumConditionalExpectation[ QuantumOperator[Jz] + 2 * QuantumOperator[Jy] ]]
	,
	4 * QuantumConditionalExpectation[QuantumOperator[Jx] ] + 
		QuantumConditionalExpectation[ QuantumOperator[Jz]**QuantumOperator[Jy] ]
		- QuantumConditionalExpectation[ QuantumOperator[Jz] ]
		- 2 * QuantumConditionalExpectation[ QuantumOperator[Jy] ]
	,
	TestID->"Condition expectation distributes over addition complicated"
]