(* Test QuantumTensor Semantics *)
TestExecute[
	Needs["QuantumOperator`"];
]

Test[
	QuantumTensor[0]
	,
	0
	,
	TestID->"Tensor of 0 is 0"
]

Test[
	{QuantumTensor[0,QuantumOperator[a],QuantumOperator[b]],
	 QuantumTensor[QuantumOperator[a],0,QuantumOperator[b]],
  	 QuantumTensor[QuantumOperator[a],QuantumOperator[b],0]
	}
	,
	{0,0,0}
	,
	TestID->"Tensor of 0 and anything is zero"
]

Test[
	QuantumTensor[QuantumOperator[a],QuantumOperator[b] ] ** QuantumTensor[QuantumOperator[c],QuantumOperator[d] ]
	,
	QuantumTensor[QuantumOperator[a]**QuantumOperator[c],QuantumOperator[b]**QuantumOperator[d] ]
	,
	TestID->"Tensor[a,b]**Tensor[c,d] = Tensor[a**c,b**d]"
]

Test[
	QuantumTensor[QuantumOperator[1],QuantumOperator[b] ] ** QuantumTensor[QuantumOperator[a],QuantumOperator[1] ]
	,
	QuantumTensor[QuantumOperator[a],QuantumOperator[b] ]
	,
	TestID->"Tensor[QuantumOperator[1],QuantumOperator[b]]**Tensor[QuantumOperator[a],QuantumOperator[1]] =
					 Tensor[QuantumOperator[a],QuantumOperator[b]]"
]

Test[
	QuantumTensor[ pi * QuantumOperator[a], QuantumOperator[b], QuantumOperator[c] ]
	,
	pi * QuantumTensor[ QuantumOperator[a], QuantumOperator[b], QuantumOperator[c]]
	,
	TestID->"Constants factor out"
]

Test[
	QuantumTensorFactor[QuantumTensor[ a, b, c,e] - QuantumTensor[a ,d,c,e]]
	,
	QuantumTensor[a, b - d,c,e]
	,
	TestID->"QuantumTensorFactor with common elements add"
]

Test[
	QuantumTensorFactor[QuantumTensor[a,1] + QuantumTensor[b,1] - QuantumTensor[c, 1] + QuantumTensor[e,f]]
	,
	QuantumTensor[a+b-c,1] + QuantumTensor[e,f]
	,
	TestID->"QuantumTensorFactor with arbitrary number of common elements add"
]

Test[
	QuantumTensorFactor[QuantumTensor[a,1] + (1/2)*QuantumTensor[b,1] - 2*QuantumTensor[c, 1] + QuantumTensor[e,f]]
	,
	QuantumTensor[a+(1/2)*b- 2* c,1] + QuantumTensor[e,f]
	,
	TestID->"QuantumTensorFactor with prefactors add"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[Jx],QuantumOperator[Jy], I*QuantumOperator[Jz]];
	AddQuantumCommutator[QuantumOperator[Jy],QuantumOperator[Jz], I*QuantumOperator[Jx]];
	AddQuantumCommutator[QuantumOperator[Jz],QuantumOperator[Jx], I*QuantumOperator[Jy]];
	QuantumTensorExpand[QuantumCommutatorSimplify[QuantumTensor[QuantumOperator[Jx],QuantumOperator[Jx]] **QuantumTensor[QuantumOperator[Jy],QuantumOperator[Jy]]
	 - QuantumTensor[QuantumOperator[Jy],QuantumOperator[Jy]] ** QuantumTensor[QuantumOperator[Jx],QuantumOperator[Jx]]]]
	,
	I*QuantumTensor[QuantumOperator[Jx]**QuantumOperator[Jy], QuantumOperator[Jz]] + I*QuantumTensor[QuantumOperator[Jz], QuantumOperator[Jx]**QuantumOperator[Jy]] + QuantumTensor[QuantumOperator[Jz], QuantumOperator[Jz]]
	,
	TestID->"Try some commutator tensor product simplifications"
]