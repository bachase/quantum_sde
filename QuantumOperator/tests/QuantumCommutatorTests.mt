(* Test the quantum commutator file *)
TestExecute[
	Needs["QuantumOperator`"];
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[a],QuantumOperator[b],QuantumOperator[c] ];
	AddQuantumCommutator[QuantumOperator[b],QuantumOperator[a],-QuantumOperator[c] ]	
	,
	Null,
	 {AddQuantumCommutator::AlreadyDefined}
	,
	TestID->"Can't add reverse of ones already added"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[a],QuantumOperator[b],QuantumOperator[c] ]
	,
	{QuantumOperator[b]**QuantumOperator[a]->QuantumOperator[a]**QuantumOperator[b] - QuantumOperator[c]}
	,
	TestID->"Add commutator"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[a],QuantumOperator[b],QuantumOperator[c] ];
	ClearQuantumCommutators[]
	,
	{}
	,
	TestID->"Clear should give empty"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[a],QuantumOperator[b],QuantumOperator[c] ];
	GetQuantumCommutators[]
	,
	{QuantumOperator[b]**QuantumOperator[a]->QuantumOperator[a]**QuantumOperator[b] - QuantumOperator[c]}
	,
	TestID->"Check GetQuantumCommmutators"
]

Test[
	ClearQuantumCommutators[];	
	AddQuantumCommutator[QuantumOperator[a],QuantumOperator[b],QuantumOperator[c] ];
	AddQuantumCommutator[QuantumOperator[a],QuantumOperator[b],QuantumOperator[d] ];
	,
	Null
	,
	{AddQuantumCommutator::AlreadyDefined},
	TestID->"Commutator already defined error"
]
Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[a],QuantumOperator[b],QuantumOperator[c] ];
	AddQuantumCommutator[QuantumOperator[a],QuantumOperator[c],-QuantumOperator[b] ]
	,
	{ QuantumOperator[b]**QuantumOperator[a]->QuantumOperator[a]**QuantumOperator[b] - QuantumOperator[c],
	   QuantumOperator[c]**QuantumOperator[a]->QuantumOperator[a]**QuantumOperator[c] + QuantumOperator[b]}
	,
	TestID->"Can add different rules that involve same operator"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[a], QuantumOperator[b], I]
	,
	{QuantumOperator[b]**QuantumOperator[a] -> QuantumOperator[a] ** QuantumOperator[b] - I}
	,
	TestID->"Number commutator is really number times identity"
]

Test[
	ClearQuantumCommutators[];
	{AddQuantumCommutator[QuantumOperator[a], QuantumOperator[b], I],
		RemoveQuantumCommutator[QuantumOperator[a],QuantumOperator[b] ] }
	,
	{{QuantumOperator[b]**QuantumOperator[a] -> -I + QuantumOperator[a]**QuantumOperator[b]}, {}}
	,
	TestID->"Test remove commutator"
]	

Test[
	ClearQuantumCommutators[];
	{AddQuantumCommutator[QuantumOperator[a], QuantumOperator[b], I],
		RemoveQuantumCommutator[QuantumOperator[c],QuantumOperator[b] ] }
	,
	{{QuantumOperator[b]**QuantumOperator[a] -> -I + QuantumOperator[a]**QuantumOperator[b]}, {QuantumOperator[b]**QuantumOperator[a] -> -I + QuantumOperator[a]**QuantumOperator[b]}}
	,
	TestID->"Test remove commutator if not there"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[x],QuantumOperator[p], I*\[HBar]];
	QuantumCommutatorSimplify[QuantumOperator[x]**QuantumOperator[p] + QuantumOperator[p]**QuantumOperator[x]]
	,
	2*QuantumOperator[x]**QuantumOperator[p] - I*\[HBar]
	,
	TestID->"Check QuantumCommutatorSimplify x**p + p**x = 2*x**p - I \[HBar]"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[x],QuantumOperator[p], I*\[HBar]];
	QuantumCommutatorSimplify[QuantumOperator[x]**QuantumOperator[p] - QuantumOperator[p]**QuantumOperator[x]]
	,
	I*\[HBar]
	,
	TestID->"Check QuantumComutatorSimplify x**p - p**x = I \[HBar]"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[Jx],QuantumOperator[Jy], I*QuantumOperator[Jz]];
	AddQuantumCommutator[QuantumOperator[Jy],QuantumOperator[Jz], I*QuantumOperator[Jx]];
	AddQuantumCommutator[QuantumOperator[Jz],QuantumOperator[Jx], I*QuantumOperator[Jy]];		
	QuantumCommutatorSimplify[QuantumOperator[Jx]**QuantumOperator[Jy]**QuantumOperator[Jy] - QuantumOperator[Jy]**QuantumOperator[Jy]**QuantumOperator[Jx]]
	,
	(2*I)*QuantumOperator[Jy]**QuantumOperator[Jz] + QuantumOperator[Jx]
	,
	TestID->"Check QuantumComutatorSimplify Jx**Jy^2 - Jy^2**Jx = "
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[Jx],QuantumOperator[Jy], I*QuantumOperator[Jz]];
	AddQuantumCommutator[QuantumOperator[Jy],QuantumOperator[Jz], I*QuantumOperator[Jx]];
	AddQuantumCommutator[QuantumOperator[Jz],QuantumOperator[Jx], I*QuantumOperator[Jy]];		
	QuantumCommutatorSimplify[QuantumOperator[Jz]**QuantumOperator[Jx]**QuantumOperator[Jz] 
					- 1/2*QuantumOperator[Jx]**QuantumOperator[Jz]**QuantumOperator[Jz]
					- 1/2*QuantumOperator[Jz]**QuantumOperator[Jz]**QuantumOperator[Jx]]
	,
	-1/2*QuantumOperator[Jx]
	,
	TestID->"Check QuantumComutatorSimplify Jz**Jx**Jz -1/2*Jz**Jz**Jx - 1/2*Jx**Jz**Jz = -Jx"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[Jx],QuantumOperator[Jy], I*QuantumOperator[Jz]];
	AddQuantumCommutator[QuantumOperator[Jy],QuantumOperator[Jz], I*QuantumOperator[Jx]];
	AddQuantumCommutator[QuantumOperator[Jz],QuantumOperator[Jx], I*QuantumOperator[Jy]];		
	QuantumCommutatorSimplify[QuantumOperator[Jz]**QuantumOperator[Jx]**QuantumOperator[Jx]**QuantumOperator[Jz] 
					- 1/2*QuantumOperator[Jx]**QuantumOperator[Jx]**QuantumOperator[Jz]**QuantumOperator[Jz]
					- 1/2*QuantumOperator[Jz]**QuantumOperator[Jz]**QuantumOperator[Jx]**QuantumOperator[Jx]]
	,
	QuantumOperator[Jy]**QuantumOperator[Jy] - QuantumOperator[Jx]**QuantumOperator[Jx]
	,
	TestID->"Check QuantumComutatorSimplify Jz**Jx^2**Jz -1/2*Jz**Jz**Jx^2 - 1/2*Jx^2**Jz**Jz = Jy^2-Jx^2"
]

Test[
	ClearQuantumCommutators[];
	AddQuantumCommutator[QuantumOperator[Jx],QuantumOperator[Jy], I*QuantumOperator[Jz]];
	AddQuantumCommutator[QuantumOperator[Jy],QuantumOperator[Jz], I*QuantumOperator[Jx]];
	AddQuantumCommutator[QuantumOperator[Jz],QuantumOperator[Jx], I*QuantumOperator[Jy]];		
	QuantumCommutatorSimplify[2*QuantumOperator[Jz]**QuantumOperator[Jx]**QuantumOperator[Jz] 
					- 1/2*QuantumOperator[Jx]**QuantumOperator[Jz]**QuantumOperator[Jz]
					- 1/2*QuantumOperator[Jz]**QuantumOperator[Jz]**QuantumOperator[Jx]]
	,
	QuantumOperator[Jz]**QuantumOperator[Jx]**QuantumOperator[Jz] -1/2*QuantumOperator[Jx]
	,
	TestID->"Check with prefix Jz**Jx**Jz -1/2*Jz**Jz**Jx - 1/2*Jx**Jz**Jz = -Jx"
]
