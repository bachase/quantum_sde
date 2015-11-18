(* Mathematica Test File *)
TestExecute[
	Needs["ItoCalculus`"];
]
Test[
	QuantumItoDifferential[x,y,e,f] + QuantumItoDifferential[x,z,f,e]
	,
	QuantumItoDifferential[2*x,y+z,e+f,e+f]
	,
	TestID->"Check that ito terms add"
]

Test[
	QuantumItoDifferential[x,y,e,f] - QuantumItoDifferential[x,z,f,e]
	,
	QuantumItoDifferential[0,y-z,e-f,f-e]
	,
	TestID->"Check that ito terms subtract"
]

Test[
	QuantumItoDifferential[x,y,e,f] ** QuantumItoDifferential[x,z,f,e]
	,
	QuantumItoDifferential[y**f,y**e,f**f,f**e]
	,
	TestID->"Check that ito product"
]

Test[
	QuantumItoDifferential[a,{b1,b2},{c1,c2},{{d11,d12},{d21,d22}}] 
		** QuantumItoDifferential[e,{f1,f2},{g1,g2},{{h11,h12},{h21,h22}}] 
	,
	QuantumItoDifferential[b1 ** g1 + b2 ** g2,
		{b1 ** h11 + b2 ** h21, b1 ** h12 + b2 ** h22},
		{d11 ** g1 + d12 ** g2, d21 ** g1 + d22 ** g2},
		{{d11 ** h11 + d12 ** h21, 
  d11 ** h12 + d12 ** h22}, {d21 ** h11 + d22 ** h21, 
  d21 ** h12 + d22 ** h22}}]
	,
	TestID->"Check multi-dimensional Ito product"
]

Test[
	QuantumItoDifferential[a,{b1,b2},{c1,c2},{{d11,d12},{d21,d22}}] **f
	,
	QuantumItoDifferential[a**f,{b1**f,b2**f},{c1**f,c2**f},
				{{d11**f,d12**f},{d21**f,d22**f}}] 
	,
	TestID->"Check multi-dimensional multiplication"
]