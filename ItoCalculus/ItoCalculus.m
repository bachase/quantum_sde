(* ItoCalculus *)
(* Encapsulates the ito calculus/notation *)

(* Brad Chase, QMC@UNM *)

BeginPackage["ItoCalculus`"]
(* Exported symbols added here with SymbolName::usage *) 
ItoDifferential::usage = "Represents an ito sde term, with time variable t and stochastic
	increment dW_t"
QuantumItoDifferential::usage = "Represents a quantum sde term, with time variable t and stochastic
	terms dA_t, dA_t^{dag}, dLambda.  Supply vector and matrix arguments for a multidimensional version."	
ItoProductRule::usage = "Applies the Ito product rule to an arbitrary set of terms. \n\t ItoProductRule[Ops_List, dOps_list]
	returns the differential for the given list of operators multiplied in the order given, using the corresponding entires of dOps for the differentials."		
QuantumItoProductRule::usage = "Applies the Ito product rule to an arbitrary set of terms. \n\t ItoProductRule[Ops_List, dOps_list]
	returns the differential for the given list of operators multiplied in the order given, using the corresponding entires of dOps for the differentials."			
Begin["`Private`"]
	ItoDifferential[0,0] := 0;
	ItoDifferential/: ItoDifferential[a_,b_]  + ItoDifferential[c_,d_] :=
						ItoDifferential[ a +  c, b + d];
    ItoDifferential/: ItoDifferential[a_,b_]  - ItoDifferential[c_,d_] :=
						ItoDifferential[ a -  c, b - d];						
	ItoDifferential/: ItoDifferential[a_,b_]  * ItoDifferential[c_,d_] :=
						ItoDifferential[b * d , 0];
	ItoDifferential/: a_*ItoDifferential[c_,d_] := ItoDifferential[a * c,a * d]
	ItoDifferential/: Power[ItoDifferential[a_,b_], 2] := ItoDifferential[b * b, 0]
	
	(* General forms *)
	QuantumItoDifferential[0,0,0,0] := 0;
	QuantumItoDifferential/: QuantumItoDifferential[a_,b_,c_,d_] + QuantumItoDifferential[e_,f_,g_,h_] :=
							QuantumItoDifferential[a + e, b + f, c + g, d + h];
	QuantumItoDifferential/: QuantumItoDifferential[a_,b_,c_,d_] - QuantumItoDifferential[e_,f_,g_,h_] :=
							QuantumItoDifferential[a - e, b - f, c - g, d - h];	
	QuantumItoDifferential/: a_*QuantumItoDifferential[b_,c_,d_,e_] := QuantumItoDifferential[a * b,a * c, a*d,a*e];
					
	(* multidimensional forms *)
	QuantumItoDifferential/: QuantumItoDifferential[a_,b_?VectorQ,c_?VectorQ,d_?MatrixQ]  ** QuantumItoDifferential[e_,f_?VectorQ,g_?VectorQ,h_?MatrixQ] :=
								QuantumItoDifferential[Inner[NonCommutativeMultiply,b,g],
													   Inner[NonCommutativeMultiply,b,h],
													   Inner[NonCommutativeMultiply,d,g],
													   Inner[NonCommutativeMultiply,d,h]];
    QuantumItoDifferential/: a_**QuantumItoDifferential[b_,c_?VectorQ,d_?VectorQ,e_?MatrixQ] := QuantumItoDifferential[a**b, Map[a**#1 &, c],
    																							Map[a **#1 &, d], Map[ a** #1 &, e, {2}]];
    QuantumItoDifferential/: QuantumItoDifferential[b_,c_?VectorQ,d_?VectorQ,e_?MatrixQ] ** a_ := QuantumItoDifferential[b**a, Map[#1**a &, c],
    																							Map[#1**a &, d], Map[#1**a &, e, {2}]];
    QuantumItoDifferential/: Power[QuantumItoDifferential[a_,b_?VectorQ,c_?VectorQ,d_?MatrixQ], 2] := 								
    						    QuantumItoDifferential[Inner[NonCommutativeMultiply,b,c],
													   Inner[NonCommutativeMultiply,b,d],
													   Inner[NonCommutativeMultiply,d,c],
													   Inner[NonCommutativeMultiply,d,d]];

    																				   	
	(* 1 dimensional forms *)										
	QuantumItoDifferential/: QuantumItoDifferential[a_,b_,c_,d_]  ** QuantumItoDifferential[e_,f_,g_,h_] :=
						QuantumItoDifferential[b**g , b**h,d**g,d**h];

	QuantumItoDifferential/: a_**QuantumItoDifferential[b_,c_,d_,e_] := QuantumItoDifferential[a ** b,a ** c, a**d,a**e];
	QuantumItoDifferential/: QuantumItoDifferential[b_,c_,d_,e_] ** a_:= QuantumItoDifferential[ b** a, c** a, d**a, e**a];	
	QuantumItoDifferential/: Power[QuantumItoDifferential[a_,b_,c_,d_], 2] := QuantumItoDifferential[b ** c, b**d, d**c, d**d];
	
	(* Ito Product Rule *)

	ItoProductRule[Ops_?VectorQ, dOps_?VectorQ] := Plus @@ Map[Function[currDiffIndexes,
						Times @@ Map[If[MemberQ[currDiffIndexes,#1],dOps[[#1]],
								Ops[[#1]]] &, Range[Length[Ops]]]],
								   DeleteCases[Subsets[Range[Length[Ops]]],{}]]
								   
	QuantumItoProductRule[Ops_?VectorQ, dOps_?VectorQ] := Plus @@ Map[
						Function[productList,Fold[(#1**#2)&,productList[[1]],Rest[productList]]],
						Map[Function[currDiffIndexes,
						Map[If[MemberQ[currDiffIndexes,#1],dOps[[#1]],
								Ops[[#1]]] &, Range[Length[Ops]]]],
								   DeleteCases[Subsets[Range[Length[Ops]]],{}]]]
								   									
	If[$Notebooks,
		targetNotebook = EvaluationNotebook[];
		notationNotebook = NotebookOpen["ItoCalculusNotation.nb", Visible->False];
		SelectionMove[notationNotebook, All, Notebook];
		SelectionEvaluateCreateCell[notationNotebook];
     ] 
End[]

EndPackage[]

