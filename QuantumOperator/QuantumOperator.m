(* QuantumOperator *)
(* Encapsulates the non-commutatives rules for a quantum operator *)

(* Brad Chase, QMC@UNM *)
(* Created by the Wolfram Workbench Aug 22, 2008 *)

BeginPackage["QuantumOperator`"]

QuantumOperator::usage = "QuantumOperator is the head for a quantum operator.  Use non-commutative
multiply (**) to multiply these objects correctly. "

AddQuantumCommutator::usage = "QuantumCommutator allows you to specify rules for performing commutation 
relations.  QuantumCommutator[ Operator[a], Operator[b], Operator[c] ] sets a rule that 
Operator[b] ** Operator[a] -> Operator[a] ** Operator[b]  - Operactor[c]. All rules set are then used
by calling QuantumCommutatorSimplify on an expression.  Returns the list of current rules."

AddQuantumCommutator::AlreadyDefined = " Error:: Commutator already defined"

RemoveQuantumCommutator::usage = " Removes the specified commutator from the list of rules. 
So RemoveQuantumCommutator[Operator[a],Operator[b] ] removes the rules for those operators.  Returns the list
of current rules"

GetQuantumCommutators::usage = " Returns current list of quantum commutator rules ";
ClearQuantumCommutators::usage = " Clears list of quantum commutator rules";

QuantumCommutatorSimplify::usage = " QuantumCommutatorSimplify[expr] simplifies expr by applying
the established commutator rules.  Note that this might not be the `simpliest' form, but essentially gives
the normal ordering as given by the commutator rules."

QuantumTensor::usage = " QuantumTensor[a,b,...] represents a tensor product of quantum systems "
QuantumTensorExpand::usage = " QuantumTensorExpand tries to expand and simplify the given expression "
QuantumTensorFactor::usage = " QuantumTensor factor attempts to factor all common elements of the given expression "

QuantumConditionalExpectation::usage = " QuantumConditionalExpectation[a] represents the conditional expectation of the given quantum expression "
QuantumConditionalExpectationDistribute::usage = " QuantumConditionalExpectationExpand[a] distributes QuantumConditionalExpectation terms over sums of its arguments "

Begin["`Private`"]

	protected = Unprotect[NonCommutativeMultiply, Times, Plus]
	
	
	(* Object types *)
	ConstantQ[e_] := FreeQ[e, QuantumOperator] && FreeQ[e, TensorQ]
	QuantumOperatorQ[e_] := !FreeQ[e,QuantumOperator]
	QuantumTensorQ[e_] := FreeQ[e, QuantumTensor]
	
	(* Define QuantumOperator, non-commutative multiply semantics *)
	0 ** _ := 0
	_ ** 0 := 0
	QuantumOperator[0] ** _ := 0
	_ ** QuantumOperator[0] := 0
	(* adding 0 returns same operator *)
	QuantumOperator/: QuantumOperator[0] + P_ := P
	QuantumOperator/: QuantumOperator[0] a_ := 0
	QuantumOperator[1] ** P_ := P;
	P_ ** QuantumOperator[1] := P;
	P_ ** Q_Plus := Map[(P ** #1) &, Q]
	Q_Plus ** P_ := Map[(#1 ** P) &, Q]
	P_ ** (c_ Q_) := c P ** Q /; ConstantQ[c]
	(c_ P_) ** Q_ := c P ** Q /; ConstantQ[c]
	P_ ** c_ := c P/; ConstantQ[c]		
	c_ ** P_ := c P/; ConstantQ[c]
	
	
	(* Commutator Replacement Rules *)
	commutatorRules = {}

	commutatorIdentities = {(a_QuantumOperator ** b_QuantumOperator ** c_QuantumOperator - 
  							c_QuantumOperator ** a_QuantumOperator ** b_QuantumOperator) :> 
 								a ** ReplaceAll[b**c - c**b,commutatorRules ] 
 									+ ReplaceAll[a**c - c**a,commutatorRules] ** b,
 							(a_QuantumOperator ** b_QuantumOperator ** c_QuantumOperator - 
  							b_QuantumOperator ** c_QuantumOperator ** a_QuantumOperator) :> 
 								ReplaceAll[a**b - b**a,commutatorRules ]  ** c
 									+ b**ReplaceAll[a**c - c**a,commutatorRules] }

	(* Only add if no definition yet, also prevent infinite loop definition *)
	(* If commutator is constant, reform as constant times Identity op *)
	AddQuantumCommutator[a_QuantumOperator, b_QuantumOperator, c_Integer] := 
			AddQuantumCommutator[a,b,c * QuantumOperator[1] ];
	
	AddQuantumCommutator[a_QuantumOperator, b_QuantumOperator, c_] :=
		If[MemberQ[commutatorRules, b**a->_ ] || MemberQ[commutatorRules, a**b ->_ ] ,
			Message[AddQuantumCommutator::AlreadyDefined],
			(commutatorRules = Join[commutatorRules, {b**a -> a**b - c}])]
			  
	GetQuantumCommutators[] := commutatorRules
	ClearQuantumCommutators[] := commutatorRules = {}
	
	(* Remove and return list *)
	RemoveQuantumCommutator[a_QuantumOperator, b_QuantumOperator] :=
		commutatorRules = DeleteCases[commutatorRules, b**a->_]

	(* Just replace all until reach a fixed point *)
	QuantumCommutatorSimplify[expr_] :=  Simplify[Simplify[expr //. Join[commutatorRules, commutatorIdentities]] //. Join[commutatorRules, commutatorIdentities] ]
	
	
	(* Define QuantumTensor semantics *)
	QuantumTensor[___,0,___] := 0
	(* product is product over members *)
	QuantumTensor[a__]**QuantumTensor[b__] := 
				Apply[QuantumTensor,MapThread[#1**#2 &,{{a},{b}}]]/; SameQ[Length[{a}],Length[{b}]]
	QuantumTensor[a___, c_ P_, b___] := c * QuantumTensor[a,P,b]/; ConstantQ[c]
	
	
	QuantumTensorFactor[expr_] := expr//. {QuantumTensor[a___,b_,c___] + QuantumTensor[a___,d_,c___] :> QuantumTensor[a,b+d,c],
										   pre1_*QuantumTensor[a___,b_,c___] + QuantumTensor[a___,d_,c___] :> QuantumTensor[a,pre1*b+d,c],
										   QuantumTensor[a___,b_,c___] + pre2_*QuantumTensor[a___,d_,c___] :> QuantumTensor[a,b+pre2*d,c],
										   pre1_*QuantumTensor[a___,b_,c___] + pre2_*QuantumTensor[a___,d_,c___] :> QuantumTensor[a,pre1*b+pre2*d,c]}
	QuantumTensorExpand[expr_] := expr //. QuantumTensor[a___] :> Distribute[QuantumTensor[a] ]
	
	(* Define QuantumConditionalExpectation semantics *)
	QuantumConditionalExpectation[c_] := c /; ConstantQ[c];
	QuantumConditionalExpectation[a_ *b_] := a*QuantumConditionalExpectation[b] /; ConstantQ[a]
	QuantumConditionalExpectationDistribute[ expr_ ] := expr /. {HoldPattern[QuantumConditionalExpectation[a_]] :> Distribute[QuantumConditionalExpectation[a]] }
	(***** Load Notations ****)
   If[$Notebooks,
		targetNotebook = EvaluationNotebook[];
		notationNotebook = NotebookOpen["QuantumOperatorNotation.nb", Visible->False];
		SelectionMove[notationNotebook, All, Notebook];
		SelectionEvaluateCreateCell[notationNotebook];
     ] 

	(* reprotect things *)
	Protect[ Evaluate[ protected ] ]
End[]

EndPackage[]

