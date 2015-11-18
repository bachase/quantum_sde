(********************************************************)
(* DSolveMatrix											*)
(*   Solves matrix differential equations               *)
(*												        *)
(* Brad Chase, UNM, May 20 2008                         *)
(* 														*)
(* 														*)
(********************************************************)

BeginPackage["DSolveMatrix`"]

DSolveMatrixRiccati::usage="DSolveMatrixRiccati[A,B,C,D,Zinit,t=0] solves the Matrix Riccati equation dZ/dt = AZ-ZD-ZCZ+B with initial condition Z(t) = Zinit.";
DSolveMatrixFirstOrder::usage="DSolveMatrixFirstOrder[A,B,C, Zinit, t=0] solves the first order matrix differential equation dZ/dt = AZ + ZB +C with initial condition Z(t) = Zinit.";


DSolveMatrixRiccati::UnableToSolve = "Unable to solve the differential equation";
DSolveMatrixFirstOrder::UnableToSolve = "Unable to solve the differential equation";

t;

Begin["`Private`"]

	FixMat[Mat_,desiredDim_] := If[Length[Dimensions[Mat]] > 0 , Mat , 0 * IdentityMatrix[desiredDim]];
	
	DSolveMatrixRiccati[AmatIn_,BmatIn_,CmatIn_,DmatIn_,initMat_,tInit_:0]:=Module[{dim = Dimensions[initMat,1][[1]]},
		Amat = FixMat[AmatIn,dim]; Bmat = FixMat[BmatIn,dim]; Cmat = FixMat[CmatIn,dim]; Dmat = FixMat[DmatIn,dim];
	
      Xmat[tvar_] := Outer[
Subscript[X, {#1,#2}][tvar]&,Table[i,{i,1,dim}],Table[i,{i,1,dim}]];
      Ymat[tvar_] := Outer[
Subscript[Y, {#1,#2}][tvar]&,Table[i,{i,1,dim}],Table[i,{i,1,dim}]];
	
      eqs  = Join[Flatten[Amat.Xmat[t] + Bmat.Ymat[t] ],Flatten[Cmat.Xmat[t] + Dmat.Ymat[t]]];
      vars = Join[Flatten[Xmat[t]],Flatten[Ymat[t]]];
      system = MapThread[#1==#2&,{D[vars,t],eqs}];
      sol =DSolve[system,vars,t][[1]];
		If[Length[Cases[sol,_Rule]] < 1,
			Message[MatrixRiccati::UnableToSolve];
			Return[0];, 1];
			
     xsol = ReplaceAll[Xmat[t],sol];
     ysol = ReplaceAll[Ymat[t],sol];
	 sol = xsol.Inverse[ysol];
	 y0 = MapThread[#1 -> #2 &, {Flatten[Simplify[ysol /. t -> tInit]], 
	    Flatten[IdentityMatrix[dim]]}];
	 x0 = MapThread[#1 -> #2 &, {Flatten[Simplify[xsol /. t -> tInit]], 
	    Flatten[initMat]}];
	 initialConds = Solve[Map[#1[[1]]== #1[[2]] &,Join[x0, y0]], Table[ C[i], {i,1, 2 * dim*dim}]][[1]];
	 Simplify[ReplaceAll[sol,initialConds]]
];
	
	DSolveMatrixFirstOrder[AmatIn_,BmatIn_,CmatIn_,initMat_, tInit_:0] := Module[{dim=Dimensions[initMat,1][[1]]},
		   DSolveMatrixRiccati[AmatIn,CmatIn,0,-BmatIn,initMat,tInit]
	];
	
End[]
EndPackage[]
