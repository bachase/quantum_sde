(****************************************************
* Gaussian Moments                                 *
*   Calculates moments for gaussian quantum states *
*												   *
* Brad Chase                                       *
* UNM, May 2008									   *
***************************************************)
BeginPackage["GaussianMoments`"]

GaussianMoments::usage = "GaussianMoments[n,{o_1,o_2,...,o_k}] calculates expectations for n gaussian quantum systems.  The list {o_i} indicates the product of operators to take, where index 1 and 2 are the x and p operators on system 1, index 3 and 4 are the x and p operators on system 2, etc.  For example GaussianMoments[2,{1,2,3,1}] calculates <x_1*p_1,*x_2*x_1>";




GaussianMoments[n_Integer, kvec_List] :=  Module[{names,dummyNames,commMat,m=Length[kvec]},
names = Flatten[Table[{Subscript[x, {i}],Subscript[p, {i}]},{i,1,n}]]; (* variable names *)
meanVec = Flatten[Table[{{Subscript[\[Mu], Subscript[x, {i}]]},{Subscript[\[Mu], Subscript[p, {i}]]}},{i,1,n}]]; (* vector of means *)
covMat =  Outer[If[#1 < #2,Subscript[V, {names[[#1]],names[[#2]]}],Subscript[V, {names[[#2]],names[[#1]]}]]&,
							Table[i,{i,1,2*n}],Table[i,{i,1,2*n}]]; (* covariance matrix *)
commMat = KroneckerProduct[IdentityMatrix[n],{{0,-1},{1,0}}]; (*commutation matrix *)
dummyNames = Table[Subscript[\[Xi], {i}],{i,1,m}]; (* dummy characteristic names *)

GaussianCharacteristicFunc[vec_]:=Exp[-1/4*vec.covMat.vec + I*meanVec.vec];
sig[a_,b_]:=a.commMat.b;
indexToVector[k_]:=IdentityMatrix[2*n][[k]];
characteristicOp[vec_]:= GaussianCharacteristicFunc[Fold[#1 +#2&,0,MapIndexed[Subscript[\[Xi], #2]indexToVector[#1]&,vec]]];

preFactor[vec_]:=Exp[-I/2*Sum[Subscript[\[Xi], {j}]Subscript[\[Xi], {l}]sig[indexToVector[vec[[j]]],indexToVector[vec[[l]]]],{l,1,m},{j,1,l}]];

Simplify[(-I)^m*ReplaceAll[Apply[D,Join[{preFactor[kvec]*characteristicOp[kvec]},dummyNames]],Map[#1->0&,dummyNames]]]]

EndPackage[]