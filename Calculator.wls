#!/usr/bin/env wolframscript
(* ::Package:: *)

s = "y 	0.5 	-4.143474048424998
  z 	0.625 	-3.1709615006770515
  s 	0.78125 	1.732020845644619
  u 	0.8931739295455593 	15.973438351860704
  v 	1.0211323755774153 	35.10114920824737
  w 	1.16742248509526 	62.78631425389338
  \[Alpha] 	1.3346705004190407 	99.28201254141936
  \[Beta] 	1.459278106369075 	144.78860583366378
  \[Gamma] 	1.6315225219592102 	199.47274109054294
  \[Delta] 	1.865258729062898 	263.4775066411068
  \[CurlyEpsilon] 	2.085422656904751 	336.92853801096174
  \[Zeta] 	2.384185791015625 	512.6072746595227
  \[Eta] 	2.725750517411985 	727.2885715481912
  \[Theta] 	3.1162487047650607 	849.4650465308255
  \[Iota] 	3.407188146764981 	1123.859483302632
  \[Kappa] 	3.9832092821269787 	1438.7509481751554
  \[Lambda] 	4.453363361722031 	1794.6202607002178
  \[Nu] 	5.206251464550825 	2191.902179662476
  \[Xi] 	5.820766091346741 	2630.993842401342";
varTable = # -> <|"a" -> ToExpression@#2, "b" -> ToExpression@#3|> & @@@ StringSplit[StringSplit[s, "\n"]] // Association;


s = "y 	2 	0.6992669815829572
  z 	2.2 	0.7336524096644791
  s 	2.42 	2.7382112580862277
  u 	2.662 	16.973438351860707
  v 	2.9282 	36.10114920785727
  w 	3.22102 	63.786314253893394
  \[Alpha] 	3.543122 	100.28201254141943
  \[Beta] 	3.8974342 	145.78860583366372
  \[Gamma] 	4.28717762 	200.47274109054277
  \[Delta] 	4.715895382 	264.47750664110686
  \[CurlyEpsilon] 	5.1874849202 	337.92853801096135
  \[Zeta] 	5.70623341222 	513.6072746595223
  \[Eta] 	6.276856753442 	728.2885715481901
  \[Theta] 	6.9045424287862 	850.4650465308251
  \[Iota] 	7.59499667166483 	1124.859483302631
  \[Kappa] 	8.354496338831312 	1439.7509481751538
  \[Lambda] 	9.189945972714444 	1795.620260700215
  \[Nu] 	10.108940569985888 	2192.9021796624766
  \[Xi] 	11.119834626984478 	2631.993842401341";
upTable = # -> <|"a" -> ToExpression@#2, "b" -> ToExpression@#3|> & @@@ StringSplit[StringSplit[s, "\n"]] // Association;


s = "y 	1
  z 	0.27
  s 	0.09
  u 	0.1
  v 	0.12
  w 	0.17
  \[Alpha] 	0.22
  \[Beta] 	0.5
  \[Gamma] 	0.7
  \[Delta] 	0.9
  \[CurlyEpsilon] 	2
  \[Zeta] 	3
  \[Eta] 	4
  \[Theta] 	4.5
  \[Iota] 	6
  \[Kappa] 	7
  \[Lambda] 	8
  \[Nu] 	9
  \[Xi] 	10";
starTable = # -> <|"a" -> ToExpression@#2|> & @@@ StringSplit[StringSplit[s, "\n"]] // Association;


fToLevel[llf_, assoc_] :=
	Ramp @ Floor[((llf - Log10[Log10[2]]) / Log10[2] - assoc["b"]) / assoc["a"] + 1]


getExponent[l_] :=
	Which[
		l < 10,
			0
		,
		l < 25,
			1
		,
		l < 1500,
			l / 25 + 1
		,
		l < 6000,
			l / 50 + 31
		,
		l < 10000,
			l / 100 + 91
		,
		l < 24000,
			l / 200 + 141
		,
		True,
			l / 400 + 201
	] // Floor


toNextExponent[l_] :=
	Which[
		l < 10,
			10 - l
		,
		l < 25,
			25 - l
		,
		l < 1500,
			Mod[-l, 25, 1]
		,
		l < 6000,
			Mod[-l, 50, 1]
		,
		l < 10000,
			Mod[-l, 100, 1]
		,
		l < 24000,
			Mod[-l, 200, 1]
		,
		True,
			Mod[-l, 400, 1]
	]


totalVar[l_] := totalVar[l] = l*2^getExponent[l]


starCost[cur_, d_, a_] := Sum[Floor[a (cur + i) Exp[a (cur + i)/200] + 1], {i, 0, d - 1}]


calcoptBLP[llf_, initstars_, ex_:0] :=
	Block[{vars, ups, upsc, calcpow2, calcpows, iter, dumpf, base, len = Length[varTable], starTable = (Values /* First) /@ starTable // Values, format, prereqs, getcons, prereqm, prereqb, co, po, le, lpo, c, m, b, initpow, stars, resx, resv, heux, heuv, myFirst, myLast, $toler = 10^-9, pows, powsd, ress, grid, weakThreshold, weaks, strongThreshold, strongs, getconsSingle, resx2, exps, postpow, coeff = 1.1, myprint = Sow @* Row @* List},
		exps = SparseArray[i_ /; i <= ex :> (coeff)^-(ex - i + 1), len, 1];
		vars = fToLevel[llf, #]& /@ varTable // Values;
		ups = fToLevel[llf, #] + 1& /@ upTable // Values;
		upsc = FoldList[Times, 1, Rest[ups]^Most[exps]] // N;
		calcpows[vv_] :=
			upsc (totalVar /@ vv)^exps;
		calcpow2[vv_] := calcpow2[vv] =
			Total[calcpows[vv]];
		postpow[p_] :=
			p^((coeff)^ex); (* off by power of 1.1^exp? *)
		format[l_] :=
			MapThread[Rule, {Keys[varTable], l}] // Association;
		getcons[i_, s_, prog_:0, costs_:{}, pows_:{}, progs_:{}] :=
			Block[{nprog, cost, pow},
				nprog = prog + toNextExponent[vars[[i]] + prog];
				cost = starCost[0, nprog, starTable[[i]]];
				pow = upsc[[i]] (totalVar[vars[[i]] + nprog]^exps[[i]] - totalVar[vars[[i]]]^exps[[i]]) // N;
				If[cost > s,
					{costs, pows, progs}
					,
					getcons[i, s, nprog, Append[costs, cost], Append[pows, pow], Append[progs, nprog]]
				]
			];
		getconsSingle[i_, s_, init_, prog_:0, costs_:{}, pows_:{}, progs_:{}] :=
			Block[{nprog, cost, pow},
				nprog = prog + 1;
				cost = starCost[init, nprog, starTable[[i]]];
				pow = upsc[[i]] (totalVar[vars[[i]] + init + nprog]^exps[[i]] - totalVar[vars[[i]] + init]^exps[[i]]) // N;
				If[cost > s,
					{costs, pows, progs}
					,
					getconsSingle[i, s, init, nprog, Append[costs, cost], Append[pows, pow], Append[progs, nprog]]
				]
			];
		prereqm[l_] := prereqm[l] =
			If[l == 0,
				{}
				,
				{ConstantArray[-1, l]}
			];
		prereqb[l_] := prereqb[l] =
			If[l == 0,
				0
				,
				-1
			];
		Do[
			{co[i], po[i], le[i]} = getcons[i, initstars]; lpo[i] = Length[po[i]], {i, len}
		];
		m = ArrayFlatten @ ReplacePart[
			ConstantArray[0, {len + 1, len}], {
				{i_, i_} :> prereqm[lpo[i]], {len + 1, i_} :> {-co[i]}
			}
		];
		b = Append[
			Array[prereqb @* lpo, len], -initstars
		];
		c = -Array[po, len, 1, Join];
		m = DeleteCases[m, {0 ..}];
		b = DeleteCases[b, 0];
		myLast[l_List] :=
			If[Length[l] == 0,
				-Infinity
				,
				Last @ l
			];
		heux = With[{pos = With[{arr = Array[myLast @* po, len]},
				First @ FirstPosition[arr, Max @ arr]
			]},
			Array[If[# == pos,
					SparseArray[-1 -> 1, lpo[#]]
					,
					SparseArray[{}, lpo[#]]
				]&, len, 1, Join
			]
		];
		heuv = heux . c;
		resx = LinearProgramming[Normalize @ c, m, b, Automatic, Integers, Tolerance -> $toler];
		resx = Internal`PartitionRagged[resx, Array[lpo, len]];
		myFirst[l_List] :=
			If[Length[l] == 0,
				0
				,
				First @ l
			];
		ress = myFirst /@ Pick[Array[co, len], resx, 1];
		resx = myFirst /@ Pick[Array[le, len], resx, 1];
		weakThreshold = 2 * Max[calcpows[resx + vars] / (resx + vars)];
		weaks = Position[calcpows[resx + vars], _?(NumericQ @ # && # <= weakThreshold&), {1}, Heads -> False];
		ress = ReplacePart[ress, weaks -> 0];
		resx = ReplacePart[resx, weaks -> 0];
		strongs = Complement[List /@ Range[len], weaks];
		Do[
			{co[i], po[i], le[i]} = Which[
				MemberQ[strongs, {i}],
					getconsSingle[i, initstars - Total @ ress, resx[[i]]]
				,
				MemberQ[weaks, {i}],
					getcons[i, initstars - Total @ ress]
				,
				True,
					{{}, {}, {}}
			];
			lpo[i] = Length[po[i]], {i, len}
		];
		m = ArrayFlatten @ ReplacePart[
			ConstantArray[0, {len + 1, len}], {
				{i_, i_} :> prereqm[lpo[i]], {len + 1, i_} :> {-co[i]}
			}
		];
		b = Append[
			Array[prereqb @* lpo, len], Total @ ress - initstars
		];
		c = -Array[po, len, 1, Join];
		m = DeleteCases[m, {0 ..}];
		b = DeleteCases[b, 0];
		resx2 = LinearProgramming[Normalize @ c, m, b, Automatic, Integers, Tolerance -> $toler];
		resx2 = Internal`PartitionRagged[resx2, Array[lpo, len]];
		ress += myFirst /@ Pick[Array[co, len], resx2, 1];
		resx += myFirst /@ Pick[Array[le, len], resx2, 1];
		initpow = calcpow2[vars];
		resv = calcpow2[vars + resx];
		heuv = initpow - heuv;
		resv = postpow @ resv;
		heuv = postpow @ heuv;
		myprint["dump y: ", heuv];
		myprint["found y: ", resv];
		myprint["speedup: ", PercentForm[resv / heuv - 1, {Infinity, 4}]];
		myprint["stars used: ", Total @ ress];
		myprint["stars left: ", initstars - Total @ ress];
		pows = (calcpows[resx + vars]) / calcpow2[resx + vars];
		powsd = (calcpows[resx + vars] - calcpows[vars]) / calcpow2[resx + vars];
		grid = {
			Keys[varTable], vars, resx + vars, NumberForm[#, Infinity, NumberSigns -> {"-", "+"}]& /@ resx, ress, PercentForm[#, {Infinity, 4}]& /@ pows, Table[
				Which[
					resx[[i]] == 0,
						"none"
					,
					powsd[[i]] >= 0.01,
						"must"
					,
					powsd[[i]] >= 0.001,
						"high"
					,
					powsd[[i]] >= 0.0001,
						"medium"
					,
					True,
						"low"
				], {i, len}
			]
		} // Transpose;
		grid = Prepend[grid, {"variable", "level before", "level after", "level purchased", "stars used", "power after", "priority"}];
		myprint[Grid[
			grid, Frame -> All, Background -> {
				None, Switch[Last @ #,
					"must",
						LightRed
					,
					"high",
						LightYellow
					,
					"medium",
						LightGreen
					,
					_,
						None
				]& /@ grid
			}
		]];
	] // Reap // Last // Last // Column;