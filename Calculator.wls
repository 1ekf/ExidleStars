#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*Optimal Star Distribution Calculator*)


(* ::Subtitle:: *)
(*by Eaux Tacous#1021*)


(* ::Section:: *)
(*Constants*)


(* ::Text:: *)
(*Taken from exidle wiki.*)


symbTable = {"y", "z", "s", "u", "v", "w", "\[Alpha]", "\[Beta]", "\[Gamma]", "\[Delta]", "\[CurlyEpsilon]", "\[Zeta]", "\[Eta]", "\[Theta]", "\[Iota]", "\[Kappa]", "\[Lambda]", "\[Nu]", "\[Xi]"};


(*varTable = {
    {0.5`, -4.143474048424998`},
    {0.625`, -3.1709615006770515`},
    {0.78125`, 1.732020845644619`},
    {0.8931739295455593`, 15.973438351860704`},
    {1.0211323755774153`, 35.10114920824737`},
    {1.16742248509526`, 62.78631425389338`},
    {1.3346705004190407`, 99.28201254141936`},
    {1.459278106369075`, 144.78860583366378`},
    {1.6315225219592102`, 199.47274109054294`},
    {1.865258729062898`, 263.4775066411068`},
    {2.085422656904751`, 336.92853801096174`},
    {2.384185791015625`, 512.6072746595227`},
    {2.725750517411985`, 727.2885715481912`},
    {3.1162487047650607`, 849.4650465308255`},
    {3.407188146764981`, 1123.859483302632`},
    {3.9832092821269787`, 1438.7509481751554`},
    {4.453363361722031`, 1794.6202607002178`},
    {5.206251464550825`, 2191.902179662476`},
    {5.820766091346741`, 2630.993842401342`}
};*)


(*upTable = {
    {2, 0.6992669815829572`},
    {2.2`, 0.7336524096644791`},
    {2.42`, 2.7382112580862277`},
    {2.662`, 16.973438351860707`},
    {2.9282`, 36.10114920785727`},
    {3.22102`, 63.786314253893394`},
    {3.543122`, 100.28201254141943`},
    {3.8974342`, 145.78860583366372`},
    {4.28717762`, 200.47274109054277`},
    {4.715895382`, 264.47750664110686`},
    {5.1874849202`, 337.92853801096135`},
    {5.70623341222`, 513.6072746595223`},
    {6.276856753442`, 728.2885715481901`},
    {6.9045424287862`, 850.4650465308251`},
    {7.59499667166483`, 1124.859483302631`},
    {8.354496338831312`, 1439.7509481751538`},
    {9.189945972714444`, 1795.620260700215`},
    {10.108940569985888`, 2192.9021796624766`},
    {11.119834626984478`, 2631.993842401341`}
};*)


(*starTable = {
    1,
    0.27`,
    0.09`,
    0.1`,
    0.12`,
    0.17`,
    0.22`,
    0.5`,
    0.7`,
    0.9`,
    2,
    3,
    4,
    4.5`,
    6,
    7,
    8,
    9,
    10
};*)


(* ::Section:: *)
(*Helper Functions*)


myprint = Sow @* (Style[#, RGBColor["#ccc"], Bold]&) @* Row @* List;


(*fToLevel =
    Function[{llf, a, b},
        Ramp @ Floor[((llf - Log[10, Log[10, 2]]) / Log[10, 2] - b) / a + 1]
    ];*)


(*getExponent =
    Function[l,
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
    ];*)


(*toNextExponent =
    Function[l,
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
    ];*)


(*totalVar = Function[l, Evaluate[l * 2.^getExponent[l]]];
totalVarExp = Function[{l, e}, Evaluate[l^e * 2.^(e getExponent[l])]];*)


(*singleStarCost = Function[{a, cur}, Floor[a cur Exp[a cur / 200] + 1]];
starCost = Function[{a, cur, d}, Sum[Floor[a (cur + i) Exp[a (cur + i) / 200] + 1], {i, 0, d - 1}]];*)


(* Prevent Mathematica from complaining *)
externLP = Flatten[LinearProgramming[##]]&;


(* ::Section:: *)
(*Main algorithm*)


calcoptBLPWrapper[in___] :=
    Print[
        StringReplace[
            "data:image/png;base64," ~ StringJoin ~ ExportString[
                im = Rasterize[
                    Block[{time, eval, $Messages, Print = myprint},
                        MemoryConstrained[
                            TimeConstrained[
                                Print["Inputs: ", ""[in]];
                                eval = EvaluationData[Catch[calcoptBLP[in]]];
                                Print["Total CPU time: ", eval["Timing"]];
                                If[eval["Success"],
                                    eval["Result"]
                                    ,
                                    Print["Something went really wrong."];
                                    Print["Please report your input and the following error(s) to Eaux Tacous#1021"];
                                    Print /@ eval["Messages"];
                                ],
                                10,
                                Print @ Style["Error: Timeout", Hue[{0, .8, .9}]]
                            ],
                            100*^6,
                            Print @ Style["Error: Out of memory", Hue[{0, .8, .9}]]
                        ]
                    ] // Reap // Last // Last // Column,
                    Background -> RGBColor["#222"], BaselinePosition -> Center, ImageFormattingWidth -> Infinity, ImageResolution -> 100
                ],
                {"Base64", "PNG"},
                "ColorMapLength" -> 24,
                CompressionLevel -> 1,
                Dithering -> None,
                IncludeMetaInformation -> {}
            ],
            {"\n" -> ""}
        ]
    ];


calcoptBLP[___] :=
    Throw[Print @ Style["Invalid Input", Hue[{0, .8, .9}]]];


calcoptBLP[llf_, initstars:Except[_Integer]?NumericQ, opt_.] :=
    calcoptBLP[llf, Round[initstars], opt]


calcoptBLP[llf_?(NumericQ @ # && 10 <= # <= 1*^6&), initstars_Integer?(0 <= # <= 1*^9&), Optional[Automatic, Automatic]] :=
    Block[{lpsi, level},
        lpsi = (llf / 25 - 1) / Log2[10];
        level = Clip[Quotient[lpsi, 20, 90], {0, 24}];
        Print["Automatic third argument used: ", level];
        calcoptBLP[llf, initstars, level]
    ];


calcoptBLP[llf_?(NumericQ @ # && 10 <= # <= 1*^6&), initstars_Integer?(0 <= # <= 1*^9&), ex_Integer /; 0 <= ex <= 24] :=
    calcoptBLP[llf, initstars, Prepend[Table[Clip[1 + 0.04 * (ex - 2 n), {1, 1.08}], {n, 0, 17}], 9]];


(*calcoptBLPComp =
    (* Workaround to get Compile to work properly *)
    With[{varTable = varTable, upTable = upTable, starTable = starTable, fToLevel = fToLevel, getExponent = getExponent, toNextExponent = toNextExponent, totalVarExp = totalVarExp, starCost = starCost, singleStarCost = singleStarCost, ge = Compile`GetElement},
        Compile[{{llf, _Real}, {initstars, _Real}, {ex, _Real, 1}},
            Block[{ex2, totex, exps, vars, ups, upsc, costbag, powerbag, levelbag, lengthbag, lengthlist, acc, length, costlist, powerlist, levellist, m, b, c, x, resx, powsinit, powsres, maxp, fincosts, finlevs, finpows, initpows, weakThreshold, strongFlags, stars, initpowtot, finpowtot},
                ex2 = FoldList[Times, 1, Rest @ ex];
                totex = Last @ ex2;
                exps = ex2 / totex;
                vars = fToLevel[llf, First @ #, Last @ #]& /@ varTable;
                ups = fToLevel[llf, First @ #, Last @ #] + 1& /@ upTable;
                upsc = FoldList[Times, 1, Rest[ups]^Most[exps]];
                initpows =
                    MapThread[
                        Function[{var, up, exp, star},
                            up totalVarExp[var, exp]
                        ],
                        {vars, upsc, exps, starTable}
                    ];
                (* Passthrough 1 *)
                maxp = 0.;
                costbag = Internal`Bag[Most @ {1}];
                powerbag = Internal`Bag[];
                levelbag = Internal`Bag[Most @ {1}];
                lengthbag = Internal`Bag[Most @ {1}];
                length = 0;
                MapThread[
                    Function[{var, up, exp, star, initpow},
                        Block[{prevCost = 0, prevLevel = 0, deltaLevel = 0, cost = 0, power = 0., level = 0},
                            While[cost < initstars,
                                deltaLevel = toNextExponent[var + prevLevel];
                                cost = prevCost + starCost[star, prevLevel, deltaLevel];
                                level = prevLevel + deltaLevel;
                                power = up totalVarExp[var + level, exp] - initpow;
                                If[cost <= initstars,
                                    Internal`StuffBag[costbag, cost];
                                    Internal`StuffBag[powerbag, power];
                                    Internal`StuffBag[levelbag, level];
                                    length++;
                                    prevLevel = level;
                                    prevCost = cost;
                                ];
                            ];
                            Internal`StuffBag[lengthbag, length];
                            0
                        ]
                    ],
                    {vars, upsc, exps, starTable, initpows}
                ];
                lengthlist = Internal`BagPart[lengthbag, All];
                costlist = Internal`BagPart[costbag, All];
                powerlist = Internal`BagPart[powerbag, All];
                maxp = Max[powerlist];
                levellist = Internal`BagPart[levelbag, All];
                c = -powerlist / Max[powerlist];
                m = Table[0, 20, length];
                acc = 1;
                Fold[(Do[m[[acc, i]] = -1, {i, # + 1, #2}];
                        acc++;
                        #2
                    )&, 0, lengthlist
                ];
                m[[20]] = -costlist;
                b = Table[If[i == 20,
                    -initstars
                    ,
                    -1
                ], {i, 20}];
                Print["LP size (pass 1): ", Dimensions @ m];
                x = Round @ externLP[c, m, b, Automatic, Integers, Tolerance -> 10^-9];
                fincosts = Table[0, 19];
                finlevs = Table[0, 19];
                finpows = Table[0., 19];
                acc = 1;
                Fold[(Do[
                            If[x[[i]] == 1,
                                fincosts[[acc]] = costlist[[i]];
                                finlevs[[acc]] = levellist[[i]];
                                finpows[[acc]] = powerlist[[i]];
                            ],
                            {i, # + 1, #2}
                        ];
                        acc++;
                        #2
                    )&, 0, lengthlist
                ];
                weakThreshold = 2 * Max @ MapThread[Function[{ip, pow, var, lev}, If[var + lev > 0,
                    (ip + pow) / (var + lev)
                    ,
                    0.
                ]], {initpows, finpows, vars, finlevs}];
                strongFlags = MapThread[Function[{ip, pow}, Boole[ip + pow > weakThreshold]], {initpows, finpows}];
                fincosts = MapThread[Function[{flag, cost}, flag cost], {strongFlags, fincosts}, 1];
                finlevs = MapThread[Function[{flag, lev}, flag lev], {strongFlags, finlevs}, 1];
                finpows = MapThread[Function[{flag, pow}, flag pow], {strongFlags, finpows}, 1];
                (* Passthrough 2 *)
                costbag = Internal`Bag[Most @ {1}];
                powerbag = Internal`Bag[];
                levelbag = Internal`Bag[Most @ {1}];
                lengthbag = Internal`Bag[Most @ {1}];
                length = 0;
                stars = initstars - Plus @@ fincosts;
                MapThread[
                    Function[{var, up, exp, star, initpow, lev, flag},
                        Block[{prevCost = 0, prevLevel = lev, deltaLevel = 0, cost = 0, power = 0., level = 0},
                            While[cost < stars,
                                If[flag == 0,
                                    deltaLevel = toNextExponent[var + prevLevel];
                                    cost = prevCost + starCost[star, prevLevel, deltaLevel];
                                    level = prevLevel + deltaLevel
                                    ,
                                    cost = prevCost + singleStarCost[star, prevLevel];
                                    level = prevLevel + 1
                                ];
                                power = up totalVarExp[var + level, exp] - initpow;
                                If[cost <= stars,
                                    Internal`StuffBag[costbag, cost];
                                    Internal`StuffBag[powerbag, power];
                                    Internal`StuffBag[levelbag, level];
                                    length++;
                                    prevLevel = level;
                                    prevCost = cost;
                                ];
                            ];
                            Internal`StuffBag[lengthbag, length];
                            0
                        ]
                    ],
                    {vars, upsc, exps, starTable, initpows + finpows, finlevs, strongFlags}
                ];
                lengthlist = Internal`BagPart[lengthbag, All];
                costlist = Internal`BagPart[costbag, All];
                powerlist = Internal`BagPart[powerbag, All];
                levellist = Internal`BagPart[levelbag, All];
                c = -powerlist / Max[powerlist];
                m = Table[0, 20, length];
                acc = 1;
                Fold[(Do[m[[acc, i]] = -1, {i, # + 1, #2}];
                        acc++;
                        #2
                    )&, 0, lengthlist
                ];
                m[[20]] = -costlist;
                b = Table[If[i == 20,
                    -stars
                    ,
                    -1
                ], {i, 20}];
                Print["LP size (pass 2): ", Dimensions @ m];
                If[Length[c] > 0,
                    x = Round @ externLP[c, m, b, Automatic, Integers, Tolerance -> 10^-9];
                    acc = 1;
                    Fold[(Do[
                                If[x[[i]] == 1,
                                    fincosts[[acc]] += costlist[[i]];
                                    finlevs[[acc]] += levellist[[i]];
                                    finpows[[acc]] += powerlist[[i]];
                                ],
                                {i, # + 1, #2}
                            ];
                            acc++;
                            #2
                        )&, 0, lengthlist
                    ];
                ];
                initpowtot = Plus @@ initpows;
                finpowtot = Plus @@ finpows + initpowtot;
                Print["dump y: ", (maxp + initpowtot)^totex];
                Print["found y: ", finpowtot^totex];
                {fincosts, vars, finlevs, initpows, finpows}
            ],
            RuntimeOptions -> {"RuntimeErrorHandler" -> Function[Print["Calculation failed."]]}
        ]
    ];*)


calcoptBLPComp = Uncompress@"1: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";


calcoptBLP[llf_?(NumericQ @ # && 10 <= # <= 1*^6&), initstars_Integer?(0 <= # <= 1*^9&), ex_List /; Length[ex] == 19] :=
    Block[{fincosts, vars, finlevs, initpows, finpows, totpow, pows, powsd, grid, len = 19},
        {fincosts, vars, finlevs, initpows, finpows} = calcoptBLPComp[llf, initstars, ex];
        fincosts = Round@fincosts;
        vars = Round@vars;
        finlevs = Round@finlevs;
        totpow = Total[initpows + finpows];
        pows = (initpows + finpows) / totpow;
        powsd = finpows / totpow;
        Print["stars used: ", Total @ fincosts];
        Print["stars left: ", initstars - Total @ fincosts];
        grid = {
            symbTable, NumberForm[#, Infinity, NumberSigns -> {"-", "+"}]& /@ finlevs, fincosts, vars, finlevs + vars, PercentForm[#, {Infinity, 4}]& /@ pows, Table[
                Which[
                    finlevs[[i]] == 0,
                        "none"
                    ,
                    powsd[[i]] >= 0.01,
                        "top"
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
        grid = Prepend[grid, {"variable", "\[CapitalDelta] level", "stars used", "level before", "level after", "power after", "priority"}];
        Print[Grid[
            grid, Frame -> All, FrameStyle -> Thin, ItemStyle -> {
                Automatic, Switch[Last @ #,
                    "top",
                        Hue[{0, .8, .9}]
                    ,
                    "high",
                        Hue[{55 / 360, .65, .8}]
                    ,
                    "medium",
                        Hue[{1 / 3, .6, .6}]
                    ,
                    "low",
                        Hue[{200 / 360, .1, .7}]
                    ,
                    "none",
                        RGBColor["#888"]
                    ,
                    _,
                        Automatic
                ]& /@ grid
            }
        ]];
    ]
