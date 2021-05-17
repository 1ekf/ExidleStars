#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title::Closed:: *)
(*Optimal Star Distribution Calculator*)


(* ::Subtitle:: *)
(*by Eaux Tacous#1021*)


(* ::Section::Closed:: *)
(*Constants*)


(* ::Text:: *)
(*Taken from exidle wiki.*)


symbTable = {"y", "z", "s", "u", "v", "w", "\[Alpha]", "\[Beta]", "\[Gamma]", "\[Delta]", "\[CurlyEpsilon]", "\[Zeta]", "\[Eta]", "\[Theta]", "\[Iota]", "\[Kappa]", "\[Lambda]", "\[Nu]", "\[Xi]"};


varTable = {
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
};


upTable = {
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
};


starTable = {
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
};


(* ::Section::Closed:: *)
(*Helper Functions*)


myprint = Sow @* (Style[#, RGBColor["#ccc"], Bold]&) @* Row @* List;


fToLevel =
    Function[{llf, a, b},
        Ramp @ Floor[((llf - Log[10, Log[10, 2]]) / Log[10, 2] - b) / a + 1]
    ];


getExponent =
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
    ];


toNextExponent =
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
    ];


totalVar = Function[l, Evaluate[l * 2.^getExponent[l]]];
totalVarExp = Function[{l, e}, Evaluate[l^e * 2.^(e getExponent[l])]];


singleStarCost = Function[{a, cur}, Floor[a cur Exp[a cur / 200] + 1]];
starCost = Function[{a, cur, d}, Sum[Floor[a (cur + i) Exp[a (cur + i) / 200] + 1], {i, 0, d - 1}]];


(* Prevent Mathematica from complaining *)
externLP = Flatten[LinearProgramming[##]]&;


(* ::Section::Closed:: *)
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


calcoptBLPComp =
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
    ];


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
