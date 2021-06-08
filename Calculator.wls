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


(* ::Section::Closed:: *)
(*Helper Functions*)


myprint = Sow @* (Style[#, RGBColor["#ccc"], Bold]&) @* Row @* List;


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


calcoptBLPComp = ImportString[
"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
92vIDK5WnwHg/wGhpMzp",
{"Base64", "WDX"}];


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
