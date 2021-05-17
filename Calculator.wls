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
"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",
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
