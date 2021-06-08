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
6OTDxmcGl+tXHvBf0G2V/Q==",
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
