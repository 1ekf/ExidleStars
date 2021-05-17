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
lygOyVu9Xm2vnrar8Ojs/QqSg+vlKwD8P3z70kg=",
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
