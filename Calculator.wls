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
424TH52824T84HL9KgD+P+YPs/U=",
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
