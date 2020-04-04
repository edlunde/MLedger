(* ::Package:: *)

AddSuite[MLedgerTests, commonTests];


(* ::Subsection:: *)
(*Test directory handling*)


AddSuite[commonTests, directoryHandlingTests];


AddTest[directoryHandlingTests, "Set Up", 
 testDirTemp = currentDir (* created in testScript.sh *) <> "testDirTemp123/";
 EnsureDirectoryExists[testDirTemp];
];

AddTest[directoryHandlingTests, "Tear Down", 
 If[Length@FileNames[testDirTemp] > 0, 
  DeleteDirectory[testDirTemp, DeleteContents->True]];
];

AddTest[directoryHandlingTests, "testEnsureDirectoryExists",
 With[{dir = testDirTemp <> "tempDir/"},
  AssertTrue[Not@FileExistsQ[dir]];
  EnsureDirectoryExists[dir];
  AssertTrue[FileExistsQ[dir]];
 ];
];
