#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script

currentDir = DirectoryName[$InputFileName];

Needs["MLedger`", ParentDirectory[currentDir] <> "/MLedger.m"];
Needs["eMUnit`", currentDir <> "eMUnit.m"];

testFilesDir = currentDir <> "testFiles/";

Get[currentDir <> "dataForTests.m"]

filesWithTests = FileNames["test" ~~ __ ~~ ".m", {currentDir}]
Get /@ filesWithTests
Print /@ RunTest[MLedgerTests][[1,2;;]];
