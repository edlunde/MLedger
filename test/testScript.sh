#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript -script

currentDir = DirectoryName[$InputFileName];

Needs["MLedger`", ParentDirectory[currentDir] <> "/MLedger.m"];
Needs["eMUnit`", currentDir <> "eMUnit.m"];

testFilesDir = currentDir <> "testFiles/";

Get[currentDir <> "testBankAccounts.m"];
Print /@ RunTest[MLedgerTests][[1,2;;]];
