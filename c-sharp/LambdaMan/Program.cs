using LambdaMan.Gameloop;
using LambdaMan.Map;
using System.Reflection;



MapParser mapParser = new MapParser(@"G:\Code\LambdaMan\LambdaMan\Problems\lambdaman4.txt");
var parsedTiles = mapParser.Read();
parsedTiles = mapParser.ConnectRows(parsedTiles);


ProblemMap.Print(parsedTiles);

Mover mover = new Mover(parsedTiles);
mover.Loop();
ProblemMap.PrintPath(mover.GetPath());
Console.ReadKey();