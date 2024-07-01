using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LambdaMan.Map
{
    internal class ProblemMap
    {
        public static void Print(List<Tile> parsedTiles)
        {
            var sorted = parsedTiles.OrderBy(x => x.x).ThenBy(x => x.y);
            var groupings = sorted.GroupBy(x => x.x);
            foreach ( var grouping in groupings)
            {
                PrintRow(grouping.ToList());
            }
        }

        private static void PrintRow(List<Tile> parsedTiles)
        {
            foreach (var tile in parsedTiles)
            {
                Console.Write(tile.assignedSign); 
                Console.Write($"({tile.x},{tile.y})");
            }
            Console.WriteLine();
        }

        public static void PrintPath(Stack<Tile> path)
        {
            Console.WriteLine("The Path was");
            string movements = "";
            Console.WriteLine("---------");
            Tile point = null;

            foreach (var tile in path.Reverse())
            {
                if (point == null)
                {
                    point = tile;
                }
                if (point.x < tile.x)
                {
                    movements = movements + "D";
                }
                else if (point.y < tile.y)
                {
                    movements = movements + "R";
                }
                else if (point.x > tile.x)
                {
                    movements = movements + "U";
                }
                else if (point.y > tile.y)
                {
                    movements = movements + "L";
                }
                point = tile;


                Console.Write($"({tile.x},{tile.y})");
            }

            Console.WriteLine();
            Console.WriteLine(movements);
            Console.WriteLine($"Total score: {movements.Length}");
            Console.WriteLine("---------");
        }

        public static void PrintPath(Stack<Point> path)
        {
            Console.WriteLine("The Path was");
            string movements = "";
            Console.WriteLine("---------");
            Point? point = null;
            
            foreach (var tile in path.Reverse())
            {   
                if(point == null)
                {
                    point = tile;
                }
                if(point.Value.X < tile.X)
                {
                    movements = movements + "D";
                }else if(point.Value.Y < tile.Y)
                {
                    movements = movements + "R";
                }else if(point.Value.X > tile.X)
                {
                    movements = movements + "U";
                }
                else if (point.Value.Y > tile.Y)
                {
                    movements = movements + "L";
                }
                point = tile;


                Console.Write($"({tile.X},{tile.Y})");
            }

            Console.WriteLine();
            Console.WriteLine(movements);
            Console.WriteLine("---------");
        }
    }
}
