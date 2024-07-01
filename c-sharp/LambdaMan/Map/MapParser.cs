using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LambdaMan.Map
{
    internal class MapParser
    {
        string path;
        public MapParser(string path) {
            this.path = path;
        }

        public List<Tile> Read()
        {
            List<Tile> parsedTiles = new List<Tile>();
            string[] contentArray = File.ReadAllLines(path);
            //iterate all rows of map
            for(int rowIndex = 0; rowIndex < contentArray.Length; rowIndex++)
            {
                Tile leftNeighbour = null;
                for(int characterIndex = 0; characterIndex < contentArray[rowIndex].Length; characterIndex++)
                {
                    Tile t = new Tile(rowIndex, characterIndex, contentArray[rowIndex][characterIndex]);
                    //this needs to happen on every step after the first
                    if(leftNeighbour != null)
                    {
                        leftNeighbour.Right = t;
                        leftNeighbour.Connections.Add(t);
                    }
                    parsedTiles.Add(t);
                    if(leftNeighbour == null)
                    {
                        leftNeighbour = t;
                        continue;
                    }
                    t.Left = leftNeighbour;
                    t.Connections.Add(leftNeighbour);
                    leftNeighbour = t;
                }
            }
            return parsedTiles;
        }

        public List<Tile> ConnectRows(List<Tile> parsedTiles)
        {
            int upperBound = parsedTiles.Max(tile => tile.x);
            //y groups
            List<IGrouping<int, Tile>> groups = parsedTiles.GroupBy(tile => tile.x).ToList();
           
            for (int tileIndex = 0; upperBound > tileIndex; tileIndex++) {
                var lowerGroup = groups[tileIndex].ToList();
                if(tileIndex+1 > upperBound)
                {
                    break;
                }
                var upperGroup = groups[tileIndex+1].ToList();
                for(int xIndex = 0; xIndex < lowerGroup.Count(); xIndex++)
                {
                    lowerGroup[xIndex].Bottom = upperGroup[xIndex];
                    lowerGroup[xIndex].Connections.Add(upperGroup[xIndex]);
                    upperGroup[xIndex].Top = lowerGroup[xIndex];
                    upperGroup[xIndex].Connections.Add(lowerGroup[xIndex]);
                }
            }
            return parsedTiles;
        }
    }
}
